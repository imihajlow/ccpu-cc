use lang_c::ast::{
    AlignOf, BinaryOperatorExpression, CompoundLiteral, ConditionalExpression, Designator,
    InitializerListItem, OffsetOfExpression, SizeOfTy, SizeOfVal, StaticAssert,
    UnaryOperatorExpression,
};
use lang_c::span::Span;
use lang_c::{
    ast::{CastExpression, Expression, Initializer},
    span::Node,
};

use crate::initializer::FieldInitializer;
use crate::name_scope::NameScope;
use crate::string::{self, parse_string_literal};
use crate::{
    ctype::{self, CType, QualifiedType},
    error::{CompileError, CompileWarning, ErrorCollector},
    initializer::{Constant, TypedConstant},
    machine,
    type_builder::TypeBuilder,
};
use crate::{offsetof, type_builder};

pub fn compute_constant_initializer(
    initializer: Node<Initializer>,
    target_type: &QualifiedType,
    allow_var: bool,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    match initializer.node {
        Initializer::Expression(expr) => {
            let v = compute_constant_expr(*expr, allow_var, scope, ec)?;
            let casted = cast(v, target_type, initializer.span, ec)?;
            Ok(TypedConstant::new(casted, target_type.clone()))
        }
        Initializer::List(il) => {
            process_initializer_list(target_type, il, allow_var, scope, initializer.span, ec)
        }
    }
}

pub fn compute_constant_expr(
    expr: Node<Expression>,
    allow_var: bool,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    let span = expr.span;
    let expr = expr.node;
    match expr {
        Expression::Identifier(id) => {
            if !allow_var {
                ec.record_error(CompileError::VariablesForbidden, id.span)?;
                unreachable!();
            }
            scope.get_static_const(&id.node.name, id.span, ec)
        }
        Expression::Constant(c) => {
            match c.node {
                lang_c::ast::Constant::Integer(i) => {
                    use lang_c::ast::IntegerBase;
                    let radix = match i.base {
                        IntegerBase::Decimal => 10,
                        IntegerBase::Octal => 8,
                        IntegerBase::Hexadecimal => 16,
                        IntegerBase::Binary => 2,
                    };
                    let num = u128::from_str_radix(&i.number, radix).unwrap(); // should be already checked by lang_c
                    Ok(TypedConstant::new_from_int_literal(
                        num,
                        i.suffix,
                        radix == 10,
                    ))
                }
                lang_c::ast::Constant::Float(_) => todo!(),
                lang_c::ast::Constant::Character(s) => match string::parse_char_literal_typed(&s) {
                    Ok(x) => Ok(x),
                    Err(e) => {
                        ec.record_error(CompileError::CharParseError(e), c.span)?;
                        unreachable!()
                    }
                },
            }
        }
        Expression::Cast(c) => process_cast_expression_node(*c, allow_var, scope, ec),
        Expression::UnaryOperator(node) => {
            process_unary_operator_expression_node(*node, allow_var, scope, ec)
        }
        Expression::BinaryOperator(node) => {
            process_binary_operator_expression_node(*node, allow_var, scope, ec)
        }
        Expression::Call(node) => {
            ec.record_error(CompileError::CallsForbidden, node.span)?;
            unreachable!()
        }
        Expression::Comma(v) => {
            let results: Result<Vec<_>, _> = v
                .into_iter()
                .map(|node| compute_constant_expr(node, allow_var, scope, ec))
                .collect();
            Ok(results?.pop().unwrap()) // existence is checked by lang_c
        }
        Expression::Conditional(c) => process_condition_expression_node(*c, allow_var, scope, ec),
        Expression::StringLiteral(_) => todo!(),
        Expression::Member(_) => todo!(),
        Expression::CompoundLiteral(c) => process_compound_literal_node(*c, allow_var, scope, ec),
        Expression::SizeOfTy(t) => process_size_of_ty_node(*t, allow_var, scope, ec),
        Expression::SizeOfVal(v) => process_size_of_val_node(*v, scope, ec),
        Expression::AlignOf(a) => process_align_of_node(*a, allow_var, scope, ec),
        Expression::OffsetOf(oo) => process_offset_of_expression_node(*oo, scope, ec),
        Expression::VaArg(_) => {
            ec.record_error(
                CompileError::Unimplemented("va_arg in constant expressions".to_string()),
                span,
            )?;
            unreachable!()
        }
        Expression::Statement(_) => {
            ec.record_error(
                CompileError::Unimplemented("statement expression".to_string()),
                span,
            )?;
            unreachable!()
        } // GNU extension
        Expression::GenericSelection(_) => {
            ec.record_error(
                CompileError::Unimplemented(
                    "generic selection in constant expressions".to_string(),
                ),
                span,
            )?;
            unreachable!()
        }
    }
}

pub fn check_static_assert(
    sa: Node<StaticAssert>,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    let expr_span = sa.node.expression.span;
    let val = compute_constant_expr(*sa.node.expression, true, scope, ec)?;
    let msg = match parse_string_literal(sa.node.message.node) {
        Ok((t, data)) => match t {
            CType::Int(1, _) => String::from_utf8(data).expect("Expected utf-8"),
            _ => todo!(),
        },
        Err(e) => {
            ec.record_error(CompileError::StringParseError(e), sa.node.message.span)?;
            unreachable!();
        }
    };
    if val.t.t.is_integer() {
        if val.is_zero() {
            ec.record_error(CompileError::StaticAssertionFailed(msg), sa.span)?;
        }
    } else {
        ec.record_error(CompileError::IntegerTypeRequired, expr_span)?;
    }
    Ok(())
}

fn process_size_of_ty_node(
    node: Node<SizeOfTy>,
    _allow_var: bool,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    let span = node.span;
    let (t, _attrs) = type_builder::build_type_from_ast_type_name(node.node.0, scope, ec)?;
    let size = t.t.sizeof(scope, span, ec)?;
    Ok(TypedConstant::new_integer(size.into(), ctype::SIZE_TYPE))
}

fn process_size_of_val_node(
    node: Node<SizeOfVal>,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    let span = node.span;
    let t = if let Expression::Identifier(id) = node.node.0.node {
        let rv = scope.get_rvalue(&id.node.name, id.span, ec)?;
        rv.t
    } else {
        ec.record_error(
            CompileError::Unimplemented("sizeof complex experssions in compile time".to_string()),
            node.span,
        )?;
        unreachable!();
    };
    let size = t.t.sizeof(scope, span, ec)?;
    Ok(TypedConstant::new_integer(size.into(), ctype::SIZE_TYPE))
}

fn process_align_of_node(
    node: Node<AlignOf>,
    _allow_var: bool,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    let span = node.span;
    let (t, _attrs) = type_builder::build_type_from_ast_type_name(*node.node.0, scope, ec)?;
    let size = t.t.alignof(scope, span, ec)?;
    Ok(TypedConstant::new_integer(size.into(), ctype::SIZE_TYPE))
}

fn process_condition_expression_node(
    node: Node<ConditionalExpression>,
    allow_var: bool,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    let cond_span = node.node.condition.span;
    let else_span = node.node.else_expression.span;
    let cond_val = compute_constant_expr(*node.node.condition, allow_var, scope, ec)?;
    let then_val = compute_constant_expr(*node.node.then_expression, allow_var, scope, ec)?;
    let else_val = compute_constant_expr(*node.node.else_expression, allow_var, scope, ec)?;
    if !cond_val.t.t.is_scalar_or_array() {
        ec.record_error(CompileError::ScalarTypeRequired, cond_span)?;
        unreachable!()
    }
    if then_val.t.t.is_arithmetic() && else_val.t.t.is_arithmetic() {
        let (then_val, else_val) = TypedConstant::usual_arithmetic_convert(then_val, else_val);
        if cond_val.is_zero() {
            Ok(else_val)
        } else {
            Ok(then_val)
        }
    } else if then_val.t.t.is_void() && else_val.t.t.is_void() {
        Ok(then_val)
    } else if then_val.t.is_compatible_to(&else_val.t, false) {
        if cond_val.is_zero() {
            Ok(else_val)
        } else {
            Ok(then_val)
        }
    } else {
        ec.record_error(
            CompileError::IncompatibleTypes(then_val.t, else_val.t),
            else_span,
        )?;
        unreachable!()
    }
}

fn process_binary_operator_expression_node(
    node: Node<BinaryOperatorExpression>,
    allow_var: bool,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    use lang_c::ast::BinaryOperator;
    let lhs_span = node.node.lhs.span;
    let rhs_span = node.node.rhs.span;
    let lhs = compute_constant_expr(*node.node.lhs, allow_var, scope, ec)?;
    let rhs = compute_constant_expr(*node.node.rhs, allow_var, scope, ec)?;
    let op = node.node.operator.node;
    let op_span = node.node.operator.span;
    match op {
        BinaryOperator::Plus => {
            if lhs.t.t.is_arithmetic() || rhs.t.t.is_arithmetic() {
                // make rhs be arithmetic
                let (lhs, rhs, sus_span) = if rhs.t.t.is_arithmetic() {
                    (lhs, rhs, lhs_span)
                } else {
                    (rhs, lhs, rhs_span)
                };
                if lhs.t.t.is_arithmetic() {
                    let (lhs, rhs) = TypedConstant::usual_arithmetic_convert(lhs, rhs);
                    if lhs.t.t.is_integer() {
                        let lhs_val = lhs.unwrap_integer();
                        let rhs_val = rhs.unwrap_integer();
                        Ok(TypedConstant::new_integer(lhs_val + rhs_val, lhs.t.t))
                    } else {
                        todo!()
                    }
                } else if lhs.t.t.is_pointer() {
                    todo!()
                } else {
                    ec.record_error(CompileError::ArithmeticTypeRequired, sus_span)?;
                    unreachable!();
                }
            } else {
                ec.record_error(CompileError::ArithmeticTypeRequired, lhs_span)?;
                unreachable!()
            }
        }
        BinaryOperator::Minus => {
            if lhs.t.t.is_arithmetic() && rhs.t.t.is_arithmetic() {
                let (lhs, rhs) = TypedConstant::usual_arithmetic_convert(lhs, rhs);
                if lhs.t.t.is_integer() {
                    let lhs_val = lhs.unwrap_integer();
                    let rhs_val = rhs.unwrap_integer();
                    Ok(TypedConstant::new_integer(lhs_val - rhs_val, lhs.t.t))
                } else {
                    todo!()
                }
            } else if lhs.t.t.is_pointer() && rhs.t.t.is_pointer() {
                todo!()
            } else if lhs.t.t.is_pointer() && rhs.t.t.is_integer() {
                todo!()
            } else {
                ec.record_error(
                    CompileError::BadTypesForOperator("-".to_string()),
                    node.span,
                )?;
                unreachable!()
            }
        }
        BinaryOperator::Multiply => {
            if lhs.t.t.is_arithmetic() && rhs.t.t.is_arithmetic() {
                let (lhs, rhs) = TypedConstant::usual_arithmetic_convert(lhs, rhs);
                if lhs.t.t.is_integer() {
                    let lhs_val = lhs.unwrap_integer();
                    let rhs_val = rhs.unwrap_integer();
                    Ok(TypedConstant::new_integer(lhs_val * rhs_val, lhs.t.t))
                } else {
                    todo!()
                }
            } else {
                let span = if !lhs.t.t.is_arithmetic() {
                    lhs_span
                } else {
                    rhs_span
                };
                ec.record_error(CompileError::ArithmeticTypeRequired, span)?;
                unreachable!()
            }
        }
        BinaryOperator::Divide => {
            if lhs.t.t.is_arithmetic() && rhs.t.t.is_arithmetic() {
                let (lhs, rhs) = TypedConstant::usual_arithmetic_convert(lhs, rhs);
                if lhs.t.t.is_integer() {
                    let lhs_val = lhs.unwrap_integer();
                    let rhs_val = rhs.unwrap_integer();
                    if rhs_val == 0 {
                        ec.record_error(CompileError::DivisionByZero, rhs_span)?;
                        unreachable!();
                    }
                    Ok(TypedConstant::new_integer(lhs_val / rhs_val, lhs.t.t))
                } else {
                    todo!()
                }
            } else {
                let span = if !lhs.t.t.is_arithmetic() {
                    lhs_span
                } else {
                    rhs_span
                };
                ec.record_error(CompileError::ArithmeticTypeRequired, span)?;
                unreachable!()
            }
        }
        BinaryOperator::Modulo => integer_op(
            |lhs, rhs, ec| {
                if rhs == 0 {
                    ec.record_error(CompileError::DivisionByZero, rhs_span)?;
                    unreachable!();
                }
                Ok(lhs % rhs)
            },
            lhs,
            rhs,
            lhs_span,
            rhs_span,
            ec,
        ),
        BinaryOperator::ShiftLeft => {
            if lhs.t.t.is_integer() && rhs.t.t.is_integer() {
                let lhs = lhs.promote();
                let rhs = rhs.promote();
                let lhs_val = lhs.unwrap_integer();
                let rhs_val = rhs.unwrap_integer();
                if rhs_val < 0 {
                    ec.record_warning(CompileWarning::ShiftByNegative, rhs_span)?;
                }
                Ok(TypedConstant::new_integer(lhs_val << rhs_val, lhs.t.t))
            } else {
                let span = if !lhs.t.t.is_integer() {
                    lhs_span
                } else {
                    rhs_span
                };
                ec.record_error(CompileError::IntegerTypeRequired, span)?;
                unreachable!()
            }
        }
        BinaryOperator::ShiftRight => {
            if lhs.t.t.is_integer() && rhs.t.t.is_integer() {
                let lhs = lhs.promote();
                let rhs = rhs.promote();
                let lhs_val = lhs.unwrap_integer();
                let rhs_val = rhs.unwrap_integer();
                if rhs_val < 0 {
                    ec.record_warning(CompileWarning::ShiftByNegative, rhs_span)?;
                }
                Ok(TypedConstant::new_integer(lhs_val >> rhs_val, lhs.t.t))
            } else {
                let span = if !lhs.t.t.is_integer() {
                    lhs_span
                } else {
                    rhs_span
                };
                ec.record_error(CompileError::IntegerTypeRequired, span)?;
                unreachable!()
            }
        }
        BinaryOperator::Less => {
            let r = compare(lhs, rhs, false, node.span, ec)?;
            Ok(TypedConstant::new_integer(
                if r < 0 { 1 } else { 0 },
                ctype::INT_TYPE,
            ))
        }
        BinaryOperator::Greater => {
            let r = compare(lhs, rhs, false, node.span, ec)?;
            Ok(TypedConstant::new_integer(
                if r > 0 { 1 } else { 0 },
                ctype::INT_TYPE,
            ))
        }
        BinaryOperator::LessOrEqual => {
            let r = compare(lhs, rhs, false, node.span, ec)?;
            Ok(TypedConstant::new_integer(
                if r <= 0 { 1 } else { 0 },
                ctype::INT_TYPE,
            ))
        }
        BinaryOperator::GreaterOrEqual => {
            let r = compare(lhs, rhs, false, node.span, ec)?;
            Ok(TypedConstant::new_integer(
                if r <= 0 { 1 } else { 0 },
                ctype::INT_TYPE,
            ))
        }
        BinaryOperator::Equals => {
            let r = compare(lhs, rhs, true, node.span, ec)?;
            Ok(TypedConstant::new_integer(
                if r == 0 { 1 } else { 0 },
                ctype::INT_TYPE,
            ))
        }
        BinaryOperator::NotEquals => {
            let r = compare(lhs, rhs, true, node.span, ec)?;
            Ok(TypedConstant::new_integer(
                if r != 0 { 1 } else { 0 },
                ctype::INT_TYPE,
            ))
        }
        BinaryOperator::BitwiseAnd => integer_op(
            |lhs, rhs, _ec| Ok(lhs & rhs),
            lhs,
            rhs,
            lhs_span,
            rhs_span,
            ec,
        ),
        BinaryOperator::BitwiseXor => integer_op(
            |lhs, rhs, _ec| Ok(lhs ^ rhs),
            lhs,
            rhs,
            lhs_span,
            rhs_span,
            ec,
        ),
        BinaryOperator::BitwiseOr => integer_op(
            |lhs, rhs, _ec| Ok(lhs | rhs),
            lhs,
            rhs,
            lhs_span,
            rhs_span,
            ec,
        ),
        BinaryOperator::LogicalAnd => {
            logical_op(|lhs, rhs| lhs && rhs, lhs, rhs, lhs_span, rhs_span, ec)
        }
        BinaryOperator::LogicalOr => {
            logical_op(|lhs, rhs| lhs || rhs, lhs, rhs, lhs_span, rhs_span, ec)
        }
        BinaryOperator::Index => todo!(),
        BinaryOperator::Assign
        | BinaryOperator::AssignMultiply
        | BinaryOperator::AssignDivide
        | BinaryOperator::AssignModulo
        | BinaryOperator::AssignPlus
        | BinaryOperator::AssignMinus
        | BinaryOperator::AssignShiftLeft
        | BinaryOperator::AssignShiftRight
        | BinaryOperator::AssignBitwiseAnd
        | BinaryOperator::AssignBitwiseXor
        | BinaryOperator::AssignBitwiseOr => {
            ec.record_error(CompileError::AssignmentToConst, op_span)?;
            unreachable!()
        }
    }
}

/**
 * Common method for relation operators.
 *
 * Returns -1, 0, 1
 */
fn compare(
    lhs: TypedConstant,
    rhs: TypedConstant,
    _equality: bool,
    span: Span,
    ec: &mut ErrorCollector,
) -> Result<isize, ()> {
    if lhs.t.t.is_arithmetic() && rhs.t.t.is_arithmetic() {
        let (lhs, rhs) = TypedConstant::usual_arithmetic_convert(lhs, rhs);
        if lhs.t.t.is_integer() {
            let lhs = lhs.unwrap_integer();
            let rhs = rhs.unwrap_integer();
            Ok(if lhs < rhs {
                -1
            } else if lhs == rhs {
                0
            } else {
                1
            })
        } else {
            todo!()
        }
    } else if lhs.t.t.is_pointer() && rhs.t.t.is_pointer() {
        todo!()
    } else {
        ec.record_error(CompileError::CannotCompare(lhs.t, rhs.t), span)?;
        unreachable!()
    }
}

/**
 * Common method for integer arithmetic operations.
 */
fn integer_op<F>(
    f: F,
    lhs: TypedConstant,
    rhs: TypedConstant,
    lhs_span: Span,
    rhs_span: Span,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()>
where
    F: FnOnce(i128, i128, &mut ErrorCollector) -> Result<i128, ()>,
{
    if lhs.t.t.is_integer() && rhs.t.t.is_integer() {
        let (lhs, rhs) = TypedConstant::usual_arithmetic_convert(lhs, rhs);
        let lhs_val = lhs.unwrap_integer();
        let rhs_val = rhs.unwrap_integer();
        let r = f(lhs_val, rhs_val, ec)?;
        Ok(TypedConstant::new_integer(r, lhs.t.t))
    } else {
        let span = if !lhs.t.t.is_integer() {
            lhs_span
        } else {
            rhs_span
        };
        ec.record_error(CompileError::IntegerTypeRequired, span)?;
        unreachable!()
    }
}

/**
 * Common method for logical operations over two scalar types.
 */
fn logical_op<F>(
    f: F,
    lhs: TypedConstant,
    rhs: TypedConstant,
    lhs_span: Span,
    rhs_span: Span,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()>
where
    F: FnOnce(bool, bool) -> bool,
{
    if lhs.t.t.is_scalar_or_array() && rhs.t.t.is_scalar_or_array() {
        let (lhs, rhs) = TypedConstant::usual_arithmetic_convert(lhs, rhs);
        let lhs_zero = lhs.is_zero();
        let rhs_zero = rhs.is_zero();
        let r = f(!lhs_zero, !rhs_zero);
        Ok(TypedConstant::new_integer(if r { 1 } else { 0 }, lhs.t.t))
    } else {
        let span = if !lhs.t.t.is_scalar_or_array() {
            lhs_span
        } else {
            rhs_span
        };
        ec.record_error(CompileError::ScalarTypeRequired, span)?;
        unreachable!()
    }
}

fn process_unary_operator_expression_node(
    node: Node<UnaryOperatorExpression>,
    allow_var: bool,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    use lang_c::ast::UnaryOperator;
    let op_node = node.node.operator;
    let op = op_node.node;
    let span = op_node.span;
    let val = compute_constant_expr(*node.node.operand, allow_var, scope, ec)?;
    match op {
        UnaryOperator::PostIncrement
        | UnaryOperator::PostDecrement
        | UnaryOperator::PreIncrement
        | UnaryOperator::PreDecrement => {
            ec.record_error(CompileError::AssignmentToConst, span)?;
            unreachable!()
        }
        UnaryOperator::Address => {
            ec.record_error(
                CompileError::Unimplemented("address in constant expressions".to_string()),
                span,
            )?;
            unreachable!()
        }
        UnaryOperator::Indirection => {
            ec.record_error(
                CompileError::Unimplemented("indirection in constant expressions".to_string()),
                span,
            )?;
            unreachable!()
        }
        UnaryOperator::Plus => {
            if !val.t.t.is_arithmetic() {
                ec.record_error(CompileError::ArithmeticTypeRequired, span)?;
                unreachable!()
            }
            Ok(val.promote())
        }
        UnaryOperator::Minus => {
            if !val.t.t.is_arithmetic() {
                ec.record_error(CompileError::ArithmeticTypeRequired, span)?;
                unreachable!();
            }
            Ok(val.promote().negate())
        }
        UnaryOperator::Complement => {
            if !val.t.t.is_integer() {
                ec.record_error(CompileError::IntegerTypeRequired, span)?;
                unreachable!();
            }
            Ok(val.promote().complement())
        }
        UnaryOperator::Negate => {
            if !val.t.t.is_scalar_or_array() {
                ec.record_error(CompileError::ScalarTypeRequired, span)?;
                unreachable!();
            }
            Ok(val.promote().boolean_not())
        }
    }
}

fn process_cast_expression_node(
    c: Node<CastExpression>,
    allow_var: bool,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    let type_builder =
        TypeBuilder::new_from_specifiers_qualifiers(c.node.type_name.node.specifiers, scope, ec)?;
    let type_builder = type_builder.stage2(c.span, ec)?;
    let new_type = if let Some(decl) = c.node.type_name.node.declarator {
        type_builder.process_declarator_node(decl, scope, ec)?.1
    } else {
        type_builder.finalize().0
    };
    let value = compute_constant_expr(*c.node.expression, allow_var, scope, ec)?;

    Ok(TypedConstant {
        val: cast(value, &new_type, c.span, ec)?,
        t: new_type,
    })
}

fn process_offset_of_expression_node(
    oo: Node<OffsetOfExpression>,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    let offset = offsetof::compute_offsetof(oo, scope, ec)?;

    Ok(TypedConstant::new_integer(offset.into(), ctype::SIZE_TYPE))
}

fn process_compound_literal_node(
    c: Node<CompoundLiteral>,
    allow_var: bool,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    let type_builder =
        TypeBuilder::new_from_specifiers_qualifiers(c.node.type_name.node.specifiers, scope, ec)?;
    let type_builder = type_builder.stage2(c.node.type_name.span, ec)?;
    let target_type = if let Some(decl) = c.node.type_name.node.declarator {
        type_builder.process_declarator_node(decl, scope, ec)?.1
    } else {
        type_builder.finalize().0
    };
    process_initializer_list(
        &target_type,
        c.node.initializer_list,
        allow_var,
        scope,
        c.span,
        ec,
    )
}

fn process_initializer_list(
    target_type: &QualifiedType,
    il: Vec<Node<InitializerListItem>>,
    allow_var: bool,
    scope: &mut NameScope,
    span: Span,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    if target_type.t.is_array() {
        process_array_initializer_list(&target_type, il, allow_var, scope, span, ec)
    } else if target_type.t.is_object() {
        process_object_initializer_list(&target_type, il, allow_var, scope, span, ec)
    } else {
        todo!()
    }
}

fn process_object_initializer_list(
    target_type: &QualifiedType,
    il: Vec<Node<InitializerListItem>>,
    allow_var: bool,
    scope: &mut NameScope,
    span: Span,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    let id = if let CType::StructUnion(id) = &target_type.t {
        id
    } else {
        panic!("must be an object");
    };
    let su = scope.get_struct_union(id).clone();

    if !su.is_complete() {
        ec.record_error(CompileError::IncompleteStruct(target_type.clone()), span)?;
        unreachable!();
    };

    let mut maybe_zero = true;

    let mut initializers = Vec::new();

    let mut field_index = 0;
    for item in il {
        if field_index != 0 {
            maybe_zero = false;
        }
        if item.node.designation.len() == 0 {
            if let Some((field_name, field_type)) = su.get_field_by_index(field_index, scope) {
                if let Some(field_name) = field_name {
                    let value = compute_constant_initializer(
                        *item.node.initializer,
                        &field_type,
                        allow_var,
                        scope,
                        ec,
                    )?;
                    if !value.is_zero() {
                        maybe_zero = false;
                    }
                    let (offset, _) = su.get_field(&field_name, scope, item.span, ec)?.unwrap();
                    initializers.push(FieldInitializer {
                        offset,
                        value,
                        span: item.span,
                    });
                    field_index += 1;
                } else {
                    ec.record_error(
                        CompileError::Unimplemented(
                            "initializing unnamed struct fields".to_string(),
                        ),
                        item.span,
                    )?;
                    unreachable!();
                }
            } else {
                ec.record_error(CompileError::ExcessElementsInInitializer, item.span)?;
                unreachable!()
            }
        } else if item.node.designation.len() > 1 {
            ec.record_error(
                CompileError::Unimplemented("complex designators".to_string()),
                item.span,
            )?;
            unreachable!();
        } else {
            maybe_zero = false;
            let designation = item.node.designation.last().unwrap();
            match &designation.node {
                Designator::Member(id) => {
                    let field_name = &id.node.name;
                    let (offset, field_type) =
                        target_type.get_field(field_name, scope, item.span, ec)?;
                    let this_field_index = su.get_field_index(field_name, scope).unwrap();
                    let value = compute_constant_initializer(
                        *item.node.initializer,
                        &field_type,
                        allow_var,
                        scope,
                        ec,
                    )?;
                    initializers.push(FieldInitializer {
                        offset,
                        value,
                        span: item.span,
                    });
                    field_index = this_field_index + 1;
                }
                Designator::Index(_) => {
                    ec.record_error(
                        CompileError::ArrayDesignatorForStruct(target_type.clone()),
                        designation.span,
                    )?;
                    unreachable!()
                }
                Designator::Range(_) => {
                    ec.record_error(
                        CompileError::Unimplemented("range designator".to_string()),
                        designation.span,
                    )?;
                    unreachable!()
                }
            }
        }
    }

    if maybe_zero {
        Ok(TypedConstant {
            t: target_type.clone(),
            val: Constant::Zero,
        })
    } else {
        Ok(TypedConstant {
            t: target_type.clone(),
            val: Constant::Struct(initializers),
        })
    }
}

fn process_array_initializer_list(
    target_type: &QualifiedType,
    il: Vec<Node<InitializerListItem>>,
    allow_var: bool,
    scope: &mut NameScope,
    span: Span,
    ec: &mut ErrorCollector,
) -> Result<TypedConstant, ()> {
    assert!(target_type.t.is_array());
    let element_type = target_type.t.clone().dereference().unwrap();
    if !element_type.t.is_scalar() {
        ec.record_error(
            CompileError::Unimplemented(
                "arrays of non-scalar types in initializer lists".to_string(),
            ),
            span,
        )?;
        unreachable!()
    }
    let element_count = target_type.t.get_element_count().map(|c| c as usize);
    let mut elements = Vec::new();
    if let Some(c) = element_count {
        elements.resize(c as usize, None);
    }
    let mut index: usize = 0;
    for initializer in il {
        match initializer.node.designation.len() {
            0 => (),
            1 => {
                let designation = &initializer.node.designation[0];
                let index_const = match &designation.node {
                    Designator::Index(index_expr) => {
                        compute_constant_expr(index_expr.clone(), allow_var, scope, ec)?
                    }
                    Designator::Member(_) => {
                        ec.record_error(
                            CompileError::WrongInitializerForType(target_type.clone()),
                            designation.span,
                        )?;
                        unreachable!()
                    }
                    Designator::Range(_) => {
                        ec.record_error(
                            CompileError::Unimplemented("range designator".to_string()),
                            designation.span,
                        )?;
                        unreachable!()
                    }
                };
                if !index_const.t.t.is_integer() {
                    ec.record_error(CompileError::IntegerTypeRequired, designation.span)?;
                    unreachable!()
                }
                index = if let Ok(i) = index_const.unwrap_integer().try_into() {
                    i
                } else {
                    ec.record_error(CompileError::ConstantOutOfRange, designation.span)?;
                    unreachable!()
                };
            }
            _ => {
                ec.record_error(
                    CompileError::WrongInitializerForType(target_type.clone()),
                    initializer.span,
                )?;
                unreachable!()
            }
        }
        match element_count {
            Some(count) if count <= index => {
                ec.record_error(
                    CompileError::ArrayDesignatorIndexOutOfBounds(count, index),
                    initializer.span,
                )?;
                unreachable!()
            }
            _ => (),
        }
        if elements.len() <= index {
            elements.resize(index + 1, None);
        }
        if elements[index].is_some() {
            ec.record_warning(
                CompileWarning::PriorInitializationOverridden,
                initializer.span,
            )?;
        }

        let value = match initializer.node.initializer.node {
            Initializer::Expression(e) => compute_constant_expr(*e, allow_var, scope, ec)?,
            Initializer::List(_) => {
                ec.record_error(
                    CompileError::WrongInitializerForType(target_type.clone()),
                    initializer.node.initializer.span,
                )?;
                unreachable!()
            }
        };
        let casted = cast(value, &element_type, initializer.node.initializer.span, ec)?;

        elements[index] = Some(casted);

        index += 1;
    }

    let elements: Vec<_> = elements
        .into_iter()
        .map(|el| el.unwrap_or(Constant::Zero))
        .collect();
    let final_type = QualifiedType {
        qualifiers: target_type.qualifiers,
        t: CType::Array(Box::new(element_type), Some(elements.len() as u32)),
    };
    Ok(TypedConstant {
        t: final_type.clone(),
        val: Constant::Array(final_type.clone().dereference().unwrap(), elements),
    })
}

fn cast(
    constant: TypedConstant,
    new_type: &QualifiedType,
    span: Span,
    ec: &mut ErrorCollector,
) -> Result<Constant, ()> {
    if !constant.t.is_explicit_castable_to(&new_type) {
        ec.record_error(
            CompileError::BadCast(format!("{}", constant.t), format!("{}", new_type)),
            span,
        )?;
    }
    let new_v = match new_type.t {
        CType::Void => Constant::Void,
        CType::Int(new_size, new_sign) => match constant.t.t {
            CType::Int(old_size, old_sign) => {
                cast_int(old_size, old_sign, new_size, new_sign, constant.val)
            }
            CType::Bool => cast_from_bool(constant.val),
            CType::Pointer(_) => {
                cast_int(machine::PTR_SIZE, false, new_size, new_sign, constant.val)
            }
            CType::Float(_) => todo!(),
            CType::Array(_, _) => {
                ec.record_error(CompileError::NonConstInConstExpr, span)?;
                unreachable!()
            }
            CType::Enum(_) => cast_int(machine::INT_SIZE, true, new_size, new_sign, constant.val),
            _ => unreachable!(),
        },
        CType::Bool => match constant.t.t {
            CType::Int(_, _) | CType::Pointer(_) => cast_to_bool(constant.val),
            CType::Bool => constant.val,
            CType::Float(_) => todo!(),
            CType::Array(_, _) => Constant::Int(1),
            _ => unreachable!(),
        },
        CType::Float(_) => todo!(),
        CType::Pointer(_) => match constant.t.t {
            CType::Int(old_size, old_sign) => {
                cast_int(old_size, old_sign, machine::PTR_SIZE, false, constant.val)
            }
            CType::Bool => cast_from_bool(constant.val),
            CType::Pointer(_) => constant.val,
            CType::Float(_) => todo!(),
            CType::Array(_, _) => {
                ec.record_error(CompileError::NonConstInConstExpr, span)?;
                unreachable!()
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };
    Ok(new_v)
}

fn cast_int_ff<F>(size_to: u8, sign_to: bool, value: F) -> i128
where
    F: num_traits::AsPrimitive<u8>,
    F: num_traits::AsPrimitive<i8>,
    F: num_traits::AsPrimitive<u16>,
    F: num_traits::AsPrimitive<i16>,
    F: num_traits::AsPrimitive<u32>,
    F: num_traits::AsPrimitive<i32>,
    F: num_traits::AsPrimitive<u64>,
    F: num_traits::AsPrimitive<i64>,
{
    use num_traits::AsPrimitive;
    match (size_to, sign_to) {
        (1, true) => AsPrimitive::<i8>::as_(value) as i128,
        (1, false) => AsPrimitive::<u8>::as_(value) as i128,
        (2, true) => AsPrimitive::<i16>::as_(value) as i128,
        (2, false) => AsPrimitive::<u16>::as_(value) as i128,
        (4, true) => AsPrimitive::<i32>::as_(value) as i128,
        (4, false) => AsPrimitive::<u32>::as_(value) as i128,
        (8, true) => AsPrimitive::<i64>::as_(value) as i128,
        (8, false) => AsPrimitive::<u64>::as_(value) as i128,
        _ => unreachable!(),
    }
}

fn cast_int(
    size_from: u8,
    sign_from: bool,
    size_to: u8,
    sign_to: bool,
    value: Constant,
) -> Constant {
    if let Constant::Int(value) = value {
        let new_val = match (size_from, sign_from) {
            (1, true) => cast_int_ff(size_to, sign_to, value as i8),
            (1, false) => cast_int_ff(size_to, sign_to, value as u8),
            (2, true) => cast_int_ff(size_to, sign_to, value as i16),
            (2, false) => cast_int_ff(size_to, sign_to, value as u16),
            (4, true) => cast_int_ff(size_to, sign_to, value as i32),
            (4, false) => cast_int_ff(size_to, sign_to, value as u32),
            (8, true) => cast_int_ff(size_to, sign_to, value as i64),
            (8, false) => cast_int_ff(size_to, sign_to, value as u64),
            _ => unreachable!(),
        };
        Constant::Int(new_val)
    } else {
        panic!("value doesn't match type")
    }
}

fn cast_from_bool(value: Constant) -> Constant {
    if let Constant::Int(value) = value {
        let new_val = if value == 0 { 0 } else { 1 };
        Constant::Int(new_val)
    } else {
        panic!("value doesn't match type")
    }
}

fn cast_to_bool(value: Constant) -> Constant {
    match value {
        Constant::Zero => Constant::Zero,
        Constant::Int(v) => Constant::Int(if v == 0 { 0 } else { 1 }),
        _ => panic!("value doesn't match type"),
    }
}
