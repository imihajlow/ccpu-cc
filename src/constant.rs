use lang_c::ast::{BinaryOperatorExpression, UnaryOperatorExpression};
use lang_c::span::Span;
use lang_c::{
    ast::{CastExpression, Constant, Expression, Initializer},
    span::Node,
};

use crate::{
    ctype::{self, CType, QualifiedType},
    error::{CompileError, CompileWarning, ErrorCollector},
    initializer::{TypedValue, Value},
    machine,
    translation_unit::TranslationUnit,
    type_builder::{self, TypeBuilder},
    type_registry::TypeRegistry,
};

pub fn compute_constant_initializer(
    initializer: Node<Initializer>,
    target_type: &QualifiedType,
    allow_var: bool,
    tu: &TranslationUnit,
    ec: &mut ErrorCollector,
) -> Result<Value, ()> {
    match initializer.node {
        Initializer::Expression(expr) => {
            let v = compute_constant_expr(*expr, allow_var, tu, ec)?;
            cast(v, target_type, initializer.span, ec)
        }
        Initializer::List(_) => todo!(),
    }
}

pub fn compute_constant_expr(
    expr: Node<Expression>,
    allow_var: bool,
    tu: &TranslationUnit,
    ec: &mut ErrorCollector,
) -> Result<TypedValue, ()> {
    let span = expr.span;
    let expr = expr.node;
    match expr {
        Expression::Identifier(id) => {
            if !allow_var {
                ec.record_error(CompileError::VariablesForbidden, id.span)?;
                unreachable!();
            }
            match tu.lookup_global_declaration(&id.node.name) {
                None => {
                    ec.record_error(CompileError::UnknownIdentifier(id.node.name), id.span)?;
                    unreachable!();
                }
                Some(decl) => {
                    if !decl.t.is_const() || decl.initializer.is_none() {
                        ec.record_error(CompileError::NonConstInConstExpr, id.span)?;
                        unreachable!();
                    }
                    Ok(TypedValue {
                        t: decl.t.clone(),
                        val: decl.initializer.as_ref().unwrap().clone(),
                    })
                }
            }
        }
        Expression::Constant(c) => {
            match c.node {
                Constant::Integer(i) => {
                    use lang_c::ast::IntegerBase;
                    let radix = match i.base {
                        IntegerBase::Decimal => 10,
                        IntegerBase::Octal => 8,
                        IntegerBase::Hexadecimal => 16,
                        IntegerBase::Binary => 2,
                    };
                    let num = u128::from_str_radix(&i.number, radix).unwrap(); // should be already checked by lang_c
                    Ok(TypedValue::new_from_int_literal(num, i.suffix, radix == 10))
                }
                Constant::Float(_) => todo!(),
                Constant::Character(_) => todo!(),
            }
        }
        Expression::Cast(c) => process_cast_expression_node(*c, allow_var, tu, ec),
        Expression::UnaryOperator(node) => {
            process_unary_operator_expression_node(*node, allow_var, tu, ec)
        }
        Expression::BinaryOperator(node) => {
            process_binary_operator_expression_node(*node, allow_var, tu, ec)
        }
        Expression::StringLiteral(_) => todo!(),
        Expression::Member(_) => todo!(),
        Expression::Call(_) => todo!(),
        Expression::CompoundLiteral(_) => todo!(),
        Expression::SizeOfTy(_) => todo!(),
        Expression::SizeOfVal(_) => todo!(),
        Expression::AlignOf(_) => todo!(),
        Expression::Conditional(_) => todo!(),
        Expression::Comma(_) => todo!(),
        Expression::OffsetOf(_) => todo!(),
        Expression::VaArg(_) => todo!(),
        Expression::Statement(_) => unimplemented!(), // GNU extension
        Expression::GenericSelection(_) => unimplemented!(),
    }
}

fn process_binary_operator_expression_node(
    node: Node<BinaryOperatorExpression>,
    allow_var: bool,
    tu: &TranslationUnit,
    ec: &mut ErrorCollector,
) -> Result<TypedValue, ()> {
    use lang_c::ast::BinaryOperator;
    let lhs_span = node.node.lhs.span;
    let rhs_span = node.node.rhs.span;
    let lhs = compute_constant_expr(*node.node.lhs, allow_var, tu, ec)?;
    let rhs = compute_constant_expr(*node.node.rhs, allow_var, tu, ec)?;
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
                    let (lhs, rhs) = TypedValue::usual_arithmetic_convert(lhs, rhs);
                    if lhs.t.t.is_integer() {
                        let lhs_val = lhs.unwrap_integer();
                        let rhs_val = rhs.unwrap_integer();
                        Ok(TypedValue::new_integer(lhs_val + rhs_val, lhs.t.t))
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
                let (lhs, rhs) = TypedValue::usual_arithmetic_convert(lhs, rhs);
                if lhs.t.t.is_integer() {
                    let lhs_val = lhs.unwrap_integer();
                    let rhs_val = rhs.unwrap_integer();
                    Ok(TypedValue::new_integer(lhs_val - rhs_val, lhs.t.t))
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
                let (lhs, rhs) = TypedValue::usual_arithmetic_convert(lhs, rhs);
                if lhs.t.t.is_integer() {
                    let lhs_val = lhs.unwrap_integer();
                    let rhs_val = rhs.unwrap_integer();
                    Ok(TypedValue::new_integer(lhs_val * rhs_val, lhs.t.t))
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
                let (lhs, rhs) = TypedValue::usual_arithmetic_convert(lhs, rhs);
                if lhs.t.t.is_integer() {
                    let lhs_val = lhs.unwrap_integer();
                    let rhs_val = rhs.unwrap_integer();
                    if rhs_val == 0 {
                        ec.record_error(CompileError::DivisionByZero, rhs_span)?;
                        unreachable!();
                    }
                    Ok(TypedValue::new_integer(lhs_val / rhs_val, lhs.t.t))
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
                Ok(TypedValue::new_integer(lhs_val << rhs_val, lhs.t.t))
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
                Ok(TypedValue::new_integer(lhs_val >> rhs_val, lhs.t.t))
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
            Ok(TypedValue::new_integer(
                if r < 0 { 1 } else { 0 },
                ctype::INT_TYPE,
            ))
        }
        BinaryOperator::Greater => {
            let r = compare(lhs, rhs, false, node.span, ec)?;
            Ok(TypedValue::new_integer(
                if r > 0 { 1 } else { 0 },
                ctype::INT_TYPE,
            ))
        }
        BinaryOperator::LessOrEqual => {
            let r = compare(lhs, rhs, false, node.span, ec)?;
            Ok(TypedValue::new_integer(
                if r <= 0 { 1 } else { 0 },
                ctype::INT_TYPE,
            ))
        }
        BinaryOperator::GreaterOrEqual => {
            let r = compare(lhs, rhs, false, node.span, ec)?;
            Ok(TypedValue::new_integer(
                if r <= 0 { 1 } else { 0 },
                ctype::INT_TYPE,
            ))
        }
        BinaryOperator::Equals => {
            let r = compare(lhs, rhs, true, node.span, ec)?;
            Ok(TypedValue::new_integer(
                if r == 0 { 1 } else { 0 },
                ctype::INT_TYPE,
            ))
        }
        BinaryOperator::NotEquals => {
            let r = compare(lhs, rhs, true, node.span, ec)?;
            Ok(TypedValue::new_integer(
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
        BinaryOperator::LogicalAnd => todo!(),
        BinaryOperator::LogicalOr => todo!(),
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
    lhs: TypedValue,
    rhs: TypedValue,
    equality: bool,
    span: Span,
    ec: &mut ErrorCollector,
) -> Result<isize, ()> {
    if lhs.t.t.is_arithmetic() && rhs.t.t.is_arithmetic() {
        let (lhs, rhs) = TypedValue::usual_arithmetic_convert(lhs, rhs);
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
    lhs: TypedValue,
    rhs: TypedValue,
    lhs_span: Span,
    rhs_span: Span,
    ec: &mut ErrorCollector,
) -> Result<TypedValue, ()>
where
    F: FnOnce(i128, i128, &mut ErrorCollector) -> Result<i128, ()>,
{
    if lhs.t.t.is_integer() && rhs.t.t.is_integer() {
        let (lhs, rhs) = TypedValue::usual_arithmetic_convert(lhs, rhs);
        let lhs_val = lhs.unwrap_integer();
        let rhs_val = rhs.unwrap_integer();
        let r = f(lhs_val, rhs_val, ec)?;
        Ok(TypedValue::new_integer(r, lhs.t.t))
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

fn process_unary_operator_expression_node(
    node: Node<UnaryOperatorExpression>,
    allow_var: bool,
    tu: &TranslationUnit,
    ec: &mut ErrorCollector,
) -> Result<TypedValue, ()> {
    use lang_c::ast::UnaryOperator;
    let op_node = node.node.operator;
    let op = op_node.node;
    let span = op_node.span;
    let val = compute_constant_expr(*node.node.operand, allow_var, tu, ec)?;
    match op {
        UnaryOperator::PostIncrement
        | UnaryOperator::PostDecrement
        | UnaryOperator::PreIncrement
        | UnaryOperator::PreDecrement => {
            ec.record_error(CompileError::AssignmentToConst, span)?;
            unreachable!()
        }
        UnaryOperator::Address => unimplemented!(),
        UnaryOperator::Indirection => unimplemented!(),
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
            if !val.t.t.is_scalar() {
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
    tu: &TranslationUnit,
    ec: &mut ErrorCollector,
) -> Result<TypedValue, ()> {
    let mut type_builder = TypeBuilder::new();
    for sq in c.node.type_name.node.specifiers {
        type_builder.add_specifier_qualifier_node(sq, &tu.type_registry, ec)?;
    }
    let type_builder = type_builder.stage2(c.span, ec)?;
    let new_type = if let Some(decl) = c.node.type_name.node.declarator {
        type_builder.process_declarator_node(decl, ec)?.1
    } else {
        type_builder.finalize()
    };
    let value = compute_constant_expr(*c.node.expression, allow_var, tu, ec)?;

    Ok(TypedValue {
        val: cast(value, &new_type, c.span, ec)?,
        t: new_type,
    })
}

fn cast(
    value: TypedValue,
    new_type: &QualifiedType,
    span: Span,
    ec: &mut ErrorCollector,
) -> Result<Value, ()> {
    if !value.t.is_explicit_castable_to(&new_type) {
        ec.record_error(
            CompileError::BadCast(format!("{}", value.t), format!("{}", new_type)),
            span,
        )?;
    }
    let new_v = match new_type.t {
        CType::Void => Value::Void,
        CType::Int(new_size, new_sign) => match value.t.t {
            CType::Int(old_size, old_sign) => {
                cast_int(old_size, old_sign, new_size, new_sign, value.val)
            }
            CType::Bool => cast_from_bool(value.val),
            CType::Pointer(_) => cast_int(machine::PTR_SIZE, false, new_size, new_sign, value.val),
            CType::Float(_) => todo!(),
            CType::Array(_, _) => {
                ec.record_error(CompileError::NonConstInConstExpr, span)?;
                unreachable!()
            }
            _ => unreachable!(),
        },
        CType::Float(_) => todo!(),
        CType::Pointer(_) => match value.t.t {
            CType::Int(old_size, old_sign) => {
                cast_int(old_size, old_sign, machine::PTR_SIZE, false, value.val)
            }
            CType::Bool => cast_from_bool(value.val),
            CType::Pointer(_) => value.val,
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

fn cast_int(size_from: u8, sign_from: bool, size_to: u8, sign_to: bool, value: Value) -> Value {
    if let Value::Int(value) = value {
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
        Value::Int(new_val)
    } else {
        panic!("value doesn't match type")
    }
}

fn cast_from_bool(value: Value) -> Value {
    if let Value::Int(value) = value {
        let new_val = if value == 0 { 0 } else { 1 };
        Value::Int(new_val)
    } else {
        panic!("value doesn't match type")
    }
}

fn parse_char_literal(s: &str, ec: &mut ErrorCollector) -> Result<TypedValue, ()> {
    todo!()
}
