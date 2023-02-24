use lang_c::ast::{CallExpression, CastExpression};
use lang_c::span::Span;
use lang_c::{
    ast::{BlockItem, Declaration, Expression, Initializer, Statement, StorageClassSpecifier},
    span::Node,
};

use crate::ctype::{self, CType, FunctionArgs, Qualifiers};
use crate::error::CompileError;
use crate::lvalue::TypedLValue;
use crate::{
    block_emitter::BlockEmitter,
    constant,
    ctype::QualifiedType,
    error::{CompileWarning, ErrorCollector},
    ir,
    name_scope::NameScope,
    rvalue::{RValue, TypedRValue},
    type_builder::TypeBuilder,
};
use crate::{machine, type_builder};

use self::sizeof::{compile_alignof, compile_sizeof_type, compile_sizeof_val};

mod add;
mod assign;
mod binary;
mod relational;
mod shift;
mod sizeof;
mod sub;
mod unary;

pub fn compile_statement(
    stat: Node<Statement>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    match stat.node {
        Statement::Expression(None) => {}
        Statement::Expression(Some(e)) => {
            compile_expression(*e, scope, be, ec)?;
            ()
        }
        Statement::Compound(items) => compile_block(items, scope, be, ec)?,
        Statement::If(ifs) => be.append_if(ifs, scope, ec)?,
        Statement::While(whiles) => be.append_while(whiles, scope, ec)?,
        Statement::For(fors) => be.append_for(fors, scope, ec)?,
        Statement::DoWhile(whiles) => be.append_do_while(whiles, scope, ec)?,
        _ => todo!(),
    }
    Ok(())
}

pub fn compile_expression(
    expr: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    match expr.node {
        Expression::Constant(_) => {
            let v = constant::compute_constant_expr(expr, false, scope, ec)?;
            Ok(TypedRValue::new_from_typed_constant(v))
        }
        Expression::Identifier(id) => scope.get_rvalue(&id.node.name, id.span, ec),
        Expression::BinaryOperator(o) => binary::compile_binary_operator(*o, scope, be, ec),
        Expression::UnaryOperator(o) => unary::compile_unary_operator(*o, scope, be, ec),
        Expression::Comma(c) => compile_comma(*c, scope, be, ec),
        Expression::Conditional(c) => be.append_conditional(*c, scope, ec),
        Expression::Cast(c) => compile_cast(*c, scope, be, ec),
        Expression::Call(c) => compile_call(*c, scope, be, ec),
        Expression::SizeOfTy(si) => compile_sizeof_type(*si, scope, ec),
        Expression::SizeOfVal(si) => compile_sizeof_val(*si, scope, be, ec),
        Expression::AlignOf(si) => compile_alignof(*si, scope, ec),
        Expression::OffsetOf(_) => todo!(),
        Expression::StringLiteral(_) => todo!(),
        Expression::Member(_) => todo!(),
        Expression::CompoundLiteral(_) => todo!(),
        Expression::VaArg(_) => todo!(),
        Expression::GenericSelection(_) => unimplemented!(),
        Expression::Statement(_) => unimplemented!(),
    }
}

pub fn compile_initializer(
    initializer: Node<Initializer>,
    target: &str,
    target_span: Span,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    match initializer.node {
        Initializer::Expression(e) => {
            let lvalue = TypedLValue::new_from_name(target, target_span, scope, ec)?;
            let rhs_span = e.span;
            let rhs = compile_expression(*e, scope, be, ec)?;
            assign::compile_assign_to_lval(lvalue, (rhs, rhs_span), scope, be, ec)?;
            Ok(())
        }
        Initializer::List(_) => todo!(),
    }
}

/**
 * Do the integer promotion.
 *
 * Panics if src is not an integer.
 */
pub fn int_promote(src: TypedRValue, scope: &mut NameScope, be: &mut BlockEmitter) -> TypedRValue {
    if !src.t.t.is_integer() {
        panic!("not an integer");
    }
    let promoted_t = src.t.clone().promote();
    if src.t == promoted_t {
        src
    } else {
        let (src_width, src_sign) = src.t.t.get_width_sign().unwrap();
        let (dst_width, dst_sign) = promoted_t.t.get_width_sign().unwrap();
        let dst = scope.alloc_temp();
        be.append_operation(ir::Op::Conv(ir::ConvOp {
            src_width,
            src_sign,
            dst_width,
            dst_sign,
            dst: dst.clone(),
            src: src.unwrap_scalar(),
        }));

        TypedRValue {
            src: RValue::new_var(dst),
            t: promoted_t,
        }
    }
}

pub fn convert_to_bool(
    src: TypedRValue,
    span: Span,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    if !src.t.t.is_scalar() {
        ec.record_error(CompileError::ScalarTypeRequired, span)?;
        unreachable!();
    }
    let dst = scope.alloc_temp();
    let width = src.t.t.get_scalar_width().unwrap();
    be.append_operation(ir::Op::Bool(ir::UnaryUnsignedOp {
        dst: dst.clone(),
        src: src.unwrap_scalar(),
        width,
    }));
    Ok(TypedRValue {
        src: RValue::new_var(dst),
        t: QualifiedType {
            t: CType::Int(1, false),
            qualifiers: Qualifiers::empty(),
        },
    })
}

pub fn cast(
    src: TypedRValue,
    target_type: &CType,
    copy_if_same_type: bool,
    span: Span,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    assert!(target_type.is_scalar());
    assert!(src.t.t.is_scalar());
    if target_type.is_any_float() || src.t.t.is_any_float() {
        todo!()
    }
    let (dst_width, dst_sign) = target_type.get_width_sign().unwrap();
    let (src_width, src_sign) = src.t.t.get_width_sign().unwrap();
    if (dst_width, dst_sign) != (src_width, src_sign) {
        let dst = scope.alloc_temp();
        be.append_operation(ir::Op::Conv(ir::ConvOp {
            dst_width,
            dst_sign,
            src_width,
            src_sign,
            dst: dst.clone(),
            src: src.unwrap_scalar(),
        }));
        Ok(TypedRValue {
            src: RValue::new_var(dst),
            t: QualifiedType {
                t: target_type.clone(),
                qualifiers: Qualifiers::empty(),
            },
        })
    } else {
        if copy_if_same_type {
            let dst = scope.alloc_temp();
            be.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: dst.clone(),
                src: src.unwrap_scalar(),
                width: dst_width,
            }));
            Ok(TypedRValue {
                src: RValue::new_var(dst),
                t: QualifiedType {
                    t: target_type.clone(),
                    qualifiers: Qualifiers::empty(),
                },
            })
        } else {
            Ok(src)
        }
    }
}

/**
 * Given a pointer and an index, make an absolute address.
 *
 * Integer promotions are performed on index.
 */
pub fn compile_pointer_offset(
    ptr: (TypedRValue, Span),
    index: (TypedRValue, Span),
    subtract: bool,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let (ptr, ptr_span) = ptr;
    let (index, index_span) = index;

    if !ptr.t.t.is_dereferencable() {
        ec.record_error(CompileError::PointerTypeRequired, ptr_span)?;
        unreachable!();
    }
    if !index.t.t.is_integer() {
        ec.record_error(CompileError::IntegerTypeRequired, ptr_span)?;
        unreachable!();
    }

    let index = int_promote(index, scope, be);

    if index.t.t.sizeof(scope, index_span, ec)? > machine::PTR_SIZE.into() {
        ec.record_warning(CompileWarning::IndexTooWide(index.t.clone()), index_span)?;
    }

    let index_ssize = cast(index, &ctype::SSIZE_TYPE, false, index_span, scope, be, ec)?;
    let element_size = ptr
        .t
        .clone()
        .dereference()
        .unwrap()
        .t
        .sizeof(scope, ptr_span, ec)?;
    let element_size_src = TypedRValue {
        t: ctype::QualifiedType {
            t: ctype::SSIZE_TYPE,
            qualifiers: Qualifiers::empty(),
        },
        src: RValue::new_const(element_size.into()),
    };
    let offset_var = scope.alloc_temp();
    let target_var = scope.alloc_temp();
    let (width, sign) = ctype::SSIZE_TYPE.get_width_sign().unwrap();
    be.append_operation(ir::Op::Mul(ir::BinaryOp {
        dst: offset_var.clone(),
        lhs: index_ssize.unwrap_scalar(),
        rhs: element_size_src.unwrap_scalar(),
        width,
        sign,
    }));
    let op = if !subtract { ir::Op::Add } else { ir::Op::Sub };
    let (ptr_scalar, ptr_type) = ptr.unwrap_scalar_and_type();
    be.append_operation(op(ir::BinaryOp {
        dst: target_var.clone(),
        lhs: ptr_scalar,
        rhs: ir::Scalar::Var(offset_var),
        width,
        sign: false,
    }));
    Ok(TypedRValue {
        src: RValue::new_var(target_var),
        t: ptr_type,
    })
}

fn compile_block(
    block: Vec<Node<BlockItem>>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    scope.push();
    for item in block {
        match item.node {
            BlockItem::Statement(stat) => compile_statement(stat, scope, be, ec)?,
            BlockItem::Declaration(decl) => compile_declaration(decl, scope, be, ec)?,
            BlockItem::StaticAssert(sa) => constant::check_static_assert(sa, scope, ec)?,
        }
    }
    scope.pop_and_collect_initializers();
    Ok(())
}

fn compile_declaration(
    decl: Node<Declaration>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    let (type_builder, stclass, _extra) =
        TypeBuilder::new_from_specifiers(decl.node.specifiers, scope, ec)?;
    for init_declarator in decl.node.declarators {
        let tb = type_builder.stage2(init_declarator.span, ec)?;
        let (name, t) = tb.process_declarator_node(init_declarator.node.declarator, scope, ec)?;
        if name.is_none() {
            ec.record_warning(CompileWarning::EmptyDeclaration, init_declarator.span)?;
            continue;
        }
        let name = name.unwrap();
        let is_static = matches!(
            stclass,
            Some(Node {
                node: StorageClassSpecifier::Static,
                ..
            })
        );
        if is_static {
            let initializer = if let Some(initializer) = init_declarator.node.initializer {
                Some(constant::compute_constant_initializer(
                    initializer,
                    &t,
                    true,
                    scope,
                    ec,
                )?)
            } else {
                None
            };
            scope.declare(&name, t, &stclass, initializer, init_declarator.span, ec)?;
        } else {
            scope.declare(&name, t, &stclass, None, init_declarator.span, ec)?;
            if let Some(initializer) = init_declarator.node.initializer {
                compile_initializer(initializer, &name, init_declarator.span, scope, be, ec)?;
            }
        };
    }
    Ok(())
}

pub fn integer_promote(
    src: (TypedRValue, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let promoted_type = src.0.t.clone().promote();

    cast(src.0, &promoted_type.t, false, src.1, scope, be, ec)
}

/**
 * Perform usual arithmetic conversions according to 6.3.1.8
 */
fn usual_arithmetic_convert(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<(TypedRValue, TypedRValue), ()> {
    let (lhs, lhs_span) = lhs;
    let (rhs, rhs_span) = rhs;
    if lhs.t.t.is_long_double() || rhs.t.t.is_long_double() {
        todo!()
    } else if lhs.t.t.is_double() || rhs.t.t.is_double() {
        todo!()
    } else if lhs.t.t.is_float() || rhs.t.t.is_float() {
        todo!()
    } else if lhs.t.t.is_integer() && rhs.t.t.is_integer() {
        // Otherwise, the integer promotions are performed on both operands.
        let lhs = integer_promote((lhs, lhs_span), scope, be, ec)?;
        let rhs = integer_promote((rhs, rhs_span), scope, be, ec)?;
        // Then the following rules are applied to the promoted operands
        let common_type = lhs.t.t.least_common_int_type(&rhs.t.t);
        Ok((
            cast(lhs, &common_type, false, lhs_span, scope, be, ec)?,
            cast(rhs, &common_type, false, rhs_span, scope, be, ec)?,
        ))
    } else {
        unreachable!()
    }
}

fn compile_comma(
    c: Vec<Node<Expression>>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let mut result = None;
    for e in c {
        let r = compile_expression(e, scope, be, ec)?;
        result.replace(r);
    }
    Ok(result.unwrap())
}

fn compile_cast(
    c: Node<CastExpression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let type_span = c.node.type_name.span;
    let expr_span = c.node.expression.span;
    let target_type = type_builder::build_type_from_ast_type_name(c.node.type_name, scope, ec)?;
    let val = compile_expression(*c.node.expression, scope, be, ec)?;

    if target_type.t.is_void() {
        Ok(TypedRValue {
            src: RValue::new_void(),
            t: target_type,
        })
    } else if target_type.t.is_arithmetic() || target_type.t.is_pointer() {
        if !val.t.t.is_scalar() {
            ec.record_error(CompileError::ScalarTypeRequired, expr_span)?;
            unreachable!();
        }

        let casted = cast(val, &target_type.t, false, c.span, scope, be, ec)?;

        Ok(TypedRValue {
            src: casted.src,
            t: QualifiedType {
                t: casted.t.t,
                qualifiers: target_type.qualifiers,
            },
        })
    } else {
        ec.record_error(
            CompileError::ArithmeticOrPointerTypeRequired(target_type),
            type_span,
        )?;
        unreachable!();
    }
}

fn compile_call(
    c: Node<CallExpression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let callee_span = c.node.callee.span;
    let mut args_srcs = Vec::new();
    for arg in c.node.arguments {
        let span = arg.span;
        let rvalue = compile_expression(arg, scope, be, ec)?;
        args_srcs.push((rvalue, span));
    }
    let callee = compile_expression(*c.node.callee, scope, be, ec)?;
    let (result_type, arg_types, vararg) = if let CType::Function {
        result,
        args,
        vararg,
    } = callee.t.t.clone()
    {
        (*result, args, vararg)
    } else if let Ok(QualifiedType {
        t: CType::Function {
            result,
            args,
            vararg,
        },
        ..
    }) = callee.t.clone().dereference()
    {
        (*result, args, vararg)
    } else {
        ec.record_error(CompileError::NotAFunction(callee.t), callee_span)?;
        unreachable!();
    };

    let given_args_count = args_srcs.len();

    let arg_locations = match arg_types {
        FunctionArgs::Void => {
            if given_args_count != 0 {
                ec.record_error(CompileError::TooManyArguments(given_args_count, 0), c.span)?;
                unreachable!()
            }
            Vec::new()
        }
        FunctionArgs::Empty => {
            ec.record_warning(CompileWarning::ImplicitArgumentTypes, c.span)?;

            let mut arg_locations = Vec::new();
            for (arg_src, span) in args_srcs {
                arg_locations.push(compile_argument(None, arg_src, span, scope, be, ec)?);
            }
            arg_locations
        }
        FunctionArgs::List(arg_types) => {
            let expected_args_count = arg_types.len();
            if given_args_count < expected_args_count {
                ec.record_error(
                    CompileError::TooFewArguments(given_args_count, expected_args_count),
                    c.span,
                )?;
                unreachable!()
            }
            if given_args_count > expected_args_count && !vararg {
                ec.record_error(
                    CompileError::TooManyArguments(given_args_count, expected_args_count),
                    c.span,
                )?;
                unreachable!()
            }

            let mut args_src_iter = args_srcs.into_iter();
            let mut arg_locations = Vec::new();
            for (arg_type, _, _) in arg_types {
                let (arg_src, span) = args_src_iter.next().unwrap();
                arg_locations.push(compile_argument(
                    Some(arg_type),
                    arg_src,
                    span,
                    scope,
                    be,
                    ec,
                )?);
            }

            for (arg_src, span) in args_src_iter {
                arg_locations.push(compile_argument(None, arg_src, span, scope, be, ec)?);
            }

            arg_locations
        }
    };

    let result_location = if result_type.t.is_void() {
        None
    } else if result_type.t.is_scalar() {
        let dst = scope.alloc_temp();
        Some((dst, result_type.t.get_scalar_width().unwrap()))
    } else {
        todo!("return struct")
    };

    be.append_operation(ir::Op::Call(ir::CallOp {
        dst: result_location.clone(),
        addr: callee.unwrap_scalar(),
        args: arg_locations,
    }));

    if let Some((result, _)) = result_location {
        Ok(TypedRValue {
            src: RValue::new_var(result),
            t: result_type,
        })
    } else {
        Ok(TypedRValue {
            src: RValue::new_void(),
            t: result_type,
        })
    }
}

fn compile_argument(
    arg_type: Option<QualifiedType>,
    arg_src: TypedRValue,
    span: Span,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<(ir::Scalar, ir::Width), ()> {
    if let Some(arg_type) = arg_type {
        if arg_type.t.is_scalar() {
            let dst = cast(arg_src, &arg_type.t, true, span, scope, be, ec)?;
            let (s, t) = dst.unwrap_scalar_and_type();
            let w = t.t.get_scalar_width().unwrap();
            Ok((s, w))
        } else {
            todo!()
        }
    } else {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use crate::ir::{self, VarLocation};
    use crate::{block_emitter::LabeledBlock, translation_unit::TranslationUnit};

    use super::*;

    fn compile(code: &str) -> (TranslationUnit, ErrorCollector) {
        use lang_c::driver::{parse_preprocessed, Config, Flavor};
        let mut cfg = Config::default();
        cfg.flavor = Flavor::StdC11;
        let p = parse_preprocessed(&cfg, code.to_string()).unwrap();
        let mut ec = ErrorCollector::new();
        let tu = TranslationUnit::translate(p.unit, &mut ec).unwrap();
        assert_eq!(ec.get_error_count(), 0);
        (tu, ec)
    }

    fn get_first_body(tu: &TranslationUnit) -> &Vec<LabeledBlock> {
        tu.functions[0].get_body()
    }

    #[test]
    fn test_call_1() {
        let (tu, ec) = compile("void bar(int x, unsigned char y); void foo(void) { bar(10, 20); }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::ConstInt(10),
                    width: ir::Width::Word
                }),
                ir::Op::Conv(ir::ConvOp {
                    dst: VarLocation::Local(1),
                    dst_width: ir::Width::Byte,
                    dst_sign: false,
                    src: ir::Scalar::ConstInt(20),
                    src_width: ir::Width::Word,
                    src_sign: true,
                }),
                ir::Op::Call(ir::CallOp {
                    addr: ir::Scalar::Var(VarLocation::Global(ir::GlobalVarId(
                        "bar".to_string(),
                        0
                    ))),
                    dst: None,
                    args: vec![
                        (ir::Scalar::Var(VarLocation::Local(0)), ir::Width::Word),
                        (ir::Scalar::Var(VarLocation::Local(1)), ir::Width::Byte)
                    ]
                })
            ]
        );
    }

    #[test]
    fn test_call_2() {
        let (tu, ec) = compile("long bar(int x); void foo(void) { int x = bar(10); }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(1),
                    src: ir::Scalar::ConstInt(10),
                    width: ir::Width::Word
                }),
                ir::Op::Call(ir::CallOp {
                    addr: ir::Scalar::Var(VarLocation::Global(ir::GlobalVarId(
                        "bar".to_string(),
                        0
                    ))),
                    dst: Some((VarLocation::Local(2), ir::Width::Dword)),
                    args: vec![(ir::Scalar::Var(VarLocation::Local(1)), ir::Width::Word),]
                }),
                ir::Op::Conv(ir::ConvOp {
                    dst: VarLocation::Local(3),
                    dst_width: ir::Width::Word,
                    dst_sign: true,
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    src_width: ir::Width::Dword,
                    src_sign: true,
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::Var(VarLocation::Local(3)),
                    width: ir::Width::Word
                }),
            ]
        );
    }
}
