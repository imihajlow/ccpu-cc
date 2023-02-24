use lang_c::ast::UnaryOperatorExpression;
use lang_c::{ast::Expression, span::Node};

use crate::block_emitter::BlockEmitter;
use crate::ctype::{self, QualifiedType, Qualifiers};
use crate::error::{CompileError, ErrorCollector};
use crate::ir;
use crate::lvalue::{LValue, TypedLValue};
use crate::name_scope::NameScope;
use crate::rvalue::{RValue, TypedRValue};

use super::assign::compile_assign_to_lval;
use super::compile_expression;
use super::{add, int_promote, sub};

pub fn compile_unary_operator(
    op: Node<UnaryOperatorExpression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    use lang_c::ast::UnaryOperator;
    let operand = *op.node.operand;
    match op.node.operator.node {
        UnaryOperator::Plus => compile_unary_plus(operand, scope, be, ec),
        UnaryOperator::Minus => compile_unary_minus(operand, scope, be, ec),
        UnaryOperator::Complement => compile_unary_complement(operand, scope, be, ec),
        UnaryOperator::Negate => compile_unary_lnot(operand, scope, be, ec),
        UnaryOperator::PreIncrement => compile_pre_incdec(true, operand, scope, be, ec),
        UnaryOperator::PreDecrement => compile_pre_incdec(false, operand, scope, be, ec),
        UnaryOperator::PostIncrement => compile_post_incdec(true, operand, scope, be, ec),
        UnaryOperator::PostDecrement => compile_post_incdec(false, operand, scope, be, ec),
        UnaryOperator::Address => compile_address(operand, scope, be, ec),
        UnaryOperator::Indirection => compile_deref(operand, scope, be, ec),
    }
}

fn compile_unary_plus(
    operand: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let span = operand.span;
    let operand = compile_expression(operand, scope, be, ec)?;
    if !operand.t.t.is_arithmetic() {
        ec.record_error(CompileError::ArithmeticTypeRequired, span)?;
        unreachable!();
    }
    if operand.t.t.is_integer() {
        let operand = int_promote(operand, scope, be);
        Ok(operand)
    } else {
        todo!()
    }
}

fn compile_unary_minus(
    operand: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let span = operand.span;
    let operand = compile_expression(operand, scope, be, ec)?;
    if !operand.t.t.is_arithmetic() {
        ec.record_error(CompileError::ArithmeticTypeRequired, span)?;
        unreachable!();
    }
    if operand.t.t.is_integer() {
        let operand = int_promote(operand, scope, be);
        let (operand_scalar, operand_type) = operand.unwrap_scalar_and_type();
        let result = scope.alloc_temp();
        let width = operand_type.t.get_scalar_width().unwrap();
        be.append_operation(ir::Op::Neg(ir::UnaryUnsignedOp {
            dst: result.clone(),
            src: operand_scalar,
            width,
        }));
        Ok(TypedRValue {
            src: RValue::new_var(result),
            t: operand_type,
        })
    } else {
        todo!()
    }
}

fn compile_unary_complement(
    operand: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let span = operand.span;
    let operand = compile_expression(operand, scope, be, ec)?;
    if !operand.t.t.is_integer() {
        ec.record_error(CompileError::IntegerTypeRequired, span)?;
        unreachable!();
    }
    let operand = int_promote(operand, scope, be);
    let (operand_scalar, operand_type) = operand.unwrap_scalar_and_type();
    let result = scope.alloc_temp();
    let width = operand_type.t.get_scalar_width().unwrap();
    be.append_operation(ir::Op::Not(ir::UnaryUnsignedOp {
        dst: result.clone(),
        src: operand_scalar,
        width,
    }));
    Ok(TypedRValue {
        src: RValue::new_var(result),
        t: operand_type,
    })
}

fn compile_unary_lnot(
    operand: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let span = operand.span;
    let operand = compile_expression(operand, scope, be, ec)?;
    if !operand.t.t.is_scalar() {
        ec.record_error(CompileError::ScalarTypeRequired, span)?;
        unreachable!();
    }
    let operand = int_promote(operand, scope, be);
    let result = scope.alloc_temp();
    let width = operand.t.t.get_scalar_width().unwrap();
    be.append_operation(ir::Op::BoolInv(ir::UnaryUnsignedOp {
        dst: result.clone(),
        src: operand.unwrap_scalar(),
        width,
    }));
    Ok(TypedRValue {
        src: RValue::new_var(result),
        t: QualifiedType {
            t: ctype::INT_TYPE,
            qualifiers: Qualifiers::empty(),
        },
    })
}

fn compile_pre_incdec(
    is_inc: bool,
    operand: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let span = operand.span;
    let operand = TypedLValue::new_compile(operand, scope, be, ec)?;
    if operand.t.is_const() {
        ec.record_error(CompileError::AssignmentToConstQualified(operand.t), span)?;
        unreachable!();
    }
    if !operand.t.t.is_arithmetic() && !operand.t.t.is_pointer() {
        ec.record_error(CompileError::ScalarTypeRequired, span)?; // TODO array is scalar, but can't be used here
        unreachable!();
    }
    let operand_rvalue = operand.clone().compile_into_rvalue(scope, be)?;
    let one = TypedRValue {
        src: RValue::new_const(1),
        t: QualifiedType {
            t: ctype::INT_TYPE,
            qualifiers: Qualifiers::empty(),
        },
    };
    let result = if is_inc {
        add::compile_add_inner((operand_rvalue, span), (one, span), scope, be, ec)?
    } else {
        sub::compile_sub_inner((operand_rvalue, span), (one, span), scope, be, ec)?
    };
    compile_assign_to_lval(operand, (result, span), scope, be, ec)
}

fn compile_post_incdec(
    is_inc: bool,
    operand: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let span = operand.span;
    let operand = TypedLValue::new_compile(operand, scope, be, ec)?;
    if operand.t.is_const() {
        ec.record_error(CompileError::AssignmentToConstQualified(operand.t), span)?;
        unreachable!();
    }
    if !operand.t.t.is_arithmetic() && !operand.t.t.is_pointer() {
        ec.record_error(CompileError::ScalarTypeRequired, span)?; // TODO array is scalar, but can't be used here
        unreachable!();
    }
    let operand_rvalue = operand.clone().compile_into_rvalue(scope, be)?;
    let operand_type = operand_rvalue.t.clone();
    let old_val = scope.alloc_temp();
    let width = operand_rvalue.t.t.get_scalar_width().unwrap();
    be.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
        dst: old_val.clone(),
        src: operand_rvalue.clone().unwrap_scalar(),
        width,
    }));
    let one = TypedRValue {
        src: RValue::new_const(1),
        t: QualifiedType {
            t: ctype::INT_TYPE,
            qualifiers: Qualifiers::empty(),
        },
    };
    let result = if is_inc {
        add::compile_add_inner((operand_rvalue, span), (one, span), scope, be, ec)?
    } else {
        sub::compile_sub_inner((operand_rvalue, span), (one, span), scope, be, ec)?
    };
    compile_assign_to_lval(operand, (result, span), scope, be, ec)?;
    Ok(TypedRValue {
        src: RValue::new_var(old_val),
        t: operand_type,
    })
}

fn compile_address(
    operand: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let operand = TypedLValue::new_compile(operand, scope, be, ec)?;

    let mut t = operand.t.clone();
    t.wrap_pointer(Qualifiers::empty());

    match operand.lv {
        LValue::Indirection(src) => Ok(TypedRValue {
            src: RValue::Scalar(src),
            t,
        }),
        LValue::Var(v) => {
            scope.fix_in_memory(&v);
            let dst = scope.alloc_temp();
            be.append_operation(ir::Op::LoadAddr(ir::LoadAddrOp {
                dst: dst.clone(),
                src: v,
            }));
            Ok(TypedRValue {
                src: RValue::new_var(dst),
                t,
            })
        }
        LValue::Object(addr) => Ok(TypedRValue {
            src: RValue::Scalar(addr),
            t,
        }),
    }
}

fn compile_deref(
    operand: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let span = operand.span;
    let operand = compile_expression(operand, scope, be, ec)?;

    let (operand_scalar, operand_type) = operand.unwrap_scalar_and_type();

    match operand_type.dereference() {
        Ok(pointee) => {
            if let Some(width) = pointee.t.get_scalar_width() {
                let dst = scope.alloc_temp();
                be.append_operation(ir::Op::Load(ir::LoadOp {
                    dst: dst.clone(),
                    src_addr: operand_scalar,
                    width,
                }));
                Ok(TypedRValue {
                    src: RValue::new_var(dst),
                    t: pointee,
                })
            } else {
                todo!()
            }
        }
        Err(t) => {
            ec.record_error(CompileError::BadIndirection(t), span)?;
            unreachable!();
        }
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
        tu.functions.first().unwrap().get_body()
    }

    #[test]
    fn test_unary_1() {
        let (tu, ec) = compile("void foo(void) { int x, y; x = +y; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::Var(VarLocation::Local(1)),
                width: ir::Width::Word
            })]
        );
    }

    #[test]
    fn test_unary_2() {
        let (tu, ec) = compile("void foo(void) { long x; unsigned int y; x = -y; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Neg(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(2),
                    src: ir::Scalar::Var(VarLocation::Local(1)),
                    width: ir::Width::Word,
                }),
                ir::Op::Conv(ir::ConvOp {
                    dst: VarLocation::Local(3),
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    dst_width: ir::Width::Dword,
                    src_width: ir::Width::Word,
                    dst_sign: true,
                    src_sign: false
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::Var(VarLocation::Local(3)),
                    width: ir::Width::Dword
                })
            ]
        );
    }

    #[test]
    fn test_unary_3() {
        let (tu, ec) = compile("void foo(void) { int x, y; x = ~y; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Not(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(2),
                    src: ir::Scalar::Var(VarLocation::Local(1)),
                    width: ir::Width::Word,
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    width: ir::Width::Word
                })
            ]
        );
    }

    #[test]
    fn test_unary_4() {
        let (tu, ec) = compile("void foo(void) { int x, y; x = !y; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::BoolInv(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(2),
                    src: ir::Scalar::Var(VarLocation::Local(1)),
                    width: ir::Width::Word,
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    width: ir::Width::Word
                })
            ]
        );
    }

    #[test]
    fn test_unary_5() {
        let (tu, ec) = compile("void foo(void) { int x, y; x = ++y; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(2),
                    lhs: ir::Scalar::Var(VarLocation::Local(1)),
                    rhs: ir::Scalar::ConstInt(1),
                    width: ir::Width::Word,
                    sign: true,
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(1),
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    width: ir::Width::Word
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    width: ir::Width::Word
                })
            ]
        );
    }

    #[test]
    fn test_unary_6() {
        let (tu, ec) = compile("void foo(void) { long int *x, *y; x = --y; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Mul(ir::BinaryOp {
                    dst: VarLocation::Local(2),
                    lhs: ir::Scalar::ConstInt(1),
                    rhs: ir::Scalar::ConstInt(4),
                    width: ir::Width::Word,
                    sign: true,
                }),
                ir::Op::Sub(ir::BinaryOp {
                    dst: VarLocation::Local(3),
                    lhs: ir::Scalar::Var(VarLocation::Local(1)),
                    rhs: ir::Scalar::Var(VarLocation::Local(2)),
                    width: ir::Width::Word,
                    sign: true,
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(1),
                    src: ir::Scalar::Var(VarLocation::Local(3)),
                    width: ir::Width::Word
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::Var(VarLocation::Local(3)),
                    width: ir::Width::Word
                })
            ]
        );
    }

    #[test]
    fn test_unary_7() {
        let (tu, ec) = compile("void foo(void) { int x, y; x = y++; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(2),
                    src: ir::Scalar::Var(VarLocation::Local(1)),
                    width: ir::Width::Word
                }),
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(3),
                    lhs: ir::Scalar::Var(VarLocation::Local(1)),
                    rhs: ir::Scalar::ConstInt(1),
                    width: ir::Width::Word,
                    sign: true,
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(1),
                    src: ir::Scalar::Var(VarLocation::Local(3)),
                    width: ir::Width::Word
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    width: ir::Width::Word
                })
            ]
        );
    }

    #[test]
    fn test_unary_8() {
        let (tu, ec) = compile("void foo(void) { long int *x, *y; x = y--; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(2),
                    src: ir::Scalar::Var(VarLocation::Local(1)),
                    width: ir::Width::Word
                }),
                ir::Op::Mul(ir::BinaryOp {
                    dst: VarLocation::Local(3),
                    lhs: ir::Scalar::ConstInt(1),
                    rhs: ir::Scalar::ConstInt(4),
                    width: ir::Width::Word,
                    sign: true,
                }),
                ir::Op::Sub(ir::BinaryOp {
                    dst: VarLocation::Local(4),
                    lhs: ir::Scalar::Var(VarLocation::Local(1)),
                    rhs: ir::Scalar::Var(VarLocation::Local(3)),
                    width: ir::Width::Word,
                    sign: true,
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(1),
                    src: ir::Scalar::Var(VarLocation::Local(4)),
                    width: ir::Width::Word
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    width: ir::Width::Word
                })
            ]
        );
    }

    #[test]
    fn test_addr_1() {
        let (tu, ec) = compile("void foo(void) { int *x, y; x = &y; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        let fixed_regs = tu.scope.get_fixed_regs();
        assert_eq!(fixed_regs.len(), 1);
        assert!(fixed_regs.contains(&1));
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::LoadAddr(ir::LoadAddrOp {
                    dst: VarLocation::Local(2),
                    src: VarLocation::Local(1),
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    width: ir::Width::Word
                })
            ]
        );
    }

    #[test]
    fn test_addr_2() {
        let (tu, ec) = compile("void foo(void) { int *x, *y; x = &*y; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        let fixed_regs = tu.scope.get_fixed_regs();
        assert_eq!(fixed_regs.len(), 0);
        assert_eq!(
            body[0].ops,
            vec![ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::Var(VarLocation::Local(1)),
                width: ir::Width::Word
            })]
        );
    }

    #[test]
    fn test_deref_1() {
        let (tu, ec) = compile("void foo(void) { int x, *y; x = *y; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        let fixed_regs = tu.scope.get_fixed_regs();
        assert_eq!(fixed_regs.len(), 0);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Load(ir::LoadOp {
                    dst: VarLocation::Local(2),
                    src_addr: ir::Scalar::Var(VarLocation::Local(1)),
                    width: ir::Width::Word,
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    width: ir::Width::Word
                })
            ]
        );
    }
}
