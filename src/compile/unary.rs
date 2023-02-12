use lang_c::ast::{BinaryOperatorExpression, UnaryOperatorExpression};
use lang_c::span::Span;
use lang_c::{ast::Expression, span::Node};

use crate::block_emitter::BlockEmitter;
use crate::compile::add::compile_index;
use crate::compile::relational;
use crate::compile::shift::{
    compile_lshift, compile_lshift_inner, compile_rshift, compile_rshift_inner,
};
use crate::ctype::{self, QualifiedType, Qualifiers};
use crate::error::{CompileError, ErrorCollector};
use crate::ir;
use crate::lvalue::TypedLValue;
use crate::name_scope::NameScope;

use super::{add, assign, int_promote, sub};
use super::{compile_expression, usual_arithmetic_convert, TypedSrc};

pub fn compile_unary_operator(
    op: Node<UnaryOperatorExpression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    use lang_c::ast::UnaryOperator;
    let operand = *op.node.operand;
    match op.node.operator.node {
        UnaryOperator::Plus => compile_unary_plus(operand, scope, be, ec),
        UnaryOperator::Minus => compile_unary_minus(operand, scope, be, ec),
        UnaryOperator::Complement => compile_unary_complement(operand, scope, be, ec),
        UnaryOperator::Negate => compile_unary_lnot(operand, scope, be, ec),
        _ => todo!(),
    }
}

fn compile_unary_plus(
    operand: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
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
) -> Result<TypedSrc, ()> {
    let span = operand.span;
    let operand = compile_expression(operand, scope, be, ec)?;
    if !operand.t.t.is_arithmetic() {
        ec.record_error(CompileError::ArithmeticTypeRequired, span)?;
        unreachable!();
    }
    if operand.t.t.is_integer() {
        let operand = int_promote(operand, scope, be);
        let result = scope.alloc_temp();
        let width = operand.t.t.get_scalar_width().unwrap();
        be.append_operation(ir::Op::Neg(ir::UnaryUnsignedOp {
            dst: result.clone(),
            src: operand.src,
            width,
        }));
        Ok(TypedSrc {
            src: ir::Src::Var(result),
            t: operand.t,
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
) -> Result<TypedSrc, ()> {
    let span = operand.span;
    let operand = compile_expression(operand, scope, be, ec)?;
    if !operand.t.t.is_integer() {
        ec.record_error(CompileError::IntegerTypeRequired, span)?;
        unreachable!();
    }
    let operand = int_promote(operand, scope, be);
    let result = scope.alloc_temp();
    let width = operand.t.t.get_scalar_width().unwrap();
    be.append_operation(ir::Op::Not(ir::UnaryUnsignedOp {
        dst: result.clone(),
        src: operand.src,
        width,
    }));
    Ok(TypedSrc {
        src: ir::Src::Var(result),
        t: operand.t,
    })
}

fn compile_unary_lnot(
    operand: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
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
        src: operand.src,
        width,
    }));
    Ok(TypedSrc {
        src: ir::Src::Var(result),
        t: QualifiedType {
            t: ctype::INT_TYPE,
            qualifiers: Qualifiers::empty(),
        },
    })
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
                src: ir::Src::Var(VarLocation::Local(1)),
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
                    src: ir::Src::Var(VarLocation::Local(1)),
                    width: ir::Width::Word,
                }),
                ir::Op::Conv(ir::ConvOp {
                    dst: VarLocation::Local(3),
                    src: ir::Src::Var(VarLocation::Local(2)),
                    dst_width: ir::Width::Dword,
                    src_width: ir::Width::Word,
                    dst_sign: true,
                    src_sign: false
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Src::Var(VarLocation::Local(3)),
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
                    src: ir::Src::Var(VarLocation::Local(1)),
                    width: ir::Width::Word,
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Src::Var(VarLocation::Local(2)),
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
                    src: ir::Src::Var(VarLocation::Local(1)),
                    width: ir::Width::Word,
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Src::Var(VarLocation::Local(2)),
                    width: ir::Width::Word
                })
            ]
        );
    }
}
