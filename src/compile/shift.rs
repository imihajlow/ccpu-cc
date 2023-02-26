use lang_c::span::Span;
use lang_c::{ast::Expression, span::Node};

use crate::block_emitter::BlockEmitter;
use crate::error::{CompileError, ErrorCollector};
use crate::ir;
use crate::name_scope::NameScope;
use crate::rvalue::{RValue, TypedRValue};

use super::{compile_expression, int_promote};

pub fn compile_lshift(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs = compile_expression(lhs, scope, be, ec)?;
    let rhs = compile_expression(rhs, scope, be, ec)?;

    compile_lshift_inner((lhs, lhs_span), (rhs, rhs_span), scope, be, ec)
}

pub fn compile_rshift(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs = compile_expression(lhs, scope, be, ec)?;
    let rhs = compile_expression(rhs, scope, be, ec)?;

    compile_rshift_inner((lhs, lhs_span), (rhs, rhs_span), scope, be, ec)
}

pub fn compile_lshift_inner(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    compile_shift_inner(lhs, rhs, ir::Op::LShift, scope, be, ec)
}

pub fn compile_rshift_inner(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    compile_shift_inner(lhs, rhs, ir::Op::RShift, scope, be, ec)
}

fn compile_shift_inner<F>(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    constr: F,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()>
where
    F: FnOnce(ir::ShiftOp) -> ir::Op,
{
    let (lhs, lhs_span) = lhs;
    let (rhs, rhs_span) = rhs;

    if !lhs.t.t.is_integer() {
        ec.record_error(CompileError::IntegerTypeRequired, lhs_span)?;
        unreachable!();
    }
    if !rhs.t.t.is_integer() {
        ec.record_error(CompileError::IntegerTypeRequired, rhs_span)?;
        unreachable!();
    }

    let lhs = int_promote(lhs, scope, be);
    let rhs = int_promote(rhs, scope, be);

    let (lhs_scalar, lhs_type) = lhs.unwrap_scalar_and_type();

    let dst = scope.alloc_temp();
    let (lhs_width, lhs_sign) = lhs_type.t.get_width_sign().unwrap();

    be.append_operation(constr(ir::ShiftOp {
        dst: dst.clone(),
        lhs_width,
        lhs_sign,
        lhs: lhs_scalar,
        rhs: rhs.unwrap_scalar(),
    }));

    Ok(TypedRValue {
        src: RValue::new_var(dst),
        t: lhs_type,
    })
}

#[cfg(test)]
mod test {
    use crate::ir::{self, VarLocation};
    use crate::translation_unit::TranslationUnit;

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

    fn get_first_body(tu: &TranslationUnit) -> &Vec<ir::Block> {
        tu.functions.first().unwrap().get_body()
    }

    #[test]
    fn test_shift_1() {
        let (tu, ec) = compile("void foo(void) { long x, y; int z; x = y << z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::LShift(ir::ShiftOp {
                    dst: VarLocation::Local(3),
                    lhs_width: ir::Width::Dword,
                    lhs_sign: true,
                    lhs: ir::Scalar::Var(VarLocation::Local(1)),
                    rhs: ir::Scalar::Var(VarLocation::Local(2))
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
    fn test_shift_2() {
        let (tu, ec) = compile("void foo(void) { long x; int z; x >>= z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::RShift(ir::ShiftOp {
                    dst: VarLocation::Local(2),
                    lhs_width: ir::Width::Dword,
                    lhs_sign: true,
                    lhs: ir::Scalar::Var(VarLocation::Local(0)),
                    rhs: ir::Scalar::Var(VarLocation::Local(1))
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    width: ir::Width::Dword
                })
            ]
        );
    }
}
