use lang_c::span::Span;
use lang_c::{ast::Expression, span::Node};

use crate::block_emitter::BlockEmitter;
use crate::error::{CompileError, ErrorCollector};
use crate::ir;
use crate::name_scope::NameScope;
use crate::rvalue::{RValue, TypedRValue};

use super::{compile_expression, compile_pointer_offset, usual_arithmetic_convert};

pub fn compile_add(
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

    compile_add_inner((lhs, lhs_span), (rhs, rhs_span), scope, be, ec)
}

pub fn compile_add_inner(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let (lhs, lhs_span) = lhs;
    let (rhs, rhs_span) = rhs;
    if lhs.t.t.is_arithmetic() && rhs.t.t.is_arithmetic() {
        let (lhs, rhs) = usual_arithmetic_convert((lhs, lhs_span), (rhs, rhs_span), scope, be, ec)?;
        let (lhs_scalar, lhs_type) = lhs.unwrap_scalar_and_type();
        if lhs_type.t.is_integer() {
            let (width, sign) = lhs_type.t.get_width_sign().unwrap();
            let target = scope.alloc_temp();
            be.append_operation(ir::Op::Add(ir::BinaryOp {
                dst: target.clone(),
                width,
                sign,
                lhs: lhs_scalar,
                rhs: rhs.unwrap_scalar(),
            }));
            Ok(TypedRValue {
                src: RValue::new_var(target),
                t: lhs_type,
            })
        } else {
            todo!()
        }
    } else if (lhs.t.t.is_dereferencable() && rhs.t.t.is_integer())
        || (lhs.t.t.is_integer() && rhs.t.t.is_dereferencable())
    {
        let (ptr, offset, ptr_span, offset_span) = if lhs.t.t.is_dereferencable() {
            (lhs, rhs, lhs_span, rhs_span)
        } else {
            (rhs, lhs, rhs_span, lhs_span)
        };
        compile_pointer_offset((ptr, ptr_span), (offset, offset_span), false, scope, be, ec)
    } else {
        ec.record_error(CompileError::IncompatibleTypes(lhs.t, rhs.t), rhs_span)?;
        unreachable!();
    }
}

pub fn compile_index(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let ptr_span = lhs.span;
    let index_span = rhs.span;
    let ptr = compile_expression(lhs, scope, be, ec)?;
    let index = compile_expression(rhs, scope, be, ec)?;
    let offset =
        compile_pointer_offset((ptr, ptr_span), (index, index_span), false, scope, be, ec)?;
    let pointee = offset.t.clone().dereference().unwrap();
    let width = pointee.t.get_scalar_width().unwrap();
    let dst = scope.alloc_temp();
    be.append_operation(ir::Op::Load(ir::LoadOp {
        dst: dst.clone(),
        src_addr: offset.unwrap_scalar(),
        width,
    }));
    Ok(TypedRValue {
        src: RValue::new_var(dst),
        t: pointee,
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
    fn test_add_1() {
        let (tu, ec) = compile("void foo(void) { int x, y, z; x = y + z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(3),
                    width: ir::Width::Word,
                    sign: true,
                    lhs: ir::Scalar::Var(VarLocation::Local(1)),
                    rhs: ir::Scalar::Var(VarLocation::Local(2))
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
    fn test_add_2() {
        let (tu, ec) = compile("void foo(void) { int x, y; char z; x = y + z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Conv(ir::ConvOp {
                    dst: VarLocation::Local(3),
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    dst_width: ir::Width::Word,
                    dst_sign: true,
                    src_width: ir::Width::Byte,
                    src_sign: false,
                }),
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(4),
                    width: ir::Width::Word,
                    sign: true,
                    lhs: ir::Scalar::Var(VarLocation::Local(1)),
                    rhs: ir::Scalar::Var(VarLocation::Local(3))
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::Var(VarLocation::Local(4)),
                    width: ir::Width::Word
                })
            ]
        );
    }

    #[test]
    fn test_add_ptr_1() {
        let (tu, ec) = compile("void foo(void) { int *x, *y, z; x = y + z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Mul(ir::BinaryOp {
                    dst: VarLocation::Local(3),
                    width: ir::Width::Word,
                    sign: true,
                    lhs: ir::Scalar::Var(VarLocation::Local(2)),
                    rhs: ir::Scalar::ConstInt(2)
                }),
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(4),
                    width: ir::Width::Word,
                    sign: false,
                    lhs: ir::Scalar::Var(VarLocation::Local(1)),
                    rhs: ir::Scalar::Var(VarLocation::Local(3))
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Scalar::Var(VarLocation::Local(4)),
                    width: ir::Width::Word
                })
            ]
        );
    }

    #[test]
    fn test_assign_add_1() {
        let (tu, ec) = compile("void foo(void) { int x, y; x += y; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(2),
                    width: ir::Width::Word,
                    sign: true,
                    lhs: ir::Scalar::Var(VarLocation::Local(0)),
                    rhs: ir::Scalar::Var(VarLocation::Local(1))
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
    fn test_assign_add_2() {
        let (tu, ec) = compile("void foo(void) { int *x, y; *x += y; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Load(ir::LoadOp {
                    dst: VarLocation::Local(2),
                    src_addr: ir::Scalar::Var(VarLocation::Local(0)),
                    width: ir::Width::Word,
                }),
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(3),
                    width: ir::Width::Word,
                    sign: true,
                    lhs: ir::Scalar::Var(VarLocation::Local(2)),
                    rhs: ir::Scalar::Var(VarLocation::Local(1))
                }),
                ir::Op::Store(ir::StoreOp {
                    dst_addr: ir::Scalar::Var(VarLocation::Local(0)),
                    src: ir::Scalar::Var(VarLocation::Local(3)),
                    width: ir::Width::Word
                })
            ]
        );
    }

    #[test]
    fn test_index_1() {
        let (tu, ec) = compile("void foo(void) { long *x; int y; x[y]; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Mul(ir::BinaryOp {
                    dst: VarLocation::Local(2),
                    width: ir::Width::Word,
                    sign: true,
                    lhs: ir::Scalar::Var(VarLocation::Local(1)),
                    rhs: ir::Scalar::ConstInt(4)
                }),
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(3),
                    width: ir::Width::Word,
                    sign: false,
                    lhs: ir::Scalar::Var(VarLocation::Local(0)),
                    rhs: ir::Scalar::Var(VarLocation::Local(2))
                }),
                ir::Op::Load(ir::LoadOp {
                    dst: VarLocation::Local(4),
                    src_addr: ir::Scalar::Var(VarLocation::Local(3)),
                    width: ir::Width::Dword,
                }),
            ]
        );
    }
}
