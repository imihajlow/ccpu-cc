use lang_c::{ast::Expression, span::Node};

use crate::block_emitter::BlockEmitter;
use crate::ctype::Qualifiers;
use crate::error::{CompileError, ErrorCollector};
use crate::name_scope::NameScope;
use crate::{ctype, ir};

use super::{cast_if_needed, compile_expression, usual_arithmetic_convert, TypedSrc};

pub fn compile_add(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs = compile_expression(lhs, scope, be, ec)?;
    let rhs = compile_expression(rhs, scope, be, ec)?;

    if lhs.t.t.is_arithmetic() && rhs.t.t.is_arithmetic() {
        let (lhs, rhs) = usual_arithmetic_convert((lhs, lhs_span), (rhs, rhs_span), scope, be, ec)?;
        if lhs.t.t.is_integer() {
            let (width, sign) = lhs.t.t.get_width_sign().unwrap();
            let target = scope.alloc_temp();
            be.append_operation(ir::Op::Add(ir::BinaryOp {
                dst: target.clone(),
                width,
                sign,
                lhs: lhs.src,
                rhs: rhs.src,
            }));
            Ok(TypedSrc {
                src: ir::Src::Var(target),
                t: lhs.t,
            })
        } else {
            todo!()
        }
    } else if (lhs.t.t.is_pointer() && rhs.t.t.is_integer())
        || (lhs.t.t.is_integer() && rhs.t.t.is_pointer())
    {
        let (lhs, rhs, lhs_span, rhs_span) = if lhs.t.t.is_pointer() {
            (lhs, rhs, lhs_span, rhs_span)
        } else {
            (rhs, lhs, rhs_span, lhs_span)
        };
        if !lhs.t.t.is_pointer_to_complete() {
            ec.record_error(CompileError::SizeOfIncomplete(lhs.t), lhs_span)?;
            unreachable!();
        }
        let rhs_ssize = cast_if_needed(rhs, &ctype::SSIZE_TYPE, rhs_span, scope, be, ec)?;
        let element_size = lhs
            .t
            .clone()
            .dereference()
            .unwrap()
            .t
            .sizeof(lhs_span, ec)?;
        let element_size_src = TypedSrc {
            t: ctype::QualifiedType {
                t: ctype::SSIZE_TYPE,
                qualifiers: Qualifiers::empty(),
            },
            src: ir::Src::ConstInt(element_size.into()),
        };
        let offset_var = scope.alloc_temp();
        let target_var = scope.alloc_temp();
        let (width, sign) = ctype::SSIZE_TYPE.get_width_sign().unwrap();
        be.append_operation(ir::Op::Mul(ir::BinaryOp {
            dst: offset_var.clone(),
            lhs: rhs_ssize.src,
            rhs: element_size_src.src,
            width,
            sign,
        }));
        be.append_operation(ir::Op::Add(ir::BinaryOp {
            dst: target_var.clone(),
            lhs: lhs.src,
            rhs: ir::Src::Var(offset_var),
            width,
            sign,
        }));
        Ok(TypedSrc {
            src: ir::Src::Var(target_var),
            t: lhs.t,
        })
    } else {
        ec.record_error(CompileError::IncompatibleTypes(lhs.t, rhs.t), rhs_span)?;
        unreachable!();
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
                    lhs: ir::Src::Var(VarLocation::Local(1)),
                    rhs: ir::Src::Var(VarLocation::Local(2))
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Src::Var(VarLocation::Local(3)),
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
                    src: ir::Src::Var(VarLocation::Local(2)),
                    dst_width: ir::Width::Word,
                    dst_sign: true,
                    src_width: ir::Width::Byte,
                    src_sign: false,
                }),
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(4),
                    width: ir::Width::Word,
                    sign: true,
                    lhs: ir::Src::Var(VarLocation::Local(1)),
                    rhs: ir::Src::Var(VarLocation::Local(3))
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Src::Var(VarLocation::Local(4)),
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
                    lhs: ir::Src::Var(VarLocation::Local(2)),
                    rhs: ir::Src::ConstInt(2)
                }),
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(4),
                    width: ir::Width::Word,
                    sign: true,
                    lhs: ir::Src::Var(VarLocation::Local(1)),
                    rhs: ir::Src::Var(VarLocation::Local(3))
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Src::Var(VarLocation::Local(4)),
                    width: ir::Width::Word
                })
            ]
        );
    }
}
