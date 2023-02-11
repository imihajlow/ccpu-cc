use lang_c::span::Span;
use lang_c::{ast::Expression, span::Node};

use crate::block_emitter::BlockEmitter;
use crate::ctype::{QualifiedType, Qualifiers};
use crate::error::{CompileWarning, ErrorCollector};
use crate::name_scope::NameScope;
use crate::{ctype, ir};

use super::{compile_expression, usual_arithmetic_convert, TypedSrc};

pub fn compile_equal_to(
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

    compile_cmp_inner(
        (lhs, lhs_span),
        (rhs, rhs_span),
        ir::CompareKind::Equal,
        scope,
        be,
        ec,
    )
}

pub fn compile_not_equal_to(
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

    compile_cmp_inner(
        (lhs, lhs_span),
        (rhs, rhs_span),
        ir::CompareKind::NotEqual,
        scope,
        be,
        ec,
    )
}

pub fn compile_less_than(
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

    compile_cmp_inner(
        (lhs, lhs_span),
        (rhs, rhs_span),
        ir::CompareKind::LessThan,
        scope,
        be,
        ec,
    )
}

pub fn compile_less_or_equal(
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

    compile_cmp_inner(
        (lhs, lhs_span),
        (rhs, rhs_span),
        ir::CompareKind::LessOrEqual,
        scope,
        be,
        ec,
    )
}

pub fn compile_greater_than(
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

    compile_cmp_inner(
        (lhs, lhs_span),
        (rhs, rhs_span),
        ir::CompareKind::GreaterThan,
        scope,
        be,
        ec,
    )
}

pub fn compile_greater_or_equal(
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

    compile_cmp_inner(
        (lhs, lhs_span),
        (rhs, rhs_span),
        ir::CompareKind::GreaterOrEqual,
        scope,
        be,
        ec,
    )
}

pub fn compile_cmp_inner(
    lhs: (TypedSrc, Span),
    rhs: (TypedSrc, Span),
    kind: ir::CompareKind,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    let (lhs, lhs_span) = lhs;
    let (rhs, rhs_span) = rhs;

    if lhs.t.t.is_arithmetic() && rhs.t.t.is_arithmetic() {
        let (lhs, rhs) = usual_arithmetic_convert((lhs, lhs_span), (rhs, rhs_span), scope, be, ec)?;

        assert!(lhs.t.t == rhs.t.t);

        if lhs.t.t.is_integer() {
            let (width, sign) = lhs.t.t.get_width_sign().unwrap();
            let dst = scope.alloc_temp();
            be.append_operation(ir::Op::Compare(ir::CompareOp {
                kind,
                dst: dst.clone(),
                dst_width: ir::Width::INT_WIDTH,
                lhs: lhs.src,
                rhs: rhs.src,
                width,
                sign,
            }));
            Ok(TypedSrc {
                src: ir::Src::Var(dst),
                t: QualifiedType {
                    t: ctype::INT_TYPE,
                    qualifiers: Qualifiers::empty(),
                },
            })
        } else {
            todo!()
        }
    } else if lhs.t.t.is_dereferencable() && rhs.t.t.is_dereferencable() {
        let lhs_inner = lhs.t.clone().dereference().unwrap();
        let rhs_inner = rhs.t.clone().dereference().unwrap();
        if !lhs_inner.is_compatible_to(&rhs_inner) {
            if (kind != ir::CompareKind::Equal && kind != ir::CompareKind::NotEqual)
                || (!lhs_inner.t.is_void() && !rhs_inner.t.is_void())
            {
                ec.record_warning(
                    CompileWarning::IncompatibleTypes(lhs_inner, rhs_inner),
                    rhs_span,
                )?;
            }
        }
        let dst = scope.alloc_temp();
        let (width, sign) = lhs.t.t.get_width_sign().unwrap();
        be.append_operation(ir::Op::Compare(ir::CompareOp {
            kind,
            dst: dst.clone(),
            dst_width: ir::Width::INT_WIDTH,
            lhs: lhs.src,
            rhs: rhs.src,
            width,
            sign,
        }));
        Ok(TypedSrc {
            src: ir::Src::Var(dst),
            t: QualifiedType {
                t: ctype::INT_TYPE,
                qualifiers: Qualifiers::empty(),
            },
        })
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
        tu.functions.first().unwrap().get_body()
    }

    #[test]
    fn test_rel_1() {
        let (tu, ec) = compile("void foo(void) { int x, y, z; x = y < z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Compare(ir::CompareOp {
                    kind: ir::CompareKind::LessThan,
                    dst_width: ir::Width::Word,
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
    fn test_rel_2() {
        let (tu, ec) = compile("void foo(void) { int x, y; unsigned long z; x = y >= z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Conv(ir::ConvOp {
                    dst: VarLocation::Local(3),
                    dst_width: ir::Width::Dword,
                    dst_sign: false,
                    src_width: ir::Width::Word,
                    src_sign: true,
                    src: ir::Src::Var(VarLocation::Local(1))
                }),
                ir::Op::Compare(ir::CompareOp {
                    kind: ir::CompareKind::GreaterOrEqual,
                    dst_width: ir::Width::Word,
                    dst: VarLocation::Local(4),
                    width: ir::Width::Dword,
                    sign: false,
                    lhs: ir::Src::Var(VarLocation::Local(3)),
                    rhs: ir::Src::Var(VarLocation::Local(2))
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
    fn test_rel_3() {
        let (tu, ec) = compile("void foo(void) { int x, *y; void *z; x = y <= z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 1);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Compare(ir::CompareOp {
                    kind: ir::CompareKind::LessOrEqual,
                    dst_width: ir::Width::Word,
                    dst: VarLocation::Local(3),
                    width: ir::Width::Word,
                    sign: false,
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
    fn test_rel_4() {
        let (tu, ec) = compile("void foo(void) { int x, *y, * const z; x = y > z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Compare(ir::CompareOp {
                    kind: ir::CompareKind::GreaterThan,
                    dst_width: ir::Width::Word,
                    dst: VarLocation::Local(3),
                    width: ir::Width::Word,
                    sign: false,
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
    fn test_rel_5() {
        let (tu, ec) = compile("void foo(void) { int x, *y; void * const z; x = y == z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Compare(ir::CompareOp {
                    kind: ir::CompareKind::Equal,
                    dst_width: ir::Width::Word,
                    dst: VarLocation::Local(3),
                    width: ir::Width::Word,
                    sign: false,
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
    fn test_rel_6() {
        let (tu, ec) = compile("void foo(void) { int x, *y; char * z; x = y != z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 1);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Compare(ir::CompareOp {
                    kind: ir::CompareKind::NotEqual,
                    dst_width: ir::Width::Word,
                    dst: VarLocation::Local(3),
                    width: ir::Width::Word,
                    sign: false,
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
}
