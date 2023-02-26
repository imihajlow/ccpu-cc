use lang_c::ast::{AlignOf, SizeOfTy, SizeOfVal};
use lang_c::span::Span;
use lang_c::{ast::Expression, span::Node};

use crate::block_emitter::BlockEmitter;
use crate::ctype::{self, QualifiedType, Qualifiers};
use crate::error::{CompileError, ErrorCollector};
use crate::ir;
use crate::name_scope::NameScope;
use crate::rvalue::{RValue, TypedRValue};
use crate::type_builder::TypeBuilder;

use super::{compile_expression, compile_pointer_offset, usual_arithmetic_convert};

pub fn compile_sizeof_val(
    si: Node<SizeOfVal>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    // compile, find out type and then drop
    let t = {
        let mut be = be.clone();
        let expr = compile_expression(*si.node.0, scope, &mut be, ec)?;
        expr.t
    };

    Ok(TypedRValue {
        src: RValue::new_const(t.t.sizeof(scope, si.span, ec)? as u64),
        t: QualifiedType {
            t: ctype::INT_TYPE,
            qualifiers: Qualifiers::empty(),
        },
    })
}

pub fn compile_sizeof_type(
    si: Node<SizeOfTy>,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let type_builder =
        TypeBuilder::new_from_specifiers_qualifiers(si.node.0.node.specifiers, scope, ec)?;
    let stage2 = type_builder.stage2(si.span, ec)?;
    let t = if let Some(decl) = si.node.0.node.declarator {
        stage2.process_declarator_node(decl, scope, ec)?.1
    } else {
        stage2.finalize()
    };

    Ok(TypedRValue {
        src: RValue::new_const(t.t.sizeof(scope, si.span, ec)? as u64),
        t: QualifiedType {
            t: ctype::INT_TYPE,
            qualifiers: Qualifiers::empty(),
        },
    })
}

pub fn compile_alignof(
    si: Node<AlignOf>,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let type_builder =
        TypeBuilder::new_from_specifiers_qualifiers(si.node.0.node.specifiers, scope, ec)?;
    let stage2 = type_builder.stage2(si.span, ec)?;
    let t = if let Some(decl) = si.node.0.node.declarator {
        stage2.process_declarator_node(decl, scope, ec)?.1
    } else {
        stage2.finalize()
    };

    Ok(TypedRValue {
        src: RValue::new_const(t.t.alignof(scope, si.span, ec)? as u64),
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
    fn test_sizof_val_1() {
        let (tu, ec) = compile("int bar(int x); void foo(void) { int x = sizeof(bar(5)); }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::ConstInt(2),
                width: ir::Width::Word
            })]
        );
    }

    #[test]
    fn test_sizof_ty_1() {
        let (tu, ec) = compile("void foo(void) { int x = sizeof(const unsigned long long int); }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::ConstInt(8),
                width: ir::Width::Word
            })]
        );
    }

    #[test]
    fn test_sizof_ty_2() {
        let (tu, ec) =
            compile("void foo(void) { int x = sizeof(struct { int x; long long y; }); }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::ConstInt(16),
                width: ir::Width::Word
            })]
        );
    }

    #[test]
    fn test_alignof_1() {
        let (tu, ec) =
            compile("void foo(void) { int x = _Alignof(struct { int x; long long y; }); }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::ConstInt(8),
                width: ir::Width::Word
            })]
        );
    }

    #[test]
    fn test_alignof_2() {
        let (tu, ec) = compile("void foo(void) { int x = _Alignof(char *); }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::ConstInt(2),
                width: ir::Width::Word
            })]
        );
    }

    #[test]
    fn test_alignof_3() {
        let (tu, ec) =
            compile("void foo(void) { int x = _Alignof(union { int x; long long y; }); }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::ConstInt(8),
                width: ir::Width::Word
            })]
        );
    }
}
