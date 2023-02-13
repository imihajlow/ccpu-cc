use lang_c::span::Span;
use lang_c::{ast::Expression, span::Node};

use crate::ctype::CType;
use crate::error::CompileError;
use crate::lvalue::TypedLValue;
use crate::{
    block_emitter::BlockEmitter,
    error::{CompileWarning, ErrorCollector},
    ir,
    name_scope::NameScope,
};

use super::{cast, compile_expression, TypedSrc};

/**
 * Assignment according to 6.5.16.1
 */
pub fn compile_assign(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    let lhs_span = lhs.span;
    let lhs_lval = TypedLValue::new_compile(lhs, scope, be, ec)?;
    if lhs_lval.t.is_const() {
        ec.record_error(
            CompileError::AssignmentToConstQualified(lhs_lval.t),
            lhs_span,
        )?;
        unreachable!();
    }
    let rhs_span = rhs.span;
    let rhs_val = compile_expression(rhs, scope, be, ec)?;
    compile_assign_to_lval(lhs_lval, (rhs_val, rhs_span), scope, be, ec)
}

/**
 * Common function for assignments and initializers. Allows assignments to const.
 */
pub fn compile_assign_to_lval(
    lhs_lval: TypedLValue,
    rhs: (TypedSrc, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    use crate::lvalue::LValue;
    let (rhs_val, rhs_span) = rhs;

    if lhs_lval.t.t.is_arithmetic() && rhs_val.t.t.is_arithmetic() {
        // the left operand has atomic, qualified, or unqualified arithmetic type, and the right has arithmetic type;
        // the left operand has type atomic, qualified, or unqualified _Bool, and the right is a pointer.
        let rhs_casted = cast(rhs_val, &lhs_lval.t.t, false, rhs_span, scope, be, ec)?;
        let width = rhs_casted.t.t.get_width_sign().unwrap().0;
        match lhs_lval.lv {
            LValue::Var(v) => {
                be.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: v,
                    src: rhs_casted.src.clone(),
                    width,
                }));
            }
            LValue::Indirection(addr) => {
                be.append_operation(ir::Op::Store(ir::StoreOp {
                    dst_addr: addr,
                    src: rhs_casted.src.clone(),
                    width,
                }));
            }
        }
        Ok(rhs_casted)
    } else if lhs_lval.t.t.is_pointer() {
        // the left operand has atomic, qualified, or unqualified pointer type,
        // ... and (considering the type the left operand would have after lvalue conversion)
        //     both operands are pointers to qualified or unqualified versions of compatible types,
        //     and the type pointed to by the left has all the qualifiers of the type pointed to by the right;
        // ... and (considering the type the left operand would have after lvalue conversion)
        //     one operand is a pointer to an object type, and the other is a pointer to a qualified or unqualified version of void,
        //     and the type pointed to by the left has all the qualifiers of the type pointed to by the right;
        // ... and the right is a null pointer constant;
        match &rhs_val.t.t {
            CType::Void
            | CType::Float(_)
            | CType::Struct(_)
            | CType::Union(_)
            | CType::Function { .. } => {
                ec.record_error(
                    CompileError::IncompatibleTypes(lhs_lval.t, rhs_val.t),
                    rhs_span,
                )?;
                unreachable!()
            }
            CType::Pointer(target) | CType::Array(target, _) => {
                let lhs_pointee = lhs_lval.t.clone().dereference().unwrap();
                if lhs_pointee != **target {
                    if !target.t.is_void() && !lhs_pointee.t.is_void() {
                        ec.record_warning(
                            CompileWarning::IncompatibleTypes(
                                lhs_lval.t.clone(),
                                rhs_val.t.clone(),
                            ),
                            rhs_span,
                        )?;
                    }
                }
            }
            CType::Bool | CType::Enum(_) | CType::Int(_, _) => {
                ec.record_warning(
                    CompileWarning::IncompatibleTypes(lhs_lval.t.clone(), rhs_val.t.clone()),
                    rhs_span,
                )?;
            }
        }
        let rhs_casted = cast(rhs_val, &lhs_lval.t.t, false, rhs_span, scope, be, ec)?;
        let width = rhs_casted.t.t.get_width_sign().unwrap().0;
        match lhs_lval.lv {
            LValue::Var(v) => {
                be.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: v,
                    src: rhs_casted.src.clone(),
                    width,
                }));
            }
            LValue::Indirection(addr) => {
                be.append_operation(ir::Op::Store(ir::StoreOp {
                    dst_addr: addr,
                    src: rhs_casted.src.clone(),
                    width,
                }));
            }
        }
        Ok(rhs_casted)
    } else if lhs_lval.t.is_compatible_to(&rhs_val.t, true) {
        // the left operand has an atomic, qualified, or unqualified version of a structure or union type
        // compatible with the type of the right;
        todo!("{}", rhs_val.t)
    } else {
        ec.record_error(CompileError::IncompatibleTypes(lhs_lval.t, rhs_val.t), rhs_span)?;
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

    fn assert_compile_error(code: &str) {
        use lang_c::driver::{parse_preprocessed, Config, Flavor};
        let mut cfg = Config::default();
        cfg.flavor = Flavor::StdC11;
        let p = parse_preprocessed(&cfg, code.to_string()).unwrap();
        let mut ec = ErrorCollector::new();
        assert!(TranslationUnit::translate(p.unit, &mut ec).is_err());
    }

    fn get_first_body(tu: &TranslationUnit) -> &Vec<LabeledBlock> {
        tu.functions.first().unwrap().get_body()
    }

    #[test]
    fn test_assign_1() {
        let (tu, ec) = compile("void foo(void) { int x; x = 15; }");
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Src::ConstInt(15),
                width: ir::Width::Word
            })]
        );
    }

    #[test]
    fn test_assign_2() {
        let (tu, ec) = compile("void foo(void) { char x; x = 15; }");
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Conv(ir::ConvOp {
                    dst: VarLocation::Local(1),
                    dst_sign: false,
                    dst_width: ir::Width::Byte,
                    src: ir::Src::ConstInt(15),
                    src_sign: true,
                    src_width: ir::Width::Word
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(0),
                    src: ir::Src::Var(VarLocation::Local(1)),
                    width: ir::Width::Byte
                }),
            ]
        );
    }

    #[test]
    fn test_assign_3() {
        let (tu, ec) = compile("void foo(void) { int *x; *x = 15; }");
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![ir::Op::Store(ir::StoreOp {
                dst_addr: ir::Src::Var(VarLocation::Local(0)),
                src: ir::Src::ConstInt(15),
                width: ir::Width::Word
            })]
        );
    }

    #[test]
    fn test_assign_4() {
        let (tu, ec) = compile("void foo(void) { int x; x = 15; }");
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Src::ConstInt(15),
                width: ir::Width::Word
            })]
        );
    }

    #[test]
    fn test_assign_5() {
        let (tu, ec) = compile("void foo(void) { int *x; int *y; x = y; }");
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
    fn test_assign_6() {
        let (tu, ec) = compile("void foo(void) { char *x; char y[43]; x = y; }");
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
    fn test_assign_7() {
        let (tu, ec) = compile("void foo(void) { int *x; unsigned int y; x = y; }");
        assert_eq!(ec.get_warning_count(), 1);
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
    fn test_assign_8() {
        let (tu, ec) = compile("void foo(void) { int *x; char *y; x = y; }");
        assert_eq!(ec.get_warning_count(), 1);
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
    fn test_assign_9() {
        let (tu, ec) = compile("void foo(void) { int *x; void *y; x = y; }");
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
    fn test_assign_10() {
        let (tu, ec) = compile("void foo(void) { void *x; int *y; x = y; }");
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
    fn test_assign_11() {
        let (tu, ec) = compile("void foo(void) { int *x; const int *y; x = y; }");
        assert_eq!(ec.get_warning_count(), 1);
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
    fn test_assign_12() {
        assert_compile_error("void foo(void) { int *x; float y; x = y; }");
    }
}
