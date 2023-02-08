use lang_c::span::Span;
use lang_c::{
    ast::{
        BinaryOperatorExpression, BlockItem, Declaration, Expression, Initializer, Statement,
        StorageClassSpecifier,
    },
    span::Node,
};

use crate::ctype::{CType, Qualifiers};
use crate::error::CompileError;
use crate::lvalue::{self, TypedLValue};
use crate::{
    block_emitter::BlockEmitter,
    constant,
    ctype::QualifiedType,
    error::{CompileWarning, ErrorCollector},
    initializer::TypedValue,
    ir,
    name_scope::NameScope,
    type_builder::TypeBuilder,
};

pub struct TypedSrc {
    pub src: ir::Src,
    pub t: QualifiedType,
}

impl TypedSrc {
    pub fn new_from_typed_value(tv: TypedValue) -> Self {
        use crate::initializer::Value;
        match tv.val {
            Value::Int(x) => Self {
                src: ir::Src::ConstInt(x as u64),
                t: tv.t,
            },
            Value::Void => panic!("void constant"),
            _ => todo!(),
        }
    }
}

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
) -> Result<TypedSrc, ()> {
    match expr.node {
        Expression::Constant(_) => {
            let v = constant::compute_constant_expr(expr, false, scope, ec)?;
            Ok(TypedSrc::new_from_typed_value(v))
        }
        Expression::Identifier(id) => {
            let (t, v) = scope.get_var(&id.node.name, id.span, ec)?;
            Ok(TypedSrc {
                src: ir::Src::Var(v),
                t: t.clone(),
            })
        }
        Expression::BinaryOperator(o) => compile_binary_operator(*o, scope, be, ec),
        Expression::GenericSelection(_) => unimplemented!(),
        _ => todo!(),
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
            compile_assign_to_lval(lvalue, *e, scope, be, ec)?;
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
pub fn int_promote(src: TypedSrc, scope: &mut NameScope, be: &mut BlockEmitter) -> TypedSrc {
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
            src: src.src,
        }));

        TypedSrc {
            src: ir::Src::Var(dst),
            t: promoted_t,
        }
    }
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
    let (mut type_builder, stclass, _extra) =
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

fn compile_binary_operator(
    op: Node<BinaryOperatorExpression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    use lang_c::ast::BinaryOperator;
    match op.node.operator.node {
        BinaryOperator::Assign => compile_assign(*op.node.lhs, *op.node.rhs, scope, be, ec),
        _ => todo!(),
    }
}

/**
 * Assignment according to 6.5.16.1
 */
fn compile_assign(
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
    compile_assign_to_lval(lhs_lval, rhs, scope, be, ec)
}

/**
 * Common function for assignments and initializers. Allows assignments to const.
 */
fn compile_assign_to_lval(
    lhs_lval: TypedLValue,
    rhs: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    use crate::lvalue::LValue;
    let rhs_span = rhs.span;
    let rhs_val = compile_expression(rhs, scope, be, ec)?;

    if lhs_lval.t.t.is_arithmetic() && rhs_val.t.t.is_arithmetic() {
        // the left operand has atomic, qualified, or unqualified arithmetic type, and the right has arithmetic type;
        let rhs_casted = cast_if_needed(rhs_val, &lhs_lval.t.t, rhs_span, scope, be, ec)?;
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
                            CompileWarning::IncompatibleTypes(lhs_lval.t.clone(), rhs_val.t.clone()),
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
        let rhs_casted = cast_if_needed(rhs_val, &lhs_lval.t.t, rhs_span, scope, be, ec)?;
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
    } else {
        // the left operand has an atomic, qualified, or unqualified version of a structure or union type compatible with the type of the right;

        // the left operand has type atomic, qualified, or unqualified _Bool, and the right is a pointer.
        todo!()
    }
}

fn cast_if_needed(
    src: TypedSrc,
    target_type: &CType,
    span: Span,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    let dst = scope.alloc_temp();
    let (dst_width, dst_sign) = target_type.get_width_sign().unwrap();
    let (src_width, src_sign) = src.t.t.get_width_sign().unwrap();
    if (dst_width, dst_sign) != (src_width, src_sign) {
        be.append_operation(ir::Op::Conv(ir::ConvOp {
            dst_width,
            dst_sign,
            src_width,
            src_sign,
            dst: dst.clone(),
            src: src.src,
        }));
        Ok(TypedSrc {
            src: ir::Src::Var(dst),
            t: QualifiedType {
                t: target_type.clone(),
                qualifiers: Qualifiers::empty(),
            },
        })
    } else {
        Ok(src)
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
