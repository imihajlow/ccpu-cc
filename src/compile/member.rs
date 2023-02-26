use lang_c::ast::{MemberExpression, MemberOperator};
use lang_c::span::Node;

use crate::block_emitter::BlockEmitter;
use crate::error::CompileError;
use crate::name_scope::NameScope;
use crate::object_location::ObjectLocation;
use crate::rvalue::{RValue, TypedRValue};
use crate::{ir, ErrorCollector};

use super::compile_expression;

pub fn compile_member_expression(
    expr: Node<MemberExpression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let id = expr.node.identifier.node.name;
    let id_span = expr.node.identifier.span;
    let (obj_type, obj_addr) = match expr.node.operator.node {
        MemberOperator::Direct => {
            let lhs = compile_expression(*expr.node.expression, scope, be, ec)?;
            let obj_type = lhs.t;
            let obj_addr = lhs.src.get_object_address().unwrap();
            (obj_type, obj_addr)
        }
        MemberOperator::Indirect => {
            let lhs = compile_expression(*expr.node.expression, scope, be, ec)?;
            let obj_type = match lhs.t.dereference() {
                Ok(t) => t,
                Err(t) => {
                    ec.record_error(CompileError::BadIndirection(t), expr.span)?;
                    unreachable!();
                }
            };
            let obj_addr = lhs.src.unwrap_scalar();
            (obj_type, obj_addr)
        }
    };
    let (field_offset, mut field_type) = obj_type.get_field(&id, scope, id_span, ec)?;
    field_type.qualifiers |= obj_type.qualifiers;
    let field_addr_var = scope.alloc_temp();
    be.append_operation(ir::Op::Add(ir::BinaryOp {
        dst: field_addr_var.clone(),
        width: ir::Width::PTR_WIDTH,
        sign: false,
        lhs: obj_addr,
        rhs: ir::Scalar::ConstInt(field_offset as u64),
    }));
    if field_type.t.is_scalar() {
        let target_var = scope.alloc_temp();
        let width = field_type.t.get_scalar_width().unwrap();
        be.append_operation(ir::Op::Load(ir::LoadOp {
            dst: target_var.clone(),
            src_addr: ir::Scalar::Var(field_addr_var),
            width,
        }));
        Ok(TypedRValue {
            t: field_type,
            src: RValue::new_var(target_var),
        })
    } else if field_type.t.is_object() {
        Ok(TypedRValue {
            t: field_type,
            src: RValue::new_object(ObjectLocation::PointedBy(ir::Scalar::Var(field_addr_var))),
        })
    } else {
        todo!()
    }
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
    fn test_member_1() {
        let (tu, ec) =
            compile("struct X { long a; int b; }; void foo(void) { struct X x; int y; y = x.b; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(1),
                    width: ir::Width::PTR_WIDTH,
                    sign: false,
                    lhs: ir::Scalar::FrameOffset(0),
                    rhs: ir::Scalar::ConstInt(4)
                }),
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

    #[test]
    fn test_member_2() {
        let (tu, ec) = compile(
            "struct X { long a; int b; }; void foo(void) { struct X *x; int y; y = x->b; }",
        );
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(2),
                    width: ir::Width::PTR_WIDTH,
                    sign: false,
                    lhs: ir::Scalar::Var(VarLocation::Local(0)),
                    rhs: ir::Scalar::ConstInt(4)
                }),
                ir::Op::Load(ir::LoadOp {
                    dst: VarLocation::Local(3),
                    src_addr: ir::Scalar::Var(VarLocation::Local(2)),
                    width: ir::Width::Word,
                }),
                ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: VarLocation::Local(1),
                    src: ir::Scalar::Var(VarLocation::Local(3)),
                    width: ir::Width::Word
                })
            ]
        );
    }

    #[test]
    fn test_member_3() {
        let (tu, ec) = compile("struct X { long a; }; struct Y { int i; struct X x; }; void foo(void) { struct X x; struct Y y; x = y.x; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(0),
                    width: ir::Width::PTR_WIDTH,
                    sign: false,
                    lhs: ir::Scalar::FrameOffset(4),
                    rhs: ir::Scalar::ConstInt(4)
                }),
                ir::Op::Memcpy(ir::MemcpyOp {
                    dst_addr: ir::Scalar::FrameOffset(0),
                    src_addr: ir::Scalar::Var(VarLocation::Local(0)),
                    len: 4
                })
            ]
        );
    }

    #[test]
    fn test_member_4() {
        let (tu, ec) = compile("struct X { long a; }; struct Y { int i; struct X x; }; void foo(void) { struct X x; struct Y *y; x = y->x; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(1),
                    width: ir::Width::PTR_WIDTH,
                    sign: false,
                    lhs: ir::Scalar::Var(VarLocation::Local(0)),
                    rhs: ir::Scalar::ConstInt(4)
                }),
                ir::Op::Memcpy(ir::MemcpyOp {
                    dst_addr: ir::Scalar::FrameOffset(0),
                    src_addr: ir::Scalar::Var(VarLocation::Local(1)),
                    len: 4
                })
            ]
        );
    }
}
