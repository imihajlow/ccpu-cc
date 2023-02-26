use lang_c::ast::{MemberExpression, MemberOperator};
use lang_c::span::Node;

use crate::block_emitter::BlockEmitter;
use crate::name_scope::NameScope;
use crate::object_location::ObjectLocation;
use crate::rvalue::{RValue, TypedRValue};
use crate::{ir, machine, ErrorCollector};

use super::compile_expression;

pub fn compile_member_expression(
    expr: Node<MemberExpression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let id = expr.node.identifier.node.name;
    let id_span = expr.node.identifier.span;
    match expr.node.operator.node {
        MemberOperator::Direct => {
            let lhs = compile_expression(*expr.node.expression, scope, be, ec)?;
            let (field_offset, mut field_type) = lhs.t.get_field(&id, scope, id_span, ec)?;
            field_type.qualifiers |= lhs.t.qualifiers;
            let obj_addr = lhs.get_object_address().unwrap();
            let addr_var = scope.alloc_temp();
            be.append_operation(ir::Op::Add(ir::BinaryOp {
                dst: addr_var.clone(),
                width: ir::Width::new(machine::PTR_SIZE),
                sign: false,
                lhs: obj_addr,
                rhs: ir::Scalar::ConstInt(field_offset as u64),
            }));
            if field_type.t.is_scalar() {
                let target_var = scope.alloc_temp();
                let width = field_type.t.get_scalar_width().unwrap();
                be.append_operation(ir::Op::Load(ir::LoadOp {
                    dst: target_var.clone(),
                    src_addr: ir::Scalar::Var(addr_var),
                    width,
                }));
                Ok(TypedRValue {
                    t: field_type,
                    src: RValue::new_var(target_var),
                })
            } else if field_type.t.is_object() {
                Ok(TypedRValue {
                    t: field_type,
                    src: RValue::new_object(ObjectLocation::PointedBy(ir::Scalar::Var(addr_var))),
                })
            } else {
                todo!()
            }
        }
        MemberOperator::Indirect => {
            let lhs = compile_expression(*expr.node.expression, scope, be, ec)?;
            todo!()
        }
    }
}
