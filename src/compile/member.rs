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
    if field_type.t.is_scalar_or_array() {
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
