use lang_c::span::Span;
use lang_c::{ast::Expression, span::Node};

use crate::block_emitter::BlockEmitter;
use crate::error::{CompileError, ErrorCollector};
use crate::ir;
use crate::name_scope::NameScope;
use crate::object_location::ObjectLocation;
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
    if pointee.t.is_scalar() {
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
    } else if pointee.t.is_array() {
        todo!()
    } else {
        assert!(pointee.t.is_object());
        let location = offset.unwrap_scalar();
        Ok(TypedRValue {
            src: RValue::Object(ObjectLocation::PointedBy(location)),
            t: pointee,
        })
    }
}
