use lang_c::span::Span;
use lang_c::{ast::Expression, span::Node};

use crate::ctype::CType;
use crate::error::CompileError;
use crate::lvalue::TypedLValue;
use crate::object_location::ObjectLocation;
use crate::rvalue::{RValue, TypedRValue};
use crate::{
    block_emitter::BlockEmitter,
    error::{CompileWarning, ErrorCollector},
    ir,
    name_scope::NameScope,
};

use super::{cast, compile_expression};

/**
 * Assignment according to 6.5.16.1
 */
pub fn compile_assign(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
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
    rhs: (TypedRValue, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
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
                    src: rhs_casted.clone().unwrap_scalar(),
                    width,
                }));
            }
            LValue::Indirection(addr) => {
                be.append_operation(ir::Op::Store(ir::StoreOp {
                    dst_addr: addr,
                    src: rhs_casted.clone().unwrap_scalar(),
                    width,
                }));
            }
            LValue::Object(_) => unreachable!("arithmetic types are scalars"),
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
            CType::Void | CType::Float(_) | CType::Function { .. } => {
                ec.record_error(
                    CompileError::IncompatibleTypes(lhs_lval.t, rhs_val.t),
                    rhs_span,
                )?;
                unreachable!()
            }
            CType::StructUnion(_) => {
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
            CType::Bool | CType::Int(_, _) => {
                ec.record_warning(
                    CompileWarning::IncompatibleTypes(lhs_lval.t.clone(), rhs_val.t.clone()),
                    rhs_span,
                )?;
            }
            CType::Enum(_id) => {
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
                    src: rhs_casted.clone().unwrap_scalar(),
                    width,
                }));
            }
            LValue::Indirection(addr) => {
                be.append_operation(ir::Op::Store(ir::StoreOp {
                    dst_addr: addr,
                    src: rhs_casted.clone().unwrap_scalar(),
                    width,
                }));
            }
            LValue::Object(_) => unreachable!("pointer types are scalars"),
        }
        Ok(rhs_casted)
    } else if lhs_lval.t.is_compatible_to(&rhs_val.t, true) {
        if lhs_lval.t.t.is_object() {
            // the left operand has an atomic, qualified, or unqualified version of a structure or union type
            // compatible with the type of the right;
            let t = lhs_lval.t.clone();
            let len = rhs_val.t.t.sizeof(scope, rhs_span, ec)?;
            let dst_addr = lhs_lval.get_object_address().unwrap();
            let src_addr = rhs_val.src.unwrap_object_location().get_address();
            be.append_operation(ir::Op::Memcpy(ir::MemcpyOp {
                dst_addr: dst_addr.clone(),
                src_addr,
                len,
            }));
            Ok(TypedRValue {
                t,
                src: RValue::new_object(ObjectLocation::PointedBy(dst_addr)),
            })
        } else {
            ec.record_error(CompileError::NotAssignable, rhs_span)?;
            unreachable!();
        }
    } else {
        ec.record_error(
            CompileError::IncompatibleTypes(lhs_lval.t, rhs_val.t),
            rhs_span,
        )?;
        unreachable!();
    }
}
