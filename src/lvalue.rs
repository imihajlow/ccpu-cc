use lang_c::ast::{Expression, MemberExpression, MemberOperator};
use lang_c::span::{Node, Span};

use crate::block_emitter::BlockEmitter;
use crate::compile::{compile_expression, compile_pointer_offset};
use crate::ctype::QualifiedType;
use crate::error::{CompileError, ErrorCollector};
use crate::ir::VarLocation;
use crate::ir::{self, Scalar};
use crate::name_scope::NameScope;
use crate::object_location::ObjectLocation;
use crate::rvalue::{RValue, TypedRValue};

#[derive(Debug, Clone)]
pub enum LValue {
    Var(VarLocation),
    Indirection(Scalar),
    Object(ObjectLocation),
}

#[derive(Debug, Clone)]
pub struct TypedLValue {
    pub t: QualifiedType,
    pub lv: LValue,
}

impl TypedLValue {
    pub fn new_from_name(
        name: &str,
        span: Span,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<Self, ()> {
        scope.get_lvalue(name, span, ec)
    }

    pub fn new_compile(
        expr: Node<Expression>,
        scope: &mut NameScope,
        be: &mut BlockEmitter,
        ec: &mut ErrorCollector,
    ) -> Result<TypedLValue, ()> {
        match expr.node {
            Expression::Identifier(id) => scope.get_lvalue(&id.node.name, id.span, ec),
            Expression::UnaryOperator(op) => {
                use lang_c::ast::UnaryOperator;
                match op.node.operator.node {
                    UnaryOperator::Indirection => {
                        let e = compile_expression(*op.node.operand, scope, be, ec)?;
                        match e.t.dereference() {
                            Ok(t) => {
                                let e_scalar = e.src.unwrap_scalar();
                                Ok(TypedLValue {
                                    t,
                                    lv: LValue::Indirection(e_scalar),
                                })
                            }
                            Err(t) => {
                                ec.record_error(CompileError::BadIndirection(t), expr.span)?;
                                unreachable!();
                            }
                        }
                    }
                    _ => {
                        ec.record_error(CompileError::NotAssignable, expr.span)?;
                        unreachable!()
                    }
                }
            }
            Expression::BinaryOperator(op) => {
                use lang_c::ast::BinaryOperator;
                match op.node.operator.node {
                    BinaryOperator::Index => {
                        let array_span = op.node.lhs.span;
                        let index_span = op.node.rhs.span;
                        let array = compile_expression(*op.node.lhs, scope, be, ec)?;
                        let index = compile_expression(*op.node.rhs, scope, be, ec)?;

                        let offset = compile_pointer_offset(
                            (array, array_span),
                            (index, index_span),
                            false,
                            scope,
                            be,
                            ec,
                        )?;

                        let (offset_scalar, offset_type) = offset.unwrap_scalar_and_type();

                        Ok(TypedLValue {
                            t: offset_type.dereference().unwrap(),
                            lv: LValue::Indirection(offset_scalar),
                        })
                    }
                    _ => {
                        ec.record_error(CompileError::NotAssignable, expr.span)?;
                        unreachable!()
                    }
                }
            }
            Expression::Member(me) => Self::compile_member_expression(*me, scope, be, ec),
            Expression::GenericSelection(_) => unimplemented!(),
            _ => {
                ec.record_error(CompileError::NotAssignable, expr.span)?;
                unimplemented!()
            }
        }
    }

    pub fn compile_into_rvalue(
        self,
        scope: &mut NameScope,
        be: &mut BlockEmitter,
    ) -> Result<TypedRValue, ()> {
        match self.lv {
            LValue::Var(v) => Ok(TypedRValue {
                t: self.t,
                src: RValue::new_var(v),
            }),
            LValue::Indirection(src_addr) => {
                if let Some((width, _)) = self.t.t.get_width_sign() {
                    let dst = scope.alloc_temp();
                    be.append_operation(ir::Op::Load(ir::LoadOp {
                        dst: dst.clone(),
                        src_addr,
                        width,
                    }));
                    Ok(TypedRValue {
                        src: RValue::new_var(dst),
                        t: self.t,
                    })
                } else {
                    assert!(!self.t.t.is_scalar());
                    todo!()
                }
            }
            LValue::Object(s) => Ok(TypedRValue {
                src: RValue::new_object(s),
                t: self.t,
            }),
        }
    }

    pub fn get_object_address(self) -> Option<Scalar> {
        match self.lv {
            LValue::Var(_) => None,
            LValue::Indirection(p) => Some(p),
            LValue::Object(l) => Some(l.get_address()),
        }
    }

    pub fn get_var(self) -> Option<VarLocation> {
        if let LValue::Var(v) = self.lv {
            Some(v)
        } else {
            None
        }
    }

    fn compile_member_expression(
        expr: Node<MemberExpression>,
        scope: &mut NameScope,
        be: &mut BlockEmitter,
        ec: &mut ErrorCollector,
    ) -> Result<Self, ()> {
        let id = expr.node.identifier.node.name;
        let id_span = expr.node.identifier.span;
        match expr.node.operator.node {
            MemberOperator::Direct => {
                let lhs = TypedLValue::new_compile(*expr.node.expression, scope, be, ec)?;
                let (field_offset, mut field_type) = lhs.t.get_field(&id, scope, id_span, ec)?;
                field_type.qualifiers |= lhs.t.qualifiers;
                let obj_addr = lhs.get_object_address().unwrap();
                let target = scope.alloc_temp();
                be.append_operation(ir::Op::Add(ir::BinaryOp {
                    dst: target.clone(),
                    width: ir::Width::PTR_WIDTH,
                    sign: false,
                    lhs: obj_addr,
                    rhs: ir::Scalar::ConstInt(field_offset as u64),
                }));
                Ok(TypedLValue {
                    t: field_type,
                    lv: LValue::Indirection(ir::Scalar::Var(target)),
                })
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
                let src_addr = lhs.src.unwrap_scalar();
                let (field_offset, mut field_type) = obj_type.get_field(&id, scope, id_span, ec)?;
                field_type.qualifiers |= obj_type.qualifiers;
                let addr_var = scope.alloc_temp();
                be.append_operation(ir::Op::Add(ir::BinaryOp {
                    dst: addr_var.clone(),
                    lhs: src_addr,
                    rhs: ir::Scalar::ConstInt(field_offset as u64),
                    width: ir::Width::PTR_WIDTH,
                    sign: false,
                }));
                Ok(TypedLValue {
                    t: field_type,
                    lv: LValue::Indirection(ir::Scalar::Var(addr_var)),
                })
            }
        }
    }
}
