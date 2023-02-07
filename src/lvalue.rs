use lang_c::ast::Expression;
use lang_c::span::{Node, Span};

use crate::block_emitter::BlockEmitter;
use crate::compile::{self, compile_expression, TypedSrc};
use crate::ctype::QualifiedType;
use crate::error::{CompileError, CompileWarning, ErrorCollector};
use crate::ir::VarLocation;
use crate::ir::{self, Src};
use crate::machine;
use crate::name_scope::NameScope;

pub enum LValue {
    Var(VarLocation),
    Indirection(Src),
    // member access TODO
}

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
        let (t, v) = scope.get_var(name, span, ec)?;
        Ok(TypedLValue {
            t: t.clone(),
            lv: LValue::Var(v),
        })
    }

    pub fn new_compile(
        expr: Node<Expression>,
        scope: &mut NameScope,
        be: &mut BlockEmitter,
        ec: &mut ErrorCollector,
    ) -> Result<TypedLValue, ()> {
        match expr.node {
            Expression::Identifier(id) => {
                let (t, v) = scope.get_var(&id.node.name, id.span, ec)?;
                Ok(TypedLValue {
                    t: t.clone(),
                    lv: LValue::Var(v),
                })
            }
            Expression::UnaryOperator(op) => {
                use lang_c::ast::UnaryOperator;
                match op.node.operator.node {
                    UnaryOperator::Indirection => {
                        let e = compile_expression(*op.node.operand, scope, be, ec)?;
                        match e.t.dereference() {
                            Ok(t) => Ok(TypedLValue {
                                t,
                                lv: LValue::Indirection(e.src),
                            }),
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
                        let lhs_span = op.node.lhs.span;
                        let rhs_span = op.node.rhs.span;
                        let array = compile_expression(*op.node.lhs, scope, be, ec)?;
                        let element_type = if let Ok(t) = array.t.clone().dereference() {
                            t
                        } else {
                            ec.record_error(CompileError::BadSubscripted, lhs_span)?;
                            unreachable!();
                        };
                        let element_size = element_type.t.sizeof(rhs_span, ec)?;
                        let index = compile_expression(*op.node.rhs, scope, be, ec)?;
                        if !index.t.t.is_integer() {
                            ec.record_error(CompileError::BadSubsript, rhs_span)?;
                            unreachable!();
                        }
                        let index = compile::int_promote(index, scope, be);
                        let (width, sign) = {
                            let (width, sign) = index.t.t.get_width_sign().unwrap();
                            if width as u8 > machine::PTR_SIZE {
                                ec.record_warning(
                                    CompileWarning::IndexTooWide(index.t.clone()),
                                    rhs_span,
                                )?;
                                (ir::Width::new(machine::PTR_SIZE), sign)
                            } else {
                                (width, sign)
                            }
                        };
                        let mul_dst = scope.alloc_temp();
                        be.append_operation(ir::Op::Mul(ir::BinaryOp {
                            dst: mul_dst.clone(),
                            lhs: index.src,
                            rhs: ir::Src::ConstInt(element_size.into()),
                            width,
                            sign,
                        }));
                        let sum_dst = scope.alloc_temp();
                        be.append_operation(ir::Op::Add(ir::BinaryOp {
                            dst: sum_dst.clone(),
                            lhs: array.src,
                            rhs: ir::Src::Var(mul_dst),
                            width: ir::Width::new(machine::PTR_SIZE),
                            sign: false,
                        }));
                        Ok(TypedLValue {
                            t: element_type,
                            lv: LValue::Indirection(ir::Src::Var(sum_dst)),
                        })
                    }
                    _ => {
                        ec.record_error(CompileError::NotAssignable, expr.span)?;
                        unreachable!()
                    }
                }
            }
            Expression::Member(_) => todo!(),
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
    ) -> Result<TypedSrc, ()> {
        match self.lv {
            LValue::Var(v) => Ok(TypedSrc {
                t: self.t,
                src: ir::Src::Var(v),
            }),
            LValue::Indirection(src_addr) => {
                if let Some((width, _)) = self.t.t.get_width_sign() {
                    let dst = scope.alloc_temp();
                    be.append_operation(ir::Op::Load(ir::LoadOp {
                        dst: dst.clone(),
                        src_addr,
                        width,
                    }));
                    Ok(TypedSrc {
                        src: ir::Src::Var(dst),
                        t: self.t,
                    })
                } else {
                    unreachable!("this function is not supposed to be used for large types")
                }
            }
        }
    }
}
