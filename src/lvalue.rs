use lang_c::ast::Expression;
use lang_c::span::{Node, Span};

use crate::block_emitter::BlockEmitter;
use crate::compile::{self, compile_expression, compile_pointer_offset, int_promote};
use crate::ctype::QualifiedType;
use crate::error::{CompileError, CompileWarning, ErrorCollector};
use crate::ir::VarLocation;
use crate::ir::{self, Scalar};
use crate::machine;
use crate::name_scope::NameScope;
use crate::rvalue::{RValue, TypedRValue};

#[derive(Debug, Clone)]
pub enum LValue {
    Var(VarLocation),
    Indirection(Scalar),
    // member access TODO
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
                        let (e_scalar, e_type) = e.unwrap_scalar_and_type();
                        match e_type.dereference() {
                            Ok(t) => Ok(TypedLValue {
                                t,
                                lv: LValue::Indirection(e_scalar),
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
                    unreachable!("this function is not supposed to be used for large types")
                }
            }
        }
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

    fn get_first_body(tu: &TranslationUnit) -> &Vec<LabeledBlock> {
        tu.functions.first().unwrap().get_body()
    }

    #[test]
    fn test_lvalue_1() {
        let (tu, ec) = compile("void foo(void) { int *x, y; *x = y; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![ir::Op::Store(ir::StoreOp {
                dst_addr: ir::Scalar::Var(VarLocation::Local(0)),
                src: ir::Scalar::Var(VarLocation::Local(1)),
                width: ir::Width::Word
            })]
        );
    }

    #[test]
    fn test_lvalue_2() {
        let (tu, ec) = compile("void foo(void) { int *x, y, z; x[y] = z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Mul(ir::BinaryOp {
                    dst: VarLocation::Local(3),
                    lhs: ir::Scalar::Var(VarLocation::Local(1)),
                    rhs: ir::Scalar::ConstInt(2),
                    width: ir::Width::Word,
                    sign: true,
                }),
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(4),
                    lhs: ir::Scalar::Var(VarLocation::Local(0)),
                    rhs: ir::Scalar::Var(VarLocation::Local(3)),
                    width: ir::Width::Word,
                    sign: false,
                }),
                ir::Op::Store(ir::StoreOp {
                    dst_addr: ir::Scalar::Var(VarLocation::Local(4)),
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    width: ir::Width::Word
                })
            ]
        );
    }

    #[test]
    fn test_lvalue_3() {
        let (tu, ec) = compile("void foo(void) { int *x; char y; int z; x[y] = z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0].ops,
            vec![
                ir::Op::Conv(ir::ConvOp {
                    dst: VarLocation::Local(3),
                    dst_width: ir::Width::Word,
                    dst_sign: true,
                    src: ir::Scalar::Var(VarLocation::Local(1)),
                    src_width: ir::Width::Byte,
                    src_sign: false,
                }),
                ir::Op::Mul(ir::BinaryOp {
                    dst: VarLocation::Local(4),
                    lhs: ir::Scalar::Var(VarLocation::Local(3)),
                    rhs: ir::Scalar::ConstInt(2),
                    width: ir::Width::Word,
                    sign: true,
                }),
                ir::Op::Add(ir::BinaryOp {
                    dst: VarLocation::Local(5),
                    lhs: ir::Scalar::Var(VarLocation::Local(0)),
                    rhs: ir::Scalar::Var(VarLocation::Local(4)),
                    width: ir::Width::Word,
                    sign: false,
                }),
                ir::Op::Store(ir::StoreOp {
                    dst_addr: ir::Scalar::Var(VarLocation::Local(5)),
                    src: ir::Scalar::Var(VarLocation::Local(2)),
                    width: ir::Width::Word
                })
            ]
        );
    }
}
