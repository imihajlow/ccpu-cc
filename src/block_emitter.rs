use std::{collections::HashMap, fmt::Formatter, mem};

use lang_c::{
    ast::{
        ConditionalExpression, DoWhileStatement, Expression, ForStatement, IfStatement,
        WhileStatement,
    },
    span::{Node, Span},
};

use crate::{
    compile::{
        self, cast, compile_declaration, compile_expression, compile_statement, convert_to_bool,
        integer_promote,
    },
    constant::check_static_assert,
    ctype::{self, QualifiedType, Qualifiers},
    error::{CompileError, ErrorCollector},
    ir,
    name_scope::NameScope,
    rvalue::{RValue, TypedRValue},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LabeledTail {
    // TODO remove pub
    Tail(ir::Tail),
    GotoLabel(String),
}

pub type LabeledBlock = ir::GenericBlock<LabeledTail>; // TODO remove pub

#[derive(Clone)]
pub struct BlockEmitter {
    next_id: usize,
    current_id: usize,
    blocks: HashMap<usize, LabeledBlock>,
    current_ops: Vec<ir::Op>,
    breaks: Vec<usize>,
    continues: Vec<usize>,
}

impl BlockEmitter {
    pub fn new() -> Self {
        Self {
            next_id: 1,
            current_id: 0,
            blocks: HashMap::new(),
            current_ops: Vec::new(),
            breaks: Vec::new(),
            continues: Vec::new(),
        }
    }

    pub fn finalize(self) -> Vec<LabeledBlock> {
        let mut blocks = self.blocks;
        blocks.insert(
            self.current_id,
            LabeledBlock {
                phi: Vec::new(),
                ops: self.current_ops,
                tail: LabeledTail::Tail(ir::Tail::Ret),
            },
        );
        let mut result = Vec::new();
        for i in 0..self.next_id {
            result.push(blocks.remove(&i).unwrap());
        }
        result
    }

    pub fn append_operation(&mut self, op: ir::Op) {
        self.current_ops.push(op);
    }

    pub fn append_if(
        &mut self,
        ifs: Node<IfStatement>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let then_block_id = self.alloc_block_id();
        let cont_block_id = self.alloc_block_id();
        let else_block_id = if ifs.node.else_statement.is_some() {
            self.alloc_block_id()
        } else {
            cont_block_id
        };

        let cond_span = ifs.node.condition.span;
        let cond = compile_expression(*ifs.node.condition, scope, self, ec)?;
        let cond_bool = convert_to_bool(cond, cond_span, scope, self, ec)?;

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Cond(
                cond_bool.unwrap_scalar(),
                then_block_id,
                else_block_id,
            )),
            then_block_id,
        );

        scope.push();
        compile_statement(*ifs.node.then_statement, scope, self, ec)?;
        scope.pop_and_collect_initializers();

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
            else_block_id,
        );

        if let Some(elses) = ifs.node.else_statement {
            scope.push();
            compile_statement(*elses, scope, self, ec)?;
            scope.pop_and_collect_initializers();

            self.finish_block(
                LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
                cont_block_id,
            )
        }

        Ok(())
    }

    pub fn append_logical_and(
        &mut self,
        lhs: Node<Expression>,
        rhs: Node<Expression>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<TypedRValue, ()> {
        let lhs_span = lhs.span;
        let rhs_span = rhs.span;
        let result_var = scope.alloc_temp();
        let shortcut_block_id = self.alloc_block_id();
        let rhs_block_id = self.alloc_block_id();
        let cont_block_id = self.alloc_block_id();
        let lhs = compile_expression(lhs, scope, self, ec)?;
        let lhs_bool = convert_to_bool(lhs, lhs_span, scope, self, ec)?;

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Cond(
                lhs_bool.unwrap_scalar(),
                rhs_block_id,
                shortcut_block_id,
            )),
            rhs_block_id,
        );

        let rhs = compile_expression(rhs, scope, self, ec)?;
        let rhs_bool = convert_to_bool(rhs, rhs_span, scope, self, ec)?;
        let rhs_int = compile::cast(rhs_bool, &ctype::INT_TYPE, true, rhs_span, scope, self, ec)?;
        let width = rhs_int.t.t.get_scalar_width().unwrap();
        self.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: result_var.clone(),
            src: rhs_int.unwrap_scalar(),
            width,
        }));

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
            shortcut_block_id,
        );

        self.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: result_var.clone(),
            src: ir::Scalar::ConstInt(0),
            width,
        }));

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
            cont_block_id,
        );

        Ok(TypedRValue {
            src: RValue::new_var(result_var),
            t: QualifiedType {
                t: ctype::INT_TYPE,
                qualifiers: Qualifiers::empty(),
            },
        })
    }

    pub fn append_logical_or(
        &mut self,
        lhs: Node<Expression>,
        rhs: Node<Expression>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<TypedRValue, ()> {
        let lhs_span = lhs.span;
        let rhs_span = rhs.span;
        let result_var = scope.alloc_temp();
        let shortcut_block_id = self.alloc_block_id();
        let rhs_block_id = self.alloc_block_id();
        let cont_block_id = self.alloc_block_id();
        let lhs = compile_expression(lhs, scope, self, ec)?;
        let lhs_bool = convert_to_bool(lhs, lhs_span, scope, self, ec)?;

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Cond(
                lhs_bool.unwrap_scalar(),
                shortcut_block_id,
                rhs_block_id,
            )),
            rhs_block_id,
        );

        let rhs = compile_expression(rhs, scope, self, ec)?;
        let rhs_bool = convert_to_bool(rhs, rhs_span, scope, self, ec)?;
        let rhs_int = compile::cast(rhs_bool, &ctype::INT_TYPE, true, rhs_span, scope, self, ec)?;
        let width = rhs_int.t.t.get_scalar_width().unwrap();
        self.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: result_var.clone(),
            src: rhs_int.unwrap_scalar(),
            width,
        }));

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
            shortcut_block_id,
        );

        self.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: result_var.clone(),
            src: ir::Scalar::ConstInt(1),
            width,
        }));

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
            cont_block_id,
        );

        Ok(TypedRValue {
            src: RValue::new_var(result_var),
            t: QualifiedType {
                t: ctype::INT_TYPE,
                qualifiers: Qualifiers::empty(),
            },
        })
    }

    pub fn append_conditional(
        &mut self,
        c: Node<ConditionalExpression>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<TypedRValue, ()> {
        let cond_span = c.node.condition.span;
        let then_span = c.node.then_expression.span;
        let else_span = c.node.else_expression.span;
        let cond = compile_expression(*c.node.condition, scope, self, ec)?;
        let cond_width = cond.t.t.get_scalar_width().ok_or_else(|| {
            ec.record_error(CompileError::ScalarTypeRequired, cond_span)
                .ok();
        })?;
        let cond_dst = scope.alloc_temp();
        self.append_operation(ir::Op::Bool(ir::UnaryUnsignedOp {
            dst: cond_dst.clone(),
            src: cond.unwrap_scalar(),
            width: cond_width,
        }));

        let then_block = self.alloc_block_id();
        let else_block = self.alloc_block_id();
        let cont_block = self.alloc_block_id();

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Cond(
                ir::Scalar::Var(cond_dst),
                then_block,
                else_block,
            )),
            then_block,
        );

        let (then_type, else_type) = {
            // alas, compile twice
            let mut temp_scope = scope.clone();
            let mut temp_be = self.clone();

            let then_expr = compile_expression(
                (*c.node.then_expression).clone(),
                &mut temp_scope,
                &mut temp_be,
                ec,
            )?;
            let else_expr = compile_expression(
                (*c.node.else_expression).clone(),
                &mut temp_scope,
                &mut temp_be,
                ec,
            )?;
            (then_expr.t, else_expr.t)
        };

        if then_type.t.is_void() && else_type.t.is_void() {
            compile_expression(*c.node.then_expression, scope, self, ec)?;

            self.finish_block(LabeledTail::Tail(ir::Tail::Jump(cont_block)), else_block);

            compile_expression(*c.node.else_expression, scope, self, ec)?;

            self.finish_block(LabeledTail::Tail(ir::Tail::Jump(cont_block)), cont_block);

            Ok(TypedRValue {
                src: RValue::new_void(),
                t: QualifiedType {
                    t: ctype::CType::Void,
                    qualifiers: Qualifiers::empty(),
                },
            })
        } else if then_type.t.is_arithmetic() && else_type.t.is_arithmetic() {
            let result_dst = scope.alloc_temp();

            if then_type.t.is_integer() && else_type.t.is_integer() {
                let common_type = {
                    let then_type_promoted = then_type.t.clone().promote();
                    let else_type_promoted = else_type.t.clone().promote();
                    then_type_promoted.least_common_int_type(&else_type_promoted)
                };
                let width = common_type.get_scalar_width().unwrap();

                let then_src = compile_expression(*c.node.then_expression, scope, self, ec)?;
                let then_promoted = integer_promote((then_src, then_span), scope, self, ec)?;
                let then_casted = cast(
                    then_promoted,
                    &common_type,
                    false,
                    then_span,
                    scope,
                    self,
                    ec,
                )?;
                self.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: result_dst.clone(),
                    src: then_casted.unwrap_scalar(),
                    width,
                }));

                self.finish_block(LabeledTail::Tail(ir::Tail::Jump(cont_block)), else_block);

                let else_src = compile_expression(*c.node.else_expression, scope, self, ec)?;
                let else_promoted = integer_promote((else_src, else_span), scope, self, ec)?;
                let else_casted = cast(
                    else_promoted,
                    &common_type,
                    false,
                    else_span,
                    scope,
                    self,
                    ec,
                )?;
                self.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: result_dst.clone(),
                    src: else_casted.unwrap_scalar(),
                    width,
                }));

                self.finish_block(LabeledTail::Tail(ir::Tail::Jump(cont_block)), cont_block);
                Ok(TypedRValue {
                    src: RValue::new_var(result_dst),
                    t: QualifiedType {
                        t: common_type,
                        qualifiers: Qualifiers::empty(),
                    },
                })
            } else {
                todo!("float")
            }
        } else if then_type.t.is_dereferencable() && else_type.t.is_dereferencable() {
            let then_pointee = then_type.clone().dereference().unwrap();
            let else_pointee = else_type.clone().dereference().unwrap();
            let common_type = if !then_pointee.t.is_void()
                && !else_pointee.t.is_void()
                && then_pointee.is_compatible_to(&else_pointee, false)
            {
                then_type
            } else if then_pointee.t.is_void() {
                else_type
            } else if else_pointee.t.is_void() {
                then_type
            } else {
                ec.record_error(
                    CompileError::IncompatibleTypes(then_type, else_type),
                    else_span,
                )?;
                unreachable!()
            };
            let result_dst = scope.alloc_temp();
            let width = common_type.t.get_scalar_width().unwrap();

            let then_src = compile_expression(*c.node.then_expression, scope, self, ec)?;
            self.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: result_dst.clone(),
                src: then_src.unwrap_scalar(),
                width,
            }));

            self.finish_block(LabeledTail::Tail(ir::Tail::Jump(cont_block)), else_block);

            let else_src = compile_expression(*c.node.else_expression, scope, self, ec)?;
            self.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: result_dst.clone(),
                src: else_src.unwrap_scalar(),
                width,
            }));

            self.finish_block(LabeledTail::Tail(ir::Tail::Jump(cont_block)), cont_block);
            Ok(TypedRValue {
                src: RValue::new_var(result_dst),
                t: common_type,
            })
        } else if then_type.t.is_same_struct_union(&else_type.t) {
            todo!()
        } else {
            ec.record_error(
                CompileError::IncompatibleTypes(then_type, else_type),
                else_span,
            )?;
            unreachable!();
        }
    }

    pub fn append_while(
        &mut self,
        whiles: Node<WhileStatement>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let condition_block_id = self.alloc_block_id();
        let body_block_id = self.alloc_block_id();
        let continue_block_id = self.alloc_block_id();

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(condition_block_id)),
            condition_block_id,
        );

        let cond_span = whiles.node.expression.span;
        let cond = compile_expression(*whiles.node.expression, scope, self, ec)?;
        let cond_bool = convert_to_bool(cond, cond_span, scope, self, ec)?;

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Cond(
                cond_bool.unwrap_scalar(),
                body_block_id,
                continue_block_id,
            )),
            body_block_id,
        );

        scope.push();
        self.set_break(continue_block_id);
        self.set_continue(condition_block_id);
        compile_statement(*whiles.node.statement, scope, self, ec)?;
        self.pop_continue();
        self.pop_break();
        scope.pop_and_collect_initializers();

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(condition_block_id)),
            continue_block_id,
        );

        Ok(())
    }

    pub fn append_do_while(
        &mut self,
        whiles: Node<DoWhileStatement>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let body_block_id = self.alloc_block_id();
        let condition_block_id = self.alloc_block_id();
        let continue_block_id = self.alloc_block_id();

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(body_block_id)),
            body_block_id,
        );

        scope.push();
        self.set_break(continue_block_id);
        self.set_continue(condition_block_id);
        compile_statement(*whiles.node.statement, scope, self, ec)?;
        self.pop_continue();
        self.pop_break();
        scope.pop_and_collect_initializers();

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(condition_block_id)),
            condition_block_id,
        );

        let cond_span = whiles.node.expression.span;
        let cond = compile_expression(*whiles.node.expression, scope, self, ec)?;
        let cond_bool = convert_to_bool(cond, cond_span, scope, self, ec)?;

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Cond(
                cond_bool.unwrap_scalar(),
                body_block_id,
                continue_block_id,
            )),
            continue_block_id,
        );
        Ok(())
    }

    pub fn append_for(
        &mut self,
        fors: Node<ForStatement>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        use lang_c::ast::ForInitializer;
        let body_block_id = self.alloc_block_id();
        let condition_block_id = if fors.node.condition.is_some() {
            self.alloc_block_id()
        } else {
            body_block_id
        };
        let continue_block_id = self.alloc_block_id();

        scope.push();
        match fors.node.initializer.node {
            ForInitializer::Empty => (),
            ForInitializer::Expression(e) => {
                compile_expression(*e, scope, self, ec)?;
            }
            ForInitializer::Declaration(decl) => {
                compile_declaration(decl, scope, self, ec)?;
            }
            ForInitializer::StaticAssert(sa) => {
                check_static_assert(sa, scope, ec)?;
            }
        }

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(condition_block_id)),
            condition_block_id,
        );

        if let Some(cond) = fors.node.condition {
            let cond_span = cond.span;
            let cond = compile_expression(*cond, scope, self, ec)?;
            let cond_bool = convert_to_bool(cond, cond_span, scope, self, ec)?;

            self.finish_block(
                LabeledTail::Tail(ir::Tail::Cond(
                    cond_bool.unwrap_scalar(),
                    body_block_id,
                    continue_block_id,
                )),
                body_block_id,
            );
        }

        scope.push();
        self.set_break(continue_block_id);
        self.set_continue(condition_block_id);
        compile_statement(*fors.node.statement, scope, self, ec)?;
        scope.pop_and_collect_initializers();
        self.pop_continue();
        self.pop_break();
        if let Some(step) = fors.node.step {
            compile_expression(*step, scope, self, ec)?;
        }

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(condition_block_id)),
            continue_block_id,
        );

        scope.pop_and_collect_initializers();
        Ok(())
    }

    pub fn append_break(&mut self, span: Span, ec: &mut ErrorCollector) -> Result<(), ()> {
        let break_block_id = if let Some(id) = self.breaks.last() {
            *id
        } else {
            return ec.record_error(CompileError::InvalidBreak, span);
        };
        let orphan_block = self.alloc_block_id();
        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(break_block_id)),
            orphan_block,
        );
        Ok(())
    }

    pub fn append_continue(&mut self, span: Span, ec: &mut ErrorCollector) -> Result<(), ()> {
        let continue_block_id = if let Some(id) = self.continues.last() {
            *id
        } else {
            return ec.record_error(CompileError::InvalidBreak, span);
        };
        let orphan_block = self.alloc_block_id();
        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(continue_block_id)),
            orphan_block,
        );
        Ok(())
    }

    fn finish_block(&mut self, tail: LabeledTail, next_block: usize) {
        let current_ops = mem::replace(&mut self.current_ops, Vec::new());
        self.blocks.insert(
            self.current_id,
            LabeledBlock {
                ops: current_ops,
                phi: Vec::new(),
                tail,
            },
        );
        self.current_id = next_block;
    }

    fn alloc_block_id(&mut self) -> usize {
        let r = self.next_id;
        self.next_id += 1;
        r
    }

    fn set_break(&mut self, block_id: usize) {
        self.breaks.push(block_id);
    }

    fn set_continue(&mut self, block_id: usize) {
        self.continues.push(block_id);
    }

    fn pop_break(&mut self) {
        self.breaks.pop().expect("break stack underflow");
    }

    fn pop_continue(&mut self) {
        self.continues.pop().expect("continue stack underflow");
    }
}

impl std::fmt::Display for LabeledTail {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LabeledTail::Tail(t) => write!(f, "{}", t),
            LabeledTail::GotoLabel(l) => write!(f, "jmp {}", l),
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
    fn test_ternary_1() {
        let (tu, ec) = compile("void foo(void) { int x, y, z; x = x ? y : z; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 4);
        assert_eq!(
            body[0].ops,
            vec![ir::Op::Bool(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(3),
                src: ir::Scalar::Var(VarLocation::Local(0)),
                width: ir::Width::Word
            })]
        );
        assert_eq!(
            body[0].tail,
            LabeledTail::Tail(ir::Tail::Cond(ir::Scalar::Var(VarLocation::Local(3)), 1, 2))
        );
        assert_eq!(
            body[1].ops,
            vec![ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(4),
                src: ir::Scalar::Var(VarLocation::Local(1)),
                width: ir::Width::Word
            })]
        );
        assert_eq!(body[1].tail, LabeledTail::Tail(ir::Tail::Jump(3)));
        assert_eq!(
            body[2].ops,
            vec![ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(4),
                src: ir::Scalar::Var(VarLocation::Local(2)),
                width: ir::Width::Word
            })]
        );
        assert_eq!(body[2].tail, LabeledTail::Tail(ir::Tail::Jump(3)));
        assert_eq!(
            body[3].ops,
            vec![ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::Var(VarLocation::Local(4)),
                width: ir::Width::Word
            })]
        );
    }
}
