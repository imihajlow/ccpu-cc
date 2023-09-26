use std::{collections::HashMap, fmt::Formatter, mem};

use lang_c::{
    ast::{
        ConditionalExpression, DoWhileStatement, Expression, ForStatement, Identifier, IfStatement,
        LabeledStatement, SwitchStatement, WhileStatement,
    },
    span::{Node, Span},
};

use crate::{
    compile::{
        self, cast, compile_declaration, compile_expression, compile_statement, convert_to_bool,
        integer_promote,
    },
    constant::{check_static_assert, compute_constant_expr},
    ctype::{self, QualifiedType, Qualifiers},
    error::{CompileError, ErrorCollector},
    ir,
    name_scope::NameScope,
    rvalue::{RValue, TypedRValue},
};

#[derive(Clone)]
pub struct BlockEmitter {
    next_id: usize,
    current_id: usize,
    loop_depth: usize,
    blocks: HashMap<usize, LabeledBlock>,
    current_ops: Vec<ir::Op>,
    breaks: Vec<usize>,
    continues: Vec<usize>,
    switches: Vec<SwitchRecord>,
    labels: HashMap<String, usize>,
}

#[derive(Clone, Debug, PartialEq)]
enum LabeledTail {
    Tail(ir::Tail),
    GotoLabel(String, Span),
}

type LabeledBlock = ir::GenericBlock<LabeledTail>;

#[derive(Clone)]
struct SwitchRecord {
    block: usize,
    src: ir::Scalar,
    t: QualifiedType,
    cases: HashMap<u64, usize>,
    default: Option<usize>,
}

impl BlockEmitter {
    /**
     * Block 0 contains initial operations and ends in a jump to block 1, which is made active.
     */
    pub fn new(initial_ops: Vec<ir::Op>) -> Self {
        let mut blocks = HashMap::new();
        blocks.insert(
            0,
            LabeledBlock {
                phi: ir::Phi::new(),
                ops: initial_ops,
                tail: LabeledTail::Tail(ir::Tail::Jump(1)),
                loop_depth: 0,
                original_id: 0
            },
        );
        Self {
            next_id: 2,
            current_id: 1,
            loop_depth: 0,
            blocks,
            current_ops: Vec::new(),
            breaks: Vec::new(),
            continues: Vec::new(),
            labels: HashMap::new(),
            switches: Vec::new(),
        }
    }

    pub fn finalize(self, ec: &mut ErrorCollector) -> Result<Vec<ir::GenericBlock<ir::Tail>>, ()> {
        assert_eq!(self.loop_depth, 0);
        let mut blocks = self.blocks;
        blocks.insert(
            self.current_id,
            LabeledBlock {
                phi: ir::Phi::new(),
                ops: self.current_ops,
                tail: LabeledTail::Tail(ir::Tail::Ret),
                loop_depth: 0,
                original_id: self.current_id,
            },
        );
        let mut result = Vec::new();
        for i in 0..self.next_id {
            let block = blocks.remove(&i).unwrap();
            let tail = match block.tail {
                LabeledTail::Tail(t) => t,
                LabeledTail::GotoLabel(label, span) => {
                    if let Some(id) = self.labels.get(&label) {
                        ir::Tail::Jump(*id)
                    } else {
                        ec.record_error(CompileError::UndeclaredLabel(label), span)?;
                        unreachable!();
                    }
                }
            };
            result.push(ir::GenericBlock {
                phi: block.phi,
                ops: block.ops,
                tail,
                loop_depth: block.loop_depth,
                original_id: block.original_id,
            });
        }
        Ok(result)
    }

    pub fn append_operation(&mut self, op: ir::Op) {
        self.current_ops.push(op);
    }

    pub fn append_operation_to_block(&mut self, op: ir::Op, block_id: usize) {
        let target_ops = if self.current_id == block_id {
            &mut self.current_ops
        } else {
            &mut self.blocks.get_mut(&block_id).unwrap().ops
        };
        target_ops.push(op);
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
        let r = compile_statement(*ifs.node.then_statement, scope, self, ec);
        scope.pop_and_collect_initializers();
        r?;

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
            else_block_id,
        );

        if let Some(elses) = ifs.node.else_statement {
            scope.push();
            let r = compile_statement(*elses, scope, self, ec);
            scope.pop_and_collect_initializers();
            r?;

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
        self.loop_depth += 1;

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
        let r = compile_statement(*whiles.node.statement, scope, self, ec);
        self.pop_continue();
        self.pop_break();
        scope.pop_and_collect_initializers();
        r?;

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(condition_block_id)),
            continue_block_id,
        );
        self.loop_depth -= 1;

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
        self.loop_depth += 1;

        scope.push();
        let r: Result<(), ()> = (|| {
            self.set_break(continue_block_id);
            self.set_continue(condition_block_id);
            compile_statement(*whiles.node.statement, scope, self, ec)?;
            self.pop_continue();
            self.pop_break();
            Ok(())
        })();
        scope.pop_and_collect_initializers();
        r?;

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
        self.loop_depth -= 1;
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
        let r = (|| {
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
            self.loop_depth += 1;

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
            let r = compile_statement(*fors.node.statement, scope, self, ec);
            scope.pop_and_collect_initializers();
            r?;
            self.pop_continue();
            self.pop_break();
            if let Some(step) = fors.node.step {
                compile_expression(*step, scope, self, ec)?;
            }

            self.finish_block(
                LabeledTail::Tail(ir::Tail::Jump(condition_block_id)),
                continue_block_id,
            );
            self.loop_depth -= 1;
            Ok(())
        })();

        scope.pop_and_collect_initializers();
        r
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

    pub fn append_return(
        &mut self,
        expr: Option<Node<Expression>>,
        span: Span,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let return_type = scope.get_return_type();
        if let Some(expr) = expr {
            if return_type.t.is_void() {
                return ec.record_error(CompileError::UnexpectedRetrurnValue, span);
            }
            let val = compile_expression(expr, scope, self, ec)?;
            if return_type.t.is_scalar() {
                if !val.t.t.is_scalar() {
                    ec.record_error(CompileError::IncompatibleTypes(return_type, val.t), span)?;
                    unreachable!();
                }
                let casted = cast(val, &return_type.t, false, span, scope, self, ec)?;
                let (src, src_type) = casted.unwrap_scalar_and_type();
                let width = src_type.t.get_scalar_width().unwrap();
                self.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: ir::VarLocation::Return,
                    src,
                    width,
                }))
            } else if return_type.t.is_object() {
                todo!("return objects")
            } else {
                unreachable!()
            }
        } else {
            if !return_type.t.is_void() {
                return ec.record_error(CompileError::NoReturnValue, span);
            }
        }
        let orphan_block_id = self.alloc_block_id();
        self.finish_block(LabeledTail::Tail(ir::Tail::Ret), orphan_block_id);
        Ok(())
    }

    pub fn append_labeled_statement(
        &mut self,
        ls: Node<LabeledStatement>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        use lang_c::ast::Label;
        match ls.node.label.node {
            Label::Identifier(id) => self.append_label(id, ec)?,
            Label::Case(e) => self.append_case(*e, ls.span, scope, ec)?,
            Label::Default => self.append_default(ls.span, ec)?,
            Label::CaseRange(_) => {
                ec.record_error(
                    CompileError::Unimplemented("case range".to_string()),
                    ls.span,
                )?;
                unreachable!()
            }
        }
        compile_statement(*ls.node.statement, scope, self, ec)
    }

    pub fn append_goto(&mut self, id: Node<Identifier>) {
        let orphan = self.alloc_block_id();
        self.finish_block(LabeledTail::GotoLabel(id.node.name, id.span), orphan);
    }

    pub fn append_switch(
        &mut self,
        sw: Node<SwitchStatement>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let switch_body_block = self.alloc_block_id();
        let continue_block = self.alloc_block_id();

        let sw_expr_span = sw.node.expression.span;
        let sw_expr = compile_expression(*sw.node.expression, scope, self, ec)?;
        if !sw_expr.t.t.is_integer() {
            return ec.record_error(CompileError::IntegerTypeRequired, sw_expr_span);
        }
        let sw_expr = integer_promote((sw_expr, sw_expr_span), scope, self, ec)?;
        let (src, t) = sw_expr.unwrap_scalar_and_type();

        self.switches.push(SwitchRecord {
            src,
            t: t,
            block: self.current_id,
            cases: HashMap::new(),
            default: None,
        });
        self.breaks.push(continue_block);

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(continue_block)), // tail is to be replaced
            switch_body_block,
        );

        compile_statement(*sw.node.statement, scope, self, ec)?;

        self.breaks.pop().unwrap();
        let sr = self.switches.pop().unwrap();

        let block = self.blocks.get_mut(&sr.block).unwrap();

        let tail = ir::Tail::Switch(
            sr.src,
            sr.t.t.get_scalar_width().unwrap(),
            sr.cases.into_iter().collect(),
            sr.default.unwrap_or(continue_block),
        );
        block.tail = LabeledTail::Tail(tail);

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(continue_block)),
            continue_block,
        );
        Ok(())
    }

    fn append_label(&mut self, id: Node<Identifier>, ec: &mut ErrorCollector) -> Result<(), ()> {
        let next_block_id = self.alloc_block_id();
        if let Some(_) = self.labels.insert(id.node.name.clone(), next_block_id) {
            ec.record_error(CompileError::LabelRedefinition(id.node.name), id.span)?;
            unreachable!()
        }
        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(next_block_id)),
            next_block_id,
        );
        Ok(())
    }

    fn append_case(
        &mut self,
        expr: Node<Expression>,
        span: Span,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let new_block_id = self.alloc_block_id();
        let c_span = expr.span;
        let c = compute_constant_expr(expr, true, scope, ec)?;
        if !c.t.t.is_integer() {
            return ec.record_error(CompileError::IntegerTypeRequired, c_span);
        }
        let sr = if let Some(sr) = self.switches.last_mut() {
            sr
        } else {
            return ec.record_error(CompileError::UnexpectedCase, span);
        };
        let c = c.implicit_cast(&sr.t, span, ec)?.unwrap_integer();

        if sr.cases.insert(c as u64, new_block_id).is_some() {
            return ec.record_error(CompileError::DuplicateCase(c), span);
        }

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(new_block_id)),
            new_block_id,
        );
        Ok(())
    }

    fn append_default(&mut self, span: Span, ec: &mut ErrorCollector) -> Result<(), ()> {
        let new_block_id = self.alloc_block_id();
        let sr = if let Some(sr) = self.switches.last_mut() {
            sr
        } else {
            return ec.record_error(CompileError::UnexpectedDefault, span);
        };
        if sr.default.is_some() {
            return ec.record_error(CompileError::DuplicateDefault, span);
        }
        sr.default = Some(new_block_id);
        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(new_block_id)),
            new_block_id,
        );
        Ok(())
    }

    fn finish_block(&mut self, tail: LabeledTail, next_block: usize) {
        let current_ops = mem::replace(&mut self.current_ops, Vec::new());
        self.blocks.insert(
            self.current_id,
            LabeledBlock {
                ops: current_ops,
                phi: ir::Phi::new(),
                tail,
                loop_depth: self.loop_depth,
                original_id: self.current_id,
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
            LabeledTail::GotoLabel(l, _) => write!(f, "jmp {}", l),
        }
    }
}
