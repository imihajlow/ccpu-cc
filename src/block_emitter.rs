use std::{collections::HashMap, fmt::Formatter, mem};

use lang_c::{
    ast::{DoWhileStatement, Expression, ForStatement, IfStatement, WhileStatement},
    span::Node,
};

use crate::{
    compile::{self, compile_expression, compile_statement, convert_to_bool, TypedSrc},
    ctype::{self, QualifiedType, Qualifiers},
    error::{CompileError, ErrorCollector},
    ir,
    name_scope::NameScope,
};

pub enum LabeledTail {
    // TODO remove pub
    Tail(ir::Tail),
    GotoLabel(String),
}

pub type LabeledBlock = ir::GenericBlock<LabeledTail>; // TODO remove pub

pub struct BlockEmitter {
    next_id: usize,
    current_id: usize,
    blocks: HashMap<usize, LabeledBlock>,
    current_ops: Vec<ir::Op>,
}

impl BlockEmitter {
    pub fn new() -> Self {
        Self {
            next_id: 1,
            current_id: 0,
            blocks: HashMap::new(),
            current_ops: Vec::new(),
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
        for i in 0..=self.current_id {
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
            LabeledTail::Tail(ir::Tail::Cond(cond_bool.src, then_block_id, else_block_id)),
            then_block_id,
        );

        scope.push();
        compile_statement(*ifs.node.then_statement, scope, self, ec)?;
        scope.pop_and_collect_initializers();

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
            if ifs.node.else_statement.is_some() {
                else_block_id
            } else {
                cont_block_id
            },
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
    ) -> Result<TypedSrc, ()> {
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
                lhs_bool.src,
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
            src: rhs_int.src,
            width,
        }));

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
            shortcut_block_id,
        );

        self.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: result_var.clone(),
            src: ir::Src::ConstInt(0),
            width,
        }));

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
            cont_block_id,
        );

        Ok(TypedSrc {
            src: ir::Src::Var(result_var),
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
    ) -> Result<TypedSrc, ()> {
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
                lhs_bool.src,
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
            src: rhs_int.src,
            width,
        }));

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
            shortcut_block_id,
        );

        self.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: result_var.clone(),
            src: ir::Src::ConstInt(1),
            width,
        }));

        self.finish_block(
            LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
            cont_block_id,
        );

        Ok(TypedSrc {
            src: ir::Src::Var(result_var),
            t: QualifiedType {
                t: ctype::INT_TYPE,
                qualifiers: Qualifiers::empty(),
            },
        })
    }

    pub fn append_while(
        &mut self,
        whiles: Node<WhileStatement>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        todo!()
    }

    pub fn append_do_while(
        &mut self,
        whiles: Node<DoWhileStatement>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        todo!()
    }

    pub fn append_for(
        &mut self,
        fors: Node<ForStatement>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        todo!()
    }

    fn finish_block(&mut self, tail: LabeledTail, next_id: usize) {
        let current_ops = mem::replace(&mut self.current_ops, Vec::new());
        self.blocks.insert(
            self.current_id,
            LabeledBlock {
                ops: current_ops,
                phi: Vec::new(),
                tail,
            },
        );
        self.current_id = next_id;
    }

    fn alloc_block_id(&mut self) -> usize {
        let r = self.next_id;
        self.next_id += 1;
        r
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
