use std::{collections::HashMap, mem, fmt::Formatter};

use lang_c::{
    ast::{DoWhileStatement, ForStatement, IfStatement, WhileStatement},
    span::Node,
};

use crate::{
    compile::{compile_expression, compile_statement},
    error::ErrorCollector,
    ir,
    name_scope::NameScope,
};

pub enum LabeledTail { // TODO remove pub
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
        blocks.insert(self.current_id, LabeledBlock {
            phi: Vec::new(),
            ops: self.current_ops,
            tail: LabeledTail::Tail(ir::Tail::Ret)
        });
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

        let cond = compile_expression(*ifs.node.condition, scope, self, ec)?;

        let current_ops = mem::replace(&mut self.current_ops, Vec::new());
        self.blocks.insert(
            self.current_id,
            LabeledBlock {
                ops: current_ops,
                phi: Vec::new(),
                tail: LabeledTail::Tail(ir::Tail::Cond(cond.src, then_block_id, else_block_id)),
            },
        );

        self.current_id = then_block_id;
        scope.push();
        compile_statement(*ifs.node.then_statement, scope, self, ec)?;
        scope.pop_and_collect_initializers();
        let then_ops = mem::replace(&mut self.current_ops, Vec::new());
        self.blocks.insert(
            self.current_id,
            LabeledBlock {
                ops: then_ops,
                phi: Vec::new(),
                tail: LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
            },
        );

        if let Some(elses) = ifs.node.else_statement {
            self.current_id = else_block_id;
            scope.push();
            compile_statement(*elses, scope, self, ec)?;
            scope.pop_and_collect_initializers();
            let else_ops = mem::replace(&mut self.current_ops, Vec::new());
            self.blocks.insert(
                self.current_id,
                LabeledBlock {
                    ops: else_ops,
                    phi: Vec::new(),
                    tail: LabeledTail::Tail(ir::Tail::Jump(cont_block_id)),
                },
            );
        }

        self.current_id = cont_block_id;
        Ok(())
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
            LabeledTail::GotoLabel(l) => write!(f, "jmp {}", l)
        }
    }
}
