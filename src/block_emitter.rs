use std::{collections::HashMap, mem};

use lang_c::{
    ast::{DoWhileStatement, ForStatement, IfStatement, WhileStatement},
    span::Node,
};

use crate::{
    compile::{compile_expression, compile_statement},
    error::ErrorCollector,
    ir,
    name_scope::NameScope, type_registry::TypeRegistry,
};

enum LabeledTail {
    Tail(ir::Tail),
    GotoLabel(String),
}

type LabeledBlock = ir::GenericBlock<LabeledTail>;

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

    pub fn append_operation(&mut self, op: ir::Op) {
        self.current_ops.push(op);
    }

    pub fn append_if(
        &mut self,
        ifs: Node<IfStatement>,
        scope: &mut NameScope,
        reg: &TypeRegistry,
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
                tail: LabeledTail::Tail(ir::Tail::Cond(cond, then_block_id, else_block_id)),
            },
        );

        self.current_id = then_block_id;
        scope.push();
        compile_statement(*ifs.node.then_statement, scope, self, reg, ec)?;
        scope.pop();
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
            compile_statement(*elses, scope, self, reg, ec)?;
            scope.pop();
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
