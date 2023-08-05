use std::collections::HashSet;
use std::{fmt::Formatter, hash::Hash};

use crate::generic_ir::UnaryUnsignedOp;
use crate::ir::VarLocation;
use crate::regalloc::get_live_ranges;
use crate::{
    block_emitter::BlockEmitter,
    ccpu::{self, opt::frame::resolve_frame_pointer, reg::FrameReg},
    compile, deconstruct, flush,
    generic_ir::{self, Op},
    ir::{self, Scalar},
    name_scope::{FunctionFrame, GlobalStorageClass, NameScope},
    opt::{const_propagate::propagate_const, ssa::delete_unused_regs},
    regalloc, ssa,
};
use lang_c::{
    ast::{FunctionDefinition, StorageClassSpecifier},
    span::Node,
};
use replace_with::replace_with_or_abort;

use crate::{
    ctype::{CType, FunctionArgs, QualifiedType},
    error::{CompileError, ErrorCollector},
    type_builder::TypeBuilder,
};

pub struct Function<Reg: Eq + Hash> {
    is_inline: bool,
    is_noreturn: bool,
    is_vararg: bool,
    is_reentrant: bool,
    name: String,
    storage_class: GlobalStorageClass,
    return_type: QualifiedType,
    args: FunctionArgs,
    body: Vec<generic_ir::Block<Reg>>,
    frame: FunctionFrame,
}

impl<Reg: Hash + Eq> Function<Reg> {
    pub fn set_reentrant(&mut self, reentrant: bool) {
        self.is_reentrant = reentrant;
    }

    pub fn is_reentrant(&self) -> bool {
        self.is_reentrant
    }

    pub fn get_frame_size(&self) -> u32 {
        self.frame.get_size()
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_body(&self) -> &Vec<generic_ir::GenericBlock<generic_ir::Tail<Reg>, Reg>> {
        &self.body
    }

    #[cfg(test)]
    pub fn get_frame(&self) -> &FunctionFrame {
        &self.frame
    }
}

impl Function<ir::VirtualReg> {
    pub fn new_from_node(
        node: Node<FunctionDefinition>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<Self, ()> {
        if !node.node.declarations.is_empty() {
            unimplemented!("K&R functions");
        }
        let (type_builder, storage_class, extra) =
            TypeBuilder::new_from_specifiers(node.node.specifiers, scope, ec)?;
        let type_builder = type_builder.stage2(node.span, ec)?;
        let (name, t) = type_builder.process_declarator_node(node.node.declarator, scope, ec)?;
        let name = name.unwrap();
        scope.declare(&name, t.clone(), &storage_class, None, node.span, ec)?;
        let (return_type, args, is_vararg) = if let CType::Function {
            result,
            args,
            vararg,
        } = t.t
        {
            (*result, args, vararg)
        } else {
            unreachable!()
        };
        let storage_class = match storage_class {
            None => GlobalStorageClass::Default,
            Some(stc) => match stc.node {
                StorageClassSpecifier::Typedef
                | StorageClassSpecifier::Auto
                | StorageClassSpecifier::Register
                | StorageClassSpecifier::ThreadLocal => {
                    ec.record_error(CompileError::WrongStorageClass, stc.span)?;
                    unreachable!();
                }
                StorageClassSpecifier::Extern => GlobalStorageClass::Extern,
                StorageClassSpecifier::Static => GlobalStorageClass::Static,
            },
        };
        let init_instructions = scope.start_function(&name, &args, &return_type);
        let mut be = BlockEmitter::new(init_instructions);
        compile::compile_statement(node.node.statement, scope, &mut be, ec)?;
        let frame = scope.end_function();
        init_address_regs(&mut be, &frame);
        let mut body = be.finalize(ec)?;
        flush::insert_flush_instructions(&mut body, &frame);
        Ok(Self {
            is_inline: extra.is_inline,
            is_noreturn: extra.is_noreturn,
            is_reentrant: false,
            storage_class,
            is_vararg,
            return_type,
            args,
            name,
            body,
            frame,
        })
    }

    pub fn optimize_ssa(&mut self) {
        use crate::opt::blocks::*;
        let mut modified = true;
        resolve_frame_pointer(&mut self.body, &self.name, self.is_reentrant);
        while modified {
            modified = false;
            modified |=
                replace_with::replace_with_or_abort_and_return(&mut self.body, drop_orphan_blocks);
            modified |= simplify_jumps(&mut self.body);
            modified |= merge_chains(&mut self.body);
            modified |= delete_unused_regs(&mut self.body);
            modified |= propagate_const(&mut self.body);
        }
    }

    pub fn enforce_ssa(&mut self, scope: &mut NameScope) {
        ssa::enforce_ssa(&mut self.body, scope);
    }

    /**
     * 1. Replace constants with registers in call arguments.
     * 2. Insert copies such that no register used as a call argument is live across any call.
     */
    pub fn enforce_call_regs(&mut self, scope: &mut NameScope) {
        for block_index in 0..self.body.len() {
            let barriers = {
                let mut barriers = Vec::new();
                for (i, op) in self.body[block_index].ops.iter().enumerate() {
                    if let Op::Call(_) = op {
                        barriers.push(i);
                    }
                }
                barriers
            };
            let live = get_live_ranges(&self.body, block_index);
            let mut copy_ops = Vec::new();
            for (i, op) in self.body[block_index].ops.iter_mut().enumerate() {
                if let Op::Call(c) = op {
                    let mut used_regs = HashSet::new();
                    for (s, w) in &mut c.args {
                        replace_with_or_abort(s, |s| {
                            let need_copy = if let Scalar::Var(v) = &s {
                                match v {
                                    VarLocation::Local(r) => {
                                        let (begin, end) = live.get(&r).unwrap();
                                        let mut crosses = false;
                                        for b in &barriers {
                                            if crosses_barrier(begin, end, *b) {
                                                crosses = true;
                                                break;
                                            }
                                        }
                                        if crosses {
                                            true
                                        } else {
                                            !used_regs.insert(*r)
                                        }
                                    }
                                    VarLocation::Global(_) | VarLocation::Return => true,
                                }
                            } else {
                                true
                            };
                            if need_copy {
                                let tmp = scope.alloc_temp();
                                copy_ops.push((
                                    i,
                                    Op::Copy(UnaryUnsignedOp {
                                        dst: tmp.clone(),
                                        src: s,
                                        width: *w,
                                    }),
                                ));
                                Scalar::Var(tmp)
                            } else {
                                s
                            }
                        });
                    }
                }
            }

            for (i, op) in copy_ops.into_iter().rev() {
                self.body[block_index].ops.insert(i, op);
            }
        }
    }
}

impl Function<ir::VirtualReg> {
    pub fn deconstruct_ssa(self) -> Function<ccpu::reg::FrameReg> {
        let mut map = regalloc::allocate_registers(&self.body);
        let body = deconstruct::deconstruct_ssa(self.body, &mut map);
        Function {
            is_inline: self.is_inline,
            is_noreturn: self.is_noreturn,
            is_vararg: self.is_vararg,
            is_reentrant: self.is_reentrant,
            name: self.name,
            storage_class: self.storage_class,
            return_type: self.return_type,
            args: self.args,
            body,
            frame: self.frame,
        }
    }
}

impl Function<FrameReg> {
    pub fn optimize_deconstructed(&mut self) {
        use crate::opt::blocks::*;
        let mut modified = true;
        while modified {
            modified = false;
            modified |=
                replace_with::replace_with_or_abort_and_return(&mut self.body, drop_orphan_blocks);
            modified |= simplify_jumps(&mut self.body);
            modified |= merge_chains(&mut self.body);
        }
    }
}

fn init_address_regs(be: &mut BlockEmitter, frame: &FunctionFrame) {
    for (offset, var) in frame.address_regs_iter() {
        be.append_operation_to_block(
            ir::Op::Add(ir::BinaryOp {
                dst: var.clone(),
                lhs: ir::Scalar::Var(ir::VarLocation::Local(frame.get_frame_pointer_reg())),
                rhs: ir::Scalar::ConstInt(offset as u64),
                width: ir::Width::PTR_WIDTH,
                sign: false,
            }),
            0,
        );
    }
}

fn crosses_barrier(begin: &Option<usize>, end: &Option<usize>, barrier: usize) -> bool {
    // begin < barrier && end > barrier
    let begin_before_barrier = if let Some(begin) = begin {
        *begin < barrier
    } else {
        true
    };
    let end_after_barrier = if let Some(end) = end {
        *end > barrier
    } else {
        true
    };
    begin_before_barrier && end_after_barrier
}

impl<Reg: std::fmt::Display + Eq + Hash> std::fmt::Display for Function<Reg> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(f, "function {}({})", self.name, self.get_frame_size())?;
        for (i, b) in self.body.iter().enumerate() {
            writeln!(f, "{}:\n{}", i, b)?;
        }
        Ok(())
    }
}
