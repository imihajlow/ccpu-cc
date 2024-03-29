use crate::generic_ir::UnaryUnsignedOp;
use crate::ir::{Op, VarLocation};
use crate::opt::arithmetic::optimize_arithmetics;
use crate::opt::condition::optimize_conditions;
use crate::opt::copy::{drop_trivial_copies, reduce_copies};
use crate::opt::width::{optimize_comparison_width, optimize_width};
use crate::regalloc::get_live_ranges;
use crate::utils::factorial;
use crate::{
    block_emitter::BlockEmitter,
    ccpu::{self, opt::frame::resolve_frame_pointer, reg::FrameReg},
    compile, deconstruct, flush,
    generic_ir::{self},
    ir::{self, Scalar},
    name_scope::{FunctionFrame, GlobalStorageClass, NameScope},
    opt::{const_propagate::propagate_const, ssa::delete_unused_regs},
    regalloc, ssa,
};
use crate::{
    ctype::{CType, FunctionArgs, QualifiedType},
    error::{CompileError, ErrorCollector},
    type_builder::TypeBuilder,
};
use ir::{GlobalVarId, Width};
use lang_c::span::Span;
use lang_c::{
    ast::{FunctionDefinition, StorageClassSpecifier},
    span::Node,
};
use replace_with::replace_with_or_abort;
use std::collections::HashSet;
use std::{fmt::Formatter, hash::Hash};

pub struct Function<Reg: Eq + Hash> {
    is_inline: bool,
    is_noreturn: bool,
    is_vararg: bool,
    is_reentrant: bool,
    custom_section: Option<String>,
    name: String,
    storage_class: GlobalStorageClass,
    return_type: QualifiedType,
    args: FunctionArgs,
    body: Vec<generic_ir::Block<Reg>>,
    frame: FunctionFrame,
    span: Span,
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

    pub fn get_span(&self) -> Span {
        self.span
    }

    pub fn get_custom_section(&self) -> Option<&str> {
        self.custom_section.as_ref().map(|s| s.as_str())
    }

    pub fn get_id(&self) -> GlobalVarId {
        match self.storage_class {
            GlobalStorageClass::Static => GlobalVarId::Static(self.name.clone()),
            GlobalStorageClass::Default | GlobalStorageClass::Extern => {
                GlobalVarId::Global(self.name.clone())
            }
        }
    }

    pub fn get_body(&self) -> &Vec<generic_ir::GenericBlock<generic_ir::Tail<Reg>, Reg>> {
        &self.body
    }

    pub fn is_weak(&self) -> bool {
        self.is_inline
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
            ec.record_error(
                CompileError::Unimplemented("K&R functions".to_string()),
                node.span,
            )?;
            unreachable!()
        }
        let (type_builder, storage_class, extra) =
            TypeBuilder::new_from_specifiers(node.node.specifiers, scope, ec)?;
        let type_builder = type_builder.stage2(node.span, ec)?;
        let (name, t, attrs) =
            type_builder.process_declarator_node(node.node.declarator, scope, ec)?;
        let name = name.unwrap();
        let custom_section = attrs.get_section().map(|s| s.to_string());
        scope.declare(&name, t.clone(), &storage_class, None, attrs, node.span, ec)?;
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
        let init_instructions = scope.start_function(&name, &args, &return_type, extra.is_inline);
        let mut be = BlockEmitter::new(init_instructions);
        let compile_result = compile::compile_statement(node.node.statement, scope, &mut be, ec);
        let frame = scope.end_function();
        compile_result?;
        init_address_regs(&mut be, &frame);
        let mut body = be.finalize(ec)?;
        flush::insert_flush_instructions(&mut body, &frame);
        Ok(Self {
            is_inline: extra.is_inline,
            is_noreturn: extra.is_noreturn,
            is_reentrant: false,
            custom_section,
            storage_class,
            is_vararg,
            return_type,
            args,
            name,
            body,
            frame,
            span: node.span,
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
            modified |= optimize_arithmetics(&mut self.body);
            modified |= optimize_width(&mut self.body);
            modified |= reduce_copies(&mut self.body);
            modified |= optimize_conditions(&mut self.body);
            modified |= optimize_comparison_width(&mut self.body);
        }
    }

    pub fn enforce_ssa(&mut self, scope: &mut NameScope) {
        ssa::enforce_ssa(&mut self.body, scope);
    }

    pub fn utilise_intrin_calls(&mut self) {
        for block in &mut self.body {
            for op in &mut block.ops {
                ccpu::intrin::replace_with_intrinsic(op);
            }
        }
    }

    /**
     * 1. Replace constants with registers in call arguments.
     * 2. Insert copies such that no register used as a call argument is live across any call.
     * 3. Insert copies for call arguments to separate them from incoming arguments.
     */
    pub fn enforce_special_regs(&mut self, scope: &mut NameScope) {
        let is_call_barrier = |op: &Op| {
            if let Op::Call(_) = op {
                true
            } else {
                false
            }
        };
        let foreach_arg_call = |op: &mut Op, callback: &mut dyn FnMut(&mut Width, &mut Scalar)| {
            if let Op::Call(c) = op {
                for (s, w) in &mut c.args {
                    callback(w, s);
                }
                for (s, w) in &mut c.va_args {
                    callback(w, s);
                }
            }
        };

        let is_intrin_barrier = |op: &Op| {
            if let Op::Call(_) = op {
                true
            } else if let Op::IntrinCall(_) = op {
                true
            } else {
                false
            }
        };
        let foreach_arg_intrin =
            |op: &mut Op, callback: &mut dyn FnMut(&mut Width, &mut Scalar)| {
                if let Op::IntrinCall(c) = op {
                    c.foreach_arg_mut(callback);
                }
            };

        for block_index in 0..self.body.len() {
            let mut copy_ops = Vec::new();
            {
                let mut ops = self.enforce_call_regs_common(
                    block_index,
                    scope,
                    is_call_barrier,
                    foreach_arg_call,
                );
                copy_ops.append(&mut ops);
            }
            {
                let mut ops = self.enforce_call_regs_common(
                    block_index,
                    scope,
                    is_intrin_barrier,
                    foreach_arg_intrin,
                );
                copy_ops.append(&mut ops);
            }
            copy_ops.sort_by(|(ia, _), (ib, _)| ia.cmp(ib));
            let ops = &mut self.body[block_index].ops;
            for (i, op) in copy_ops.into_iter().rev() {
                ops.insert(i, op);
            }
        }

        let arg_regs = {
            let mut arg_regs = HashSet::new();
            let first_block = &self.body[0];
            for op in &first_block.ops {
                if let Op::Arg(op) = op {
                    arg_regs.insert(op.dst_reg);
                }
            }
            arg_regs
        };
        for block_index in 0..self.body.len() {
            let mut copy_ops = Vec::new();
            {
                let block = &mut self.body[block_index];
                for (i, op) in block.ops.iter_mut().enumerate() {
                    let mut copy_arg = |w: &mut Width, s: &mut Scalar| {
                        if let Some(reg) = s.get_reg() {
                            if arg_regs.contains(&reg) {
                                replace_with_or_abort(s, |s| {
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
                                });
                            }
                        }
                    };
                    foreach_arg_intrin(op, &mut copy_arg);
                    foreach_arg_call(op, &mut copy_arg);
                }
            }
            let ops = &mut self.body[block_index].ops;
            for (i, op) in copy_ops.into_iter().rev() {
                ops.insert(i, op);
            }
        }
    }

    fn enforce_call_regs_common<FIsBarrier, FForeachArg>(
        &mut self,
        block_index: usize,
        scope: &mut NameScope,
        is_barrier: FIsBarrier,
        foreach_arg: FForeachArg,
    ) -> Vec<(usize, Op)>
    where
        FIsBarrier: Fn(&Op) -> bool,
        FForeachArg: Fn(&mut Op, &mut dyn FnMut(&mut Width, &mut Scalar)),
    {
        let mut barriers = Vec::new();
        for (i, op) in self.body[block_index].ops.iter().enumerate() {
            if is_barrier(op) {
                barriers.push(i);
            }
        }

        let live = get_live_ranges(&self.body, block_index);
        let mut copy_ops = Vec::new();
        for (i, op) in self.body[block_index].ops.iter_mut().enumerate() {
            let mut used_regs = HashSet::new();
            foreach_arg(op, &mut |w: &mut Width, s: &mut Scalar| {
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
            });
        }
        copy_ops
    }
}

impl Function<ir::VirtualReg> {
    pub fn deconstruct_ssa(self) -> Function<ccpu::reg::FrameReg> {
        let phi_permutations = self
            .body
            .iter()
            .map(|block| factorial(block.phi.srcs.len()))
            .fold(Some(1), |a, b| {
                a.and_then(|a: usize| b.and_then(|b| a.checked_mul(b)))
            });
        let repetitions = match phi_permutations {
            Some(p) if p < 10000 => p * 5,
            _ => 10000,
        };
        let mut min_copies = None;
        let mut best_body = None;
        for _ in 0..repetitions {
            let body = self.body.clone();
            let mut map = regalloc::allocate_registers(&self.body);
            let (body, n_copies) = deconstruct::deconstruct_ssa(body, &mut map);
            let better = if let Some(m) = min_copies {
                n_copies < m
            } else {
                true
            };
            if better {
                min_copies = Some(n_copies);
                best_body = Some(body);
            }
        }
        Function {
            is_inline: self.is_inline,
            is_noreturn: self.is_noreturn,
            is_vararg: self.is_vararg,
            is_reentrant: self.is_reentrant,
            custom_section: self.custom_section,
            name: self.name,
            storage_class: self.storage_class,
            return_type: self.return_type,
            args: self.args,
            body: best_body.unwrap(),
            frame: self.frame,
            span: self.span,
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
            modified |= drop_trivial_copies(&mut self.body);
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
    if begin.is_none() || end.is_none() {
        // crosses the block barrier
        true
    } else {
        begin.unwrap() < barrier && end.unwrap() > barrier
    }
}

impl<Reg: std::fmt::Display + Eq + Hash> std::fmt::Display for Function<Reg> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(f, "function {}({})", self.name, self.get_frame_size())?;
        for (i, b) in self.body.iter().enumerate() {
            if i == b.original_id {
                writeln!(f, "{}:\n{}", i, b)?;
            } else {
                writeln!(f, "{} (originally {}):\n{}", i, b.original_id, b)?;
            }
        }
        Ok(())
    }
}
