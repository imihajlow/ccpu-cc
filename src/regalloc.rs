use rand::seq::SliceRandom;
use rand::thread_rng;
use std::collections::{HashMap, HashSet};

use crate::{ccpu::reg::FrameReg, generic_ir::IntrinCallVariant, ir, register::Register};

/**
 * Allocate physical registers for a program in an SSA form.
 *
 * Each virtual register is expected to live only across single block and in sources of phi nodes of its children.
 * Physical registers are plenty, no spilling is performed.
 */
pub fn allocate_registers(body: &[ir::Block]) -> HashMap<ir::VirtualReg, FrameReg> {
    let mut hints = HashMap::new();
    let mut allocations = HashMap::new();
    let mut preallocated = HashMap::new();

    // Preallocate this function parameters registers - Arg operations only come in the first block
    for op in body.first().unwrap().ops.iter() {
        if let ir::Op::Arg(arg_op) = op {
            preallocated.insert(
                arg_op.dst_reg,
                FrameReg::get_current_fn_arg(arg_op.arg_number).unwrap(),
            );
        }
    }
    for block in body {
        for op in &block.ops {
            if let ir::Op::Call(call_op) = op {
                for (i, (arg, _)) in call_op.args.iter().enumerate() {
                    if let Some(arg_reg) = arg.get_reg() {
                        hints.insert(arg_reg, FrameReg::get_callee_arg(i).unwrap());
                    }
                }
            } else if let ir::Op::IntrinCall(call_op) = op {
                match &call_op.variant {
                    IntrinCallVariant::Call2R((_, r), (_, a1), (_, a2)) => {
                        if let Some(reg) = a1.get_reg() {
                            hints.insert(reg, FrameReg::IntrinsicArg1);
                        }
                        if let Some(reg) = a2.get_reg() {
                            hints.insert(reg, FrameReg::IntrinsicArg2);
                        }
                        if let Some(reg) = r.get_reg() {
                            hints.insert(reg, FrameReg::IntrinsicRet);
                        }
                    }
                    IntrinCallVariant::Call3((_, a1), (_, a2), (_, a3)) => {
                        if let Some(reg) = a1.get_reg() {
                            hints.insert(reg, FrameReg::IntrinsicArg1);
                        }
                        if let Some(reg) = a2.get_reg() {
                            hints.insert(reg, FrameReg::IntrinsicArg2);
                        }
                        if let Some(reg) = a3.get_reg() {
                            hints.insert(reg, FrameReg::IntrinsicArg3);
                        }
                    }
                }
            }
        }
    }

    for i in 0..body.len() {
        allocate_registers_for_block(body, i, &mut allocations, &mut hints, &preallocated);
    }

    allocations
}

/**
 * For each operation in a block return a set of registers which are not used after this operation.
 */
pub fn get_kill_sets(body: &[ir::Block], block_index: usize) -> Vec<HashSet<ir::VirtualReg>> {
    let block = &body[block_index];
    let mut kill = vec![HashSet::<ir::VirtualReg>::new(); block.ops.len()];
    let mut live_regs = HashSet::new();
    for child_id in block.tail.get_connections() {
        body[child_id].phi.collect_read_regs(&mut live_regs);
    }
    block.tail.collect_read_regs(&mut live_regs);
    for (i, op) in block.ops.iter().enumerate().rev() {
        let mut op_read_regs = HashSet::new();
        op.collect_read_regs(&mut op_read_regs);
        kill[i].extend(op_read_regs.difference(&live_regs));
        live_regs.extend(op_read_regs.into_iter());
    }
    kill
}

/**
 * Find live ranges of registers used in a block.
 * None in the range begin means register is set in phi.
 * None in the range end means register is used in tail or in block children.
 */
pub fn get_live_ranges(
    body: &[ir::Block],
    block_index: usize,
) -> HashMap<ir::VirtualReg, (Option<usize>, Option<usize>)> {
    let kill_positions = {
        let kill = get_kill_sets(body, block_index);
        let mut kill_positions = HashMap::new();
        for (i, regs) in kill.into_iter().enumerate() {
            for reg in regs {
                let r = kill_positions.insert(reg, i);
                assert!(r.is_none());
            }
        }
        kill_positions
    };

    let mut result = HashMap::new();
    let block = &body[block_index];
    for (reg, _) in &block.phi.srcs {
        let kill = kill_positions.get(&reg).copied();
        let r = result.insert(*reg, (None, kill));
        assert!(r.is_none());
    }
    for (i, op) in block.ops.iter().enumerate() {
        if let Some(reg) = op.get_dst_reg() {
            let kill = kill_positions.get(&reg).copied();
            let r = result.insert(reg, (Some(i), kill));
            assert!(r.is_none());
        }
    }
    result
}

fn allocate_registers_for_block(
    body: &[ir::Block],
    block_index: usize,
    allocations: &mut HashMap<ir::VirtualReg, FrameReg>,
    hints: &mut HashMap<ir::VirtualReg, FrameReg>,
    preallocated: &HashMap<ir::VirtualReg, FrameReg>,
) {
    let mut vacant_registers = vec![true; FrameReg::get_register_count()];

    let block = &body[block_index];

    // Determine kill positions
    let kill = get_kill_sets(body, block_index);

    let set_regs = {
        let mut set_regs = HashSet::new();
        block.phi.collect_set_regs(&mut set_regs);
        for op in &block.ops {
            op.collect_set_regs(&mut set_regs);
        }
        set_regs
    };

    for (v, r) in preallocated {
        if set_regs.contains(v) {
            vacant_registers[r.get_index()] = false;
        }
    }

    // Get hints from chidren's phi
    for child_id in block.tail.get_connections() {
        for (phi_dst, (_, phi_srcs)) in body[child_id].phi.srcs.iter() {
            if let Some(phi_dst_allocation) = allocations.get(phi_dst) {
                for (src_block_index, src_scalar) in phi_srcs.iter() {
                    if *src_block_index == block_index {
                        if let Some(src_reg) = src_scalar.get_reg() {
                            hints.try_insert(src_reg, *phi_dst_allocation).ok();
                        }
                    }
                }
            }
        }
    }

    fn allocate(
        reg: ir::VirtualReg,
        vacant_registers: &mut Vec<bool>,
        hints: &HashMap<ir::VirtualReg, FrameReg>,
        preallocated: &HashMap<ir::VirtualReg, FrameReg>,
    ) -> FrameReg {
        if let Some(prealloc) = preallocated.get(&reg) {
            assert!(!vacant_registers[prealloc.get_index()]);
            return *prealloc;
        }
        if let Some(&hint) = hints.get(&reg) {
            let n = hint.get_index();
            if vacant_registers[n] {
                vacant_registers[n] = false;
                return hint;
            }
        }
        let phy_reg = vacant_registers
            .iter()
            .enumerate()
            .find(|(_, vacant)| **vacant)
            .map(|(i, _)| i as ir::VirtualReg)
            .expect("out of registers");
        vacant_registers[phy_reg as usize] = false;
        FrameReg::FrameA(phy_reg as u16)
    }

    // Allocate for phi
    let mut dsts: Vec<_> = block.phi.srcs.keys().collect();
    dsts.shuffle(&mut thread_rng());

    for phi_dst in dsts {
        let phi_reg = allocate(*phi_dst, &mut vacant_registers, hints, preallocated);
        let old = allocations.insert(*phi_dst, phi_reg);
        if old.is_some() {
            panic!("double allocation");
        }
    }

    // Allocate for body
    for (i, op) in block.ops.iter().enumerate() {
        for reg in kill[i].iter() {
            let phy_reg = *allocations.get(reg).unwrap();
            assert!(!vacant_registers[phy_reg.get_index()]);
            vacant_registers[phy_reg.get_index()] = true;
        }
        if let Some(dst) = op.get_dst_reg() {
            let dst_reg = allocate(dst, &mut vacant_registers, hints, preallocated);
            let old = allocations.insert(dst, dst_reg);
            if old.is_some() {
                panic!("double allocation");
            }
        }
    }

    // Hint children
    for child_id in block.tail.get_connections() {
        let child = &body[child_id];
        for (phi_dst, (_, phi_srcs)) in child.phi.srcs.iter() {
            for (src_block_index, src_scalar) in phi_srcs.iter() {
                if *src_block_index == block_index {
                    if let Some(src_reg) = src_scalar.get_reg() {
                        hints
                            .try_insert(*phi_dst, *allocations.get(&src_reg).unwrap())
                            .ok();
                    }
                }
            }
        }
    }
}
