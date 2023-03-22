use std::collections::{HashMap, HashSet};

use crate::{ccpu::reg::FrameReg, ir, register::Register};

/**
 * Allocate physical registers for a program in an SSA form.
 *
 * Each virtual register is expected to live only across single block and in sources of phi nodes of its children.
 * Physical registers are plenty, no spilling is performed.
 */
pub fn allocate_registers(body: &[ir::Block]) -> HashMap<ir::VirtualReg, FrameReg> {
    let mut hints = HashMap::new();
    let mut allocations = HashMap::new();

    // Hint this function parameters registers - Arg operations only come in the first block
    for op in body.first().unwrap().ops.iter() {
        if let ir::Op::Arg(arg_op) = op {
            hints.insert(
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
            }
        }
    }

    for i in 0..body.len() {
        allocate_registers_for_block(body, i, &mut allocations, &mut hints);
    }

    allocations
}

fn allocate_registers_for_block(
    body: &[ir::Block],
    block_index: usize,
    allocations: &mut HashMap<ir::VirtualReg, FrameReg>,
    hints: &mut HashMap<ir::VirtualReg, FrameReg>,
) {
    let mut vacant_registers = vec![true; FrameReg::get_register_count()];
    let block = &body[block_index];

    // Determine kill positions
    let kill = {
        let mut kill = vec![HashSet::<ir::VirtualReg>::new(); block.ops.len()];

        let mut live_regs = HashSet::new();
        for child_id in block.tail.get_connections() {
            body[child_id].phi.collect_read_regs(&mut live_regs);
            for (dst, (_, srcs)) in body[child_id].phi.srcs.iter() {
                if let Some(allocation) = allocations.get(dst) {
                    for (src_block_index, src) in srcs.iter() {
                        if *src_block_index == block_index {
                            if let Some(reg) = src.get_reg() {
                                hints.try_insert(reg, *allocation).ok();
                            }
                        }
                    }
                }
            }
        }
        block.tail.collect_read_regs(&mut live_regs);
        for (i, op) in block.ops.iter().enumerate().rev() {
            let mut op_read_regs = HashSet::new();
            op.collect_read_regs(&mut op_read_regs);
            kill[i].extend(op_read_regs.difference(&live_regs));
            live_regs.extend(op_read_regs.into_iter());
        }
        kill
    };

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
    ) -> FrameReg {
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
    for (phi_dst, _) in block.phi.srcs.iter() {
        let phi_reg = allocate(*phi_dst, &mut vacant_registers, hints);
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
            let dst_reg = allocate(dst, &mut vacant_registers, hints);
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
