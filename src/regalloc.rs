use std::collections::{HashMap, HashSet};

use crate::ir;

const MAX_HW_REGISTERS: usize = 200;

/**
 * Allocate physical registers for a program in an SSA form.
 *
 * Each virtual register is expected to live only across single block and in sources of phi nodes of its children.
 * Physical registers are plenty, no spilling is performed.
 */
pub fn allocate_registers(body: &[ir::Block]) -> HashMap<ir::Reg, usize> {
    let mut hints = HashMap::new();
    let mut allocations = HashMap::new();

    for op in body.first().unwrap().ops.iter() {
        if let ir::Op::Arg(reg, arg) = op {
            hints.insert(*reg, *arg);
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
    allocations: &mut HashMap<ir::Reg, usize>,
    hints: &mut HashMap<ir::Reg, usize>,
) {
    let mut vacant_registers = vec![true; MAX_HW_REGISTERS];
    let block = &body[block_index];

    // Determine kill positions
    let kill = {
        let mut kill = vec![HashSet::<ir::Reg>::new(); block.ops.len()];

        let mut live_regs = HashSet::new();
        for child_id in block.tail.get_connections() {
            body[child_id].phi.collect_read_regs(&mut live_regs);
            for (dst, srcs) in body[child_id].phi.srcs.iter() {
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
        for (phi_dst, phi_srcs) in body[child_id].phi.srcs.iter() {
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
        reg: ir::Reg,
        vacant_registers: &mut Vec<bool>,
        hints: &HashMap<ir::Reg, usize>,
    ) -> usize {
        if let Some(&hint) = hints.get(&reg) {
            if vacant_registers[hint] {
                vacant_registers[hint] = false;
                return hint;
            }
        }
        let phy_reg = vacant_registers
            .iter()
            .enumerate()
            .find(|(_, vacant)| **vacant)
            .map(|(i, _)| i)
            .expect("out of registers");
        vacant_registers[phy_reg] = false;
        phy_reg
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
            assert!(!vacant_registers[phy_reg]);
            vacant_registers[phy_reg] = true;
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
        for (phi_dst, phi_srcs) in child.phi.srcs.iter() {
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

enum LiveRangeBegin {
    Phi,
    Op(usize),
}

enum LiveRangeEnd {
    Phi,
    Op(usize),
    Tail,
    Child,
}

struct LiveRange {
    begin: LiveRangeBegin,
    end: LiveRangeEnd,
}

// fn find_live_vars(body: &[ir::Block]) -> Vec<(HashSet<ir::Reg>, HashSet<ir::Reg>)> {
//     let mut result = vec![(HashSet::new(), HashSet::new()); body.len()];
//     let mut changed = vec![true; body.len()];
//     while let Some((i, _)) = changed.iter().enumerate().find(|&(_, &c)| c) {

//     }
//     result
// }
