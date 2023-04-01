use std::{collections::HashMap, mem};

use ir::GlobalVarId;

use crate::ir;

use super::compute::compute_const;

/**
 * Pefrorm the constant propagation optimization step. Blocks must be in SSA form.
 */
pub fn propagate_const(blocks: &mut Vec<ir::Block>) -> bool {
    let mut changed = false;
    // 1. Substitute constant registers
    let const_regs = find_const_registers(blocks);
    changed |= subs_const_registers(blocks, &const_regs);

    // 2. Compute constant expressions
    for block in blocks {
        for op in &mut block.ops {
            changed |= compute_const(op);
        }
    }
    changed
}

enum Constant {
    Int(u64),
    Sym(GlobalVarId, u16),
}

fn find_const_registers(blocks: &Vec<ir::Block>) -> HashMap<ir::VirtualReg, ir::Scalar> {
    let mut result = HashMap::new();
    for block in blocks {
        for op in &block.ops {
            if let ir::Op::Copy(op) = op {
                if let Some(reg) = op.dst.get_reg() {
                    match &op.src {
                        ir::Scalar::ConstInt(_) | ir::Scalar::SymbolOffset(_, _) => {
                            result.insert(reg, op.src.clone());
                        }
                        _ => (),
                    }
                }
            }
        }
    }
    result
}

fn subs_const_registers(
    blocks: &mut Vec<ir::Block>,
    const_regs: &HashMap<ir::VirtualReg, ir::Scalar>,
) -> bool {
    let mut modified = false;
    for block in blocks {
        let mut new_copy_ops = subs_phi(&mut block.phi, const_regs);
        modified |= !new_copy_ops.is_empty();

        for op in &mut block.ops {
            modified |= op.subs_src_regs(const_regs);
        }

        new_copy_ops.append(&mut block.ops);
        block.ops = new_copy_ops;

        modified |= block.tail.subs_src_regs(const_regs);
    }
    modified
}

fn subs_phi(phi: &mut ir::Phi, const_regs: &HashMap<ir::VirtualReg, ir::Scalar>) -> Vec<ir::Op> {
    let srcs = mem::replace(&mut phi.srcs, HashMap::new());
    let mut copies = Vec::new();
    let mut new_phi = HashMap::new();
    for (reg, (w, srcs)) in srcs.into_iter() {
        // Change regs to their constant values
        let substituted: Vec<_> = srcs
            .iter()
            .map(|(_, s)| new_substituted_scalar(s, const_regs))
            .collect();

        // No empty phis
        assert_ne!(substituted.len(), 0);

        // If all new scalars are constant and the same, remove this record from phi.
        let replace = match substituted.first().unwrap() {
            ir::Scalar::ConstInt(_) | ir::Scalar::SymbolOffset(_, _) => {
                substituted.windows(2).all(|w| w[0] == w[1])
            }
            _ => false,
        };
        if replace {
            copies.push(ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: ir::VarLocation::Local(reg),
                src: substituted.first().unwrap().clone(),
                width: w,
            }));
        } else {
            new_phi.insert(reg, (w, srcs));
        }
    }
    phi.srcs = new_phi;
    copies
}

fn new_substituted_scalar(
    scalar: &ir::Scalar,
    const_regs: &HashMap<ir::VirtualReg, ir::Scalar>,
) -> ir::Scalar {
    let mut r = scalar.clone();
    r.subs_reg(const_regs);
    r
}
