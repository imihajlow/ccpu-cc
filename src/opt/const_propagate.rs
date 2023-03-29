use std::{collections::HashMap, mem};

use ir::GlobalVarId;

use crate::{ir};

/**
 * Pefrorm the constant propagation optimization step. Blocks must be in SSA form.
 */
pub fn propagate_const(blocks: &mut Vec<ir::Block>) -> bool {
    let mut changed = false;
    // 1. Substitute constant registers
    let const_regs = find_const_registers(blocks);
    changed |= subs_const_registers(blocks, &const_regs);

    // 2. Compute constant expressions
    todo!();
    changed
}

enum Constant {
    Int(u64),
    Sym(GlobalVarId, u16),
}

fn find_const_registers(blocks: &Vec<ir::Block>) -> HashMap<ir::VirtualReg, Constant> {
    let mut result = HashMap::new();
    for block in blocks {
        for op in &block.ops {
            if let ir::Op::Copy(op) = op {
                if let Some(reg) = op.dst.get_reg() {
                    match &op.src {
                        ir::Scalar::ConstInt(c) => {
                            result.insert(reg, Constant::Int(*c));
                        }
                        ir::Scalar::SymbolOffset(s, o) => {
                            result.insert(reg, Constant::Sym(s.clone(), *o));
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
    const_regs: &HashMap<ir::VirtualReg, Constant>,
) -> bool {
    let mut modified = false;
    for block in blocks {
        let mut new_copy_ops = subs_phi(&mut block.phi, const_regs);
        modified |= !new_copy_ops.is_empty();

        for op in &mut block.ops {
            modified |= match op {
                ir::Op::Copy(op) => subs_const_srcs_unary_unsigned(op, const_regs),
                ir::Op::Bool(op) => subs_const_srcs_unary_unsigned(op, const_regs),
                ir::Op::BoolInv(op) => subs_const_srcs_unary_unsigned(op, const_regs),
                ir::Op::Add(op) => subs_const_srcs_binary(op, const_regs),
                ir::Op::Sub(op) => subs_const_srcs_binary(op, const_regs),
                ir::Op::Mul(op) => subs_const_srcs_binary(op, const_regs),
                ir::Op::Div(op) => subs_const_srcs_binary(op, const_regs),
                ir::Op::Mod(op) => subs_const_srcs_binary(op, const_regs),
                ir::Op::BAnd(op) => subs_const_srcs_binary_unsigned(op, const_regs),
                ir::Op::BOr(op) => subs_const_srcs_binary_unsigned(op, const_regs),
                ir::Op::BXor(op) => subs_const_srcs_binary_unsigned(op, const_regs),
                ir::Op::LShift(op) => subs_const_srcs_shift(op, const_regs),
                ir::Op::RShift(op) => subs_const_srcs_shift(op, const_regs),
                ir::Op::Neg(op) => subs_const_srcs_unary_unsigned(op, const_regs),
                ir::Op::Not(op) => subs_const_srcs_unary_unsigned(op, const_regs),
                ir::Op::Compare(op) => subs_const_srcs_compare(op, const_regs),
                ir::Op::Conv(op) => subs_const_srcs_conv(op, const_regs),
                ir::Op::Store(op) => subs_const_srcs_store(op, const_regs),
                ir::Op::Load(op) => subs_const_srcs_load(op, const_regs),
                ir::Op::Call(op) => subs_const_srcs_call(op, const_regs),
                ir::Op::Memcpy(op) => subs_const_srcs_memcpy(op, const_regs),
                ir::Op::Arg(_) => false,
                ir::Op::Undefined(_) => false,
                #[cfg(test)]
                ir::Op::Dummy(_) => false,
            }
        }

        new_copy_ops.append(&mut block.ops);
        block.ops = new_copy_ops;

        modified |= match &mut block.tail {
            ir::Tail::Cond(c, _, _) | ir::Tail::Switch(c, _, _, _) => {
                subs_const_scalar(c, const_regs)
            }
            ir::Tail::Jump(_) => false,
            ir::Tail::Ret => false,
        };
    }
    modified
}

fn subs_phi(phi: &mut ir::Phi, const_regs: &HashMap<ir::VirtualReg, Constant>) -> Vec<ir::Op> {
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

fn subs_const_srcs_unary_unsigned(
    op: &mut ir::UnaryUnsignedOp,
    const_regs: &HashMap<ir::VirtualReg, Constant>,
) -> bool {
    subs_const_scalar(&mut op.src, const_regs)
}

fn subs_const_srcs_binary(
    op: &mut ir::BinaryOp,
    const_regs: &HashMap<ir::VirtualReg, Constant>,
) -> bool {
    subs_const_scalar(&mut op.lhs, const_regs) | subs_const_scalar(&mut op.rhs, const_regs)
}

fn subs_const_srcs_binary_unsigned(
    op: &mut ir::BinaryUnsignedOp,
    const_regs: &HashMap<ir::VirtualReg, Constant>,
) -> bool {
    subs_const_scalar(&mut op.lhs, const_regs) | subs_const_scalar(&mut op.rhs, const_regs)
}

fn subs_const_srcs_shift(
    op: &mut ir::ShiftOp,
    const_regs: &HashMap<ir::VirtualReg, Constant>,
) -> bool {
    subs_const_scalar(&mut op.lhs, const_regs) | subs_const_scalar(&mut op.rhs, const_regs)
}

fn subs_const_srcs_compare(
    op: &mut ir::CompareOp,
    const_regs: &HashMap<ir::VirtualReg, Constant>,
) -> bool {
    subs_const_scalar(&mut op.lhs, const_regs) | subs_const_scalar(&mut op.rhs, const_regs)
}

fn subs_const_srcs_conv(
    op: &mut ir::ConvOp,
    const_regs: &HashMap<ir::VirtualReg, Constant>,
) -> bool {
    subs_const_scalar(&mut op.src, const_regs)
}

fn subs_const_srcs_store(
    op: &mut ir::StoreOp,
    const_regs: &HashMap<ir::VirtualReg, Constant>,
) -> bool {
    subs_const_scalar(&mut op.dst_addr, const_regs) | subs_const_scalar(&mut op.src, const_regs)
}

fn subs_const_srcs_load(
    op: &mut ir::LoadOp,
    const_regs: &HashMap<ir::VirtualReg, Constant>,
) -> bool {
    subs_const_scalar(&mut op.src_addr, const_regs)
}

fn subs_const_srcs_call(
    op: &mut ir::CallOp,
    const_regs: &HashMap<ir::VirtualReg, Constant>,
) -> bool {
    let mut result = false;
    for (s, _) in &mut op.args {
        result |= subs_const_scalar(s, const_regs);
    }
    result
}

fn subs_const_srcs_memcpy(
    op: &mut ir::MemcpyOp,
    const_regs: &HashMap<ir::VirtualReg, Constant>,
) -> bool {
    subs_const_scalar(&mut op.dst_addr, const_regs)
        | subs_const_scalar(&mut op.src_addr, const_regs)
}

fn subs_const_scalar(
    scalar: &mut ir::Scalar,
    const_regs: &HashMap<ir::VirtualReg, Constant>,
) -> bool {
    if let Some(reg) = scalar.get_reg() {
        if let Some(c) = const_regs.get(&reg) {
            *scalar = match c {
                Constant::Int(n) => ir::Scalar::ConstInt(*n),
                Constant::Sym(s, o) => ir::Scalar::SymbolOffset(s.clone(), *o),
            };
            return true;
        }
    }
    false
}

fn new_substituted_scalar(
    scalar: &ir::Scalar,
    const_regs: &HashMap<ir::VirtualReg, Constant>,
) -> ir::Scalar {
    if let Some(reg) = scalar.get_reg() {
        if let Some(c) = const_regs.get(&reg) {
            return match c {
                Constant::Int(n) => ir::Scalar::ConstInt(*n),
                Constant::Sym(s, o) => ir::Scalar::SymbolOffset(s.clone(), *o),
            };
        }
    }
    scalar.clone()
}
