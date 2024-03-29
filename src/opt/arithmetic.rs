use crate::{
    generic_ir::BinaryUnsignedOp,
    ir::{self, Scalar},
};

pub fn optimize_arithmetics(blocks: &mut Vec<ir::Block>) -> bool {
    let mut changed = false;
    for block in blocks {
        for op in &mut block.ops {
            changed |= optimize_op(op);
        }
    }
    changed
}

fn optimize_op(op: &mut ir::Op) -> bool {
    let new_op = match op {
        ir::Op::Mul(op) => optimize_mul(op),
        ir::Op::Div(op) => optimize_div(op),
        ir::Op::Mod(op) => optimize_mod(op),
        ir::Op::Add(op) => optimize_add(op),
        ir::Op::Sub(op) => optimize_sub(op),
        _ => None,
    };
    if let Some(new_op) = new_op {
        *op = new_op;
        true
    } else {
        false
    }
}

fn optimize_mul(op: &ir::BinaryOp) -> Option<ir::Op> {
    let (lhs, rhs) = if let Scalar::ConstInt(_) = op.lhs {
        (&op.rhs, &op.lhs)
    } else {
        (&op.lhs, &op.rhs)
    };

    match rhs {
        Scalar::ConstInt(0) => Some(ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(0),
            width: op.width,
        })),
        Scalar::ConstInt(1) => Some(ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: lhs.clone(),
            width: op.width,
        })),
        Scalar::ConstInt(x) if x.is_power_of_two() => Some(ir::Op::LShift(ir::ShiftOp {
            dst: op.dst.clone(),
            lhs: lhs.clone(),
            lhs_sign: op.sign,
            lhs_width: op.width,
            rhs: Scalar::ConstInt(log2(*x)),
        })),
        _ => None,
    }
}

fn optimize_div(op: &ir::BinaryOp) -> Option<ir::Op> {
    match op.rhs {
        Scalar::ConstInt(0) => todo!("handle division by zero"),
        Scalar::ConstInt(1) => Some(ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: op.lhs.clone(),
            width: op.width,
        })),
        Scalar::ConstInt(x) if x.is_power_of_two() => Some(ir::Op::RShift(ir::ShiftOp {
            dst: op.dst.clone(),
            lhs: op.lhs.clone(),
            lhs_sign: op.sign,
            lhs_width: op.width,
            rhs: Scalar::ConstInt(log2(x)),
        })),
        _ => None,
    }
}

fn optimize_mod(op: &ir::BinaryOp) -> Option<ir::Op> {
    match op.rhs {
        Scalar::ConstInt(0) => todo!("handle division by zero"),
        Scalar::ConstInt(1) => Some(ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(0),
            width: op.width,
        })),
        Scalar::ConstInt(x) if x.is_power_of_two() => Some(ir::Op::BAnd(BinaryUnsignedOp {
            dst: op.dst.clone(),
            lhs: op.lhs.clone(),
            rhs: Scalar::ConstInt(x - 1),
            width: op.width,
        })),
        _ => None,
    }
}

fn optimize_add(op: &ir::BinaryOp) -> Option<ir::Op> {
    let (lhs, rhs) = if let Scalar::ConstInt(_) = op.lhs {
        (&op.rhs, &op.lhs)
    } else {
        (&op.lhs, &op.rhs)
    };

    match rhs {
        Scalar::ConstInt(0) => Some(ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: lhs.clone(),
            width: op.width,
        })),
        _ => None,
    }
}

fn optimize_sub(op: &ir::BinaryOp) -> Option<ir::Op> {
    match op.rhs {
        Scalar::ConstInt(0) => Some(ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: op.lhs.clone(),
            width: op.width,
        })),
        _ => None,
    }
}

fn log2(x: u64) -> u64 {
    64 - (x.leading_zeros() as u64) - 1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_log2() {
        assert_eq!(log2(1), 0);
        assert_eq!(log2(2), 1);
        assert_eq!(log2(4), 2);
        assert_eq!(log2(1024), 10);
    }
}
