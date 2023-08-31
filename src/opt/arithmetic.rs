use crate::ir::{self, Scalar};

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
