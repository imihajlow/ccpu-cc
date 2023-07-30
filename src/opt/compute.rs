use crate::{generic_ir::Scalar, ir::*};

pub fn compute_const(op: &mut Op) -> bool {
    let new_op = match op {
        Op::Copy(_) => None,
        Op::Bool(op) => compute_bool(op),
        Op::BoolInv(op) => compute_boolinv(op),
        Op::Add(op) => compute_add(op),
        Op::Sub(op) => compute_sub(op),
        Op::Mul(op) => compute_mul(op),
        Op::Div(op) => compute_div(op),
        Op::Mod(op) => compute_mod(op),
        Op::BAnd(op) => compute_band(op),
        Op::BOr(op) => compute_bor(op),
        Op::BXor(op) => compute_bxor(op),
        Op::LShift(op) => compute_lshift(op),
        Op::RShift(op) => compute_rshift(op),
        Op::Neg(op) => compute_neg(op),
        Op::Not(op) => compute_not(op),
        Op::Compare(op) => compute_compare(op),
        Op::Conv(op) => compute_conv(op),
        Op::Store(_) => None,
        Op::Load(_) => None,
        Op::Call(_) => None,
        Op::Memcpy(_) => None,
        Op::Arg(_) => None,
        Op::FramePointer(_) => None,
        Op::Undefined(_) => None,
        #[cfg(test)]
        Op::Dummy(_) => None,
    };
    if let Some(new_op) = new_op {
        *op = new_op;
        true
    } else {
        false
    }
}

fn compute_bool(op: &UnaryUnsignedOp) -> Option<Op> {
    match op.src {
        Scalar::ConstInt(c) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(if clamp(c, op.width) == 0 { 0 } else { 1 }),
            width: Width::Byte,
        })),
        Scalar::SymbolOffset(_, _) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(1),
            width: Width::Byte,
        })),
        _ => None,
    }
}

fn compute_boolinv(op: &UnaryUnsignedOp) -> Option<Op> {
    match op.src {
        Scalar::ConstInt(c) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(if clamp(c, op.width) == 0 { 1 } else { 0 }),
            width: Width::Byte,
        })),
        Scalar::SymbolOffset(_, _) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(0),
            width: Width::Byte,
        })),
        _ => None,
    }
}

fn compute_add(op: &BinaryOp) -> Option<Op> {
    match (&op.lhs, &op.rhs) {
        (Scalar::ConstInt(lhs), Scalar::ConstInt(rhs)) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(clamp(lhs.wrapping_add(*rhs), op.width)),
            width: op.width,
        })),

        (Scalar::SymbolOffset(s, o), Scalar::ConstInt(x))
        | (Scalar::ConstInt(x), Scalar::SymbolOffset(s, o)) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::SymbolOffset(s.clone(), o.wrapping_add((x & 0xffff) as u16)),
            width: op.width,
        })),
        (Scalar::SymbolOffset(_, _), Scalar::SymbolOffset(_, _)) => unreachable!("WTF is this"),
        _ => None,
    }
}
fn compute_sub(op: &BinaryOp) -> Option<Op> {
    match (&op.lhs, &op.rhs) {
        (Scalar::ConstInt(lhs), Scalar::ConstInt(rhs)) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(clamp(lhs.wrapping_sub(*rhs), op.width)),
            width: op.width,
        })),

        (Scalar::SymbolOffset(s, o), Scalar::ConstInt(x)) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::SymbolOffset(s.clone(), o.wrapping_sub((x & 0xffff) as u16)),
            width: op.width,
        })),
        (Scalar::SymbolOffset(s1, o1), Scalar::SymbolOffset(s2, o2)) if s1 == s2 => {
            let diff = (*o1 as i64) - (*o2 as i64);
            Some(Op::Copy(UnaryUnsignedOp {
                dst: op.dst.clone(),
                src: Scalar::ConstInt(diff as u64),
                width: op.width,
            }))
        }
        (Scalar::ConstInt(_), Scalar::SymbolOffset(_, _)) => unreachable!("WTF is this"),
        _ => None,
    }
}
fn compute_mul(op: &BinaryOp) -> Option<Op> {
    match (&op.lhs, &op.rhs) {
        (Scalar::ConstInt(lhs), Scalar::ConstInt(rhs)) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(clamp(lhs.wrapping_mul(*rhs), op.width)),
            width: op.width,
        })),

        (Scalar::SymbolOffset(_, _), _) | (_, Scalar::SymbolOffset(_, _)) => {
            unreachable!("WTF is this")
        }
        _ => None,
    }
}
fn compute_div(op: &BinaryOp) -> Option<Op> {
    match (&op.lhs, &op.rhs) {
        (Scalar::ConstInt(lhs), Scalar::ConstInt(rhs)) => {
            let lhs = widen(*lhs, op.width, Width::Qword, op.sign);
            let rhs = widen(*rhs, op.width, Width::Qword, op.sign);
            if rhs == 0 {
                panic!("Division by zero"); // TODO proper error
            }
            let result = if op.sign {
                let lhs = lhs as i64;
                let rhs = rhs as i64;
                lhs.wrapping_div(rhs) as u64
            } else {
                lhs.wrapping_div(rhs)
            };
            Some(Op::Copy(UnaryUnsignedOp {
                dst: op.dst.clone(),
                src: Scalar::ConstInt(clamp(result, op.width)),
                width: op.width,
            }))
        }

        (Scalar::SymbolOffset(_, _), _) | (_, Scalar::SymbolOffset(_, _)) => {
            unreachable!("WTF is this")
        }
        _ => None,
    }
}
fn compute_mod(op: &BinaryOp) -> Option<Op> {
    match (&op.lhs, &op.rhs) {
        (Scalar::ConstInt(lhs), Scalar::ConstInt(rhs)) => {
            let lhs = widen(*lhs, op.width, Width::Qword, op.sign);
            let rhs = widen(*rhs, op.width, Width::Qword, op.sign);
            if rhs == 0 {
                panic!("Division by zero"); // TODO proper error
            }
            let result = if op.sign {
                let lhs = lhs as i64;
                let rhs = rhs as i64;
                lhs.wrapping_rem(rhs) as u64
            } else {
                lhs.wrapping_rem(rhs)
            };
            Some(Op::Copy(UnaryUnsignedOp {
                dst: op.dst.clone(),
                src: Scalar::ConstInt(clamp(result, op.width)),
                width: op.width,
            }))
        }

        (Scalar::SymbolOffset(_, _), _) | (_, Scalar::SymbolOffset(_, _)) => {
            unreachable!("WTF is this")
        }
        _ => None,
    }
}
fn compute_band(op: &BinaryUnsignedOp) -> Option<Op> {
    match (&op.lhs, &op.rhs) {
        (Scalar::ConstInt(lhs), Scalar::ConstInt(rhs)) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(clamp(lhs & rhs, op.width)),
            width: op.width,
        })),
        _ => None,
    }
}
fn compute_bor(op: &BinaryUnsignedOp) -> Option<Op> {
    match (&op.lhs, &op.rhs) {
        (Scalar::ConstInt(lhs), Scalar::ConstInt(rhs)) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(clamp(lhs | rhs, op.width)),
            width: op.width,
        })),
        _ => None,
    }
}
fn compute_bxor(op: &BinaryUnsignedOp) -> Option<Op> {
    match (&op.lhs, &op.rhs) {
        (Scalar::ConstInt(lhs), Scalar::ConstInt(rhs)) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(clamp(lhs ^ rhs, op.width)),
            width: op.width,
        })),
        _ => None,
    }
}
fn compute_lshift(op: &ShiftOp) -> Option<Op> {
    match (&op.lhs, &op.rhs) {
        (Scalar::ConstInt(lhs), Scalar::ConstInt(rhs)) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(clamp(lhs.wrapping_shl(*rhs as u32), op.lhs_width)),
            width: op.lhs_width,
        })),
        (_, Scalar::ConstInt(rhs)) if *rhs >= (op.lhs_width as u64) * 8 => {
            Some(Op::Copy(UnaryUnsignedOp {
                dst: op.dst.clone(),
                src: Scalar::ConstInt(0),
                width: op.lhs_width,
            }))
        }
        _ => None,
    }
}
fn compute_rshift(op: &ShiftOp) -> Option<Op> {
    match (&op.lhs, &op.rhs) {
        (Scalar::ConstInt(lhs), Scalar::ConstInt(rhs)) => {
            let r = if op.lhs_sign {
                let lhs = widen(*lhs, op.lhs_width, Width::Qword, op.lhs_sign) as i64;
                lhs.wrapping_shr(*rhs as u32) as u64
            } else {
                lhs.wrapping_shr(*rhs as u32)
            };
            Some(Op::Copy(UnaryUnsignedOp {
                dst: op.dst.clone(),
                src: Scalar::ConstInt(clamp(r, op.lhs_width)),
                width: op.lhs_width,
            }))
        }
        (_, Scalar::ConstInt(rhs)) if !op.lhs_sign && *rhs >= (op.lhs_width as u64) * 8 => {
            Some(Op::Copy(UnaryUnsignedOp {
                dst: op.dst.clone(),
                src: Scalar::ConstInt(0),
                width: op.lhs_width,
            }))
        }
        _ => None,
    }
}
fn compute_neg(op: &UnaryUnsignedOp) -> Option<Op> {
    match &op.src {
        Scalar::ConstInt(src) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(clamp(1u64.wrapping_add(!src), op.width)),
            width: op.width,
        })),
        Scalar::SymbolOffset(_, _) => unreachable!("WTF is this"),
        _ => None,
    }
}
fn compute_not(op: &UnaryUnsignedOp) -> Option<Op> {
    match &op.src {
        Scalar::ConstInt(src) => Some(Op::Copy(UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: Scalar::ConstInt(clamp(!src, op.width)),
            width: op.width,
        })),
        Scalar::SymbolOffset(_, _) => unreachable!("WTF is this"),
        _ => None,
    }
}
fn compute_compare(op: &CompareOp) -> Option<Op> {
    match (&op.lhs, &op.rhs) {
        (Scalar::ConstInt(lhs), Scalar::ConstInt(rhs)) => {
            let r = compare(*lhs, *rhs, op.width, op.sign, op.kind);
            Some(Op::Copy(UnaryUnsignedOp {
                dst: op.dst.clone(),
                src: Scalar::ConstInt(if r { 1 } else { 0 }),
                width: op.dst_width,
            }))
        }
        (Scalar::SymbolOffset(sl, ol), Scalar::SymbolOffset(sr, or)) if sl == sr => {
            let r = compare(*ol as u64, *or as u64, Width::Word, op.sign, op.kind);
            Some(Op::Copy(UnaryUnsignedOp {
                dst: op.dst.clone(),
                src: Scalar::ConstInt(if r { 1 } else { 0 }),
                width: op.dst_width,
            }))
        }
        _ => None,
    }
}
fn compute_conv(op: &ConvOp) -> Option<Op> {
    if op.dst_width > op.src_width {
        match &op.src {
            Scalar::ConstInt(c) => Some(Op::Copy(UnaryUnsignedOp {
                dst: op.dst.clone(),
                src: Scalar::ConstInt(widen(*c, op.src_width, op.dst_width, op.src_sign)),
                width: op.dst_width,
            })),
            Scalar::SymbolOffset(s, o) => Some(Op::Copy(UnaryUnsignedOp {
                dst: op.dst.clone(),
                src: Scalar::SymbolOffset(s.clone(), *o),
                width: op.dst_width,
            })),
            _ => None,
        }
    } else {
        match &op.src {
            Scalar::ConstInt(c) => Some(Op::Copy(UnaryUnsignedOp {
                dst: op.dst.clone(),
                src: Scalar::ConstInt(clamp(*c, op.dst_width)),
                width: op.dst_width,
            })),
            Scalar::SymbolOffset(s, o) => {
                let new_offset = if op.dst_width == Width::Byte {
                    o & 0xff
                } else {
                    *o
                };
                Some(Op::Copy(UnaryUnsignedOp {
                    dst: op.dst.clone(),
                    src: Scalar::SymbolOffset(s.clone(), new_offset),
                    width: op.dst_width,
                }))
            }
            _ => None,
        }
    }
}

fn clamp(c: u64, w: Width) -> u64 {
    if w == Width::Qword {
        c
    } else {
        let mask = !((!0_u64).wrapping_shl((w as u32) * 8));
        c & mask
    }
}

fn widen(c: u64, from_w: Width, to_w: Width, sign_extend: bool) -> u64 {
    let clamped = clamp(c, from_w);
    let sign_bit = clamped >> (from_w as u16 * 8 - 1);
    if sign_extend && sign_bit != 0 {
        let from_bits = from_w as u32 * 8;
        let sign_extension: u64 = (!0_u64).wrapping_shl(from_bits);
        clamp(clamped | sign_extension, to_w)
    } else {
        clamped
    }
}

fn compare(lhs: u64, rhs: u64, width: Width, sign: bool, kind: CompareKind) -> bool {
    let lhs = widen(lhs, width, Width::Qword, sign);
    let rhs = widen(rhs, width, Width::Qword, sign);
    if sign {
        let lhs = lhs as i64;
        let rhs = rhs as i64;
        match kind {
            CompareKind::Equal => lhs == rhs,
            CompareKind::NotEqual => lhs != rhs,
            CompareKind::LessThan => lhs < rhs,
            CompareKind::GreaterThan => lhs > rhs,
            CompareKind::LessOrEqual => lhs <= rhs,
            CompareKind::GreaterOrEqual => lhs >= rhs,
        }
    } else {
        match kind {
            CompareKind::Equal => lhs == rhs,
            CompareKind::NotEqual => lhs != rhs,
            CompareKind::LessThan => lhs < rhs,
            CompareKind::GreaterThan => lhs > rhs,
            CompareKind::LessOrEqual => lhs <= rhs,
            CompareKind::GreaterOrEqual => lhs >= rhs,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::generic_ir::Width;

    #[test]
    fn test_clamp() {
        assert_eq!(clamp(0x12345, Width::Word), 0x2345);
        assert_eq!(
            clamp(0xffff_1234_ffff_1234, Width::Qword),
            0xffff_1234_ffff_1234
        );
    }

    #[test]
    fn test_widen() {
        assert_eq!(widen(0x2345, Width::Word, Width::Dword, false), 0x2345);
        assert_eq!(widen(0x2345, Width::Byte, Width::Dword, false), 0x45);
        assert_eq!(widen(0x2345, Width::Word, Width::Dword, true), 0x2345);
        assert_eq!(widen(0x8345, Width::Word, Width::Dword, true), 0xffff8345);
    }

    #[test]
    fn test_compare() {
        assert!(compare(
            0xffff,
            0x0001,
            Width::Word,
            true,
            CompareKind::LessThan
        ));
        assert!(!compare(
            0xffff,
            0x0001,
            Width::Word,
            false,
            CompareKind::LessThan
        ));
    }
}
