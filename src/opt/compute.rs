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
    todo!()
}
fn compute_mod(op: &BinaryOp) -> Option<Op> {
    todo!()
}
fn compute_band(op: &BinaryUnsignedOp) -> Option<Op> {
    todo!()
}
fn compute_bor(op: &BinaryUnsignedOp) -> Option<Op> {
    todo!()
}
fn compute_bxor(op: &BinaryUnsignedOp) -> Option<Op> {
    todo!()
}
fn compute_lshift(op: &ShiftOp) -> Option<Op> {
    todo!()
}
fn compute_rshift(op: &ShiftOp) -> Option<Op> {
    todo!()
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
    todo!()
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
    let mask = !((!0) << ((w as u64) * 8));
    c & mask
}

fn widen(c: u64, from_w: Width, to_w: Width, sign_extend: bool) -> u64 {
    let clamped = clamp(c, from_w);
    let sign_bit = clamped >> (from_w as u16 * 8 - 1);
    if sign_extend && sign_bit != 0 {
        let from_bits = from_w as u64 * 8;
        let sign_extension: u64 = !0 << from_bits;
        clamp(clamped | sign_extension, to_w)
    } else {
        clamped
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::generic_ir::Width;

    #[test]
    fn test_clamp() {
        assert_eq!(clamp(0x12345, Width::Word), 0x2345);
    }

    #[test]
    fn test_widen() {
        assert_eq!(widen(0x2345, Width::Word, Width::Dword, false), 0x2345);
        assert_eq!(widen(0x2345, Width::Byte, Width::Dword, false), 0x45);
        assert_eq!(widen(0x2345, Width::Word, Width::Dword, true), 0x2345);
        assert_eq!(widen(0x8345, Width::Word, Width::Dword, true), 0xffff8345);
    }
}
