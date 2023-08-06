use crate::{
    ccpu::{instr::InstructionWriter, reg::FrameReg},
    generic_ir::{Scalar, ShiftOp, VarLocation, Width},
};

pub fn gen_lshift(w: &mut InstructionWriter, op: &ShiftOp<FrameReg>) {
    match &op.rhs {
        Scalar::ConstInt(c) => match &op.lhs {
            Scalar::Var(lhs) => gen_lshift_var_by_const(w, &op.dst, lhs, *c, op.lhs_width),
            Scalar::SymbolOffset(_, _) => panic!("shifts by a symbol must be replaced by intrinsic calls"),
            Scalar::ConstInt(_) => {
                unreachable!("const propagation must be performed before emitting code")
            }
        },
        Scalar::Var(_) => panic!("shifts by a variable must be replaced by intrinsic calls"),
        Scalar::SymbolOffset(_, _) => unimplemented!("wtf"),
    }
}

pub fn gen_rshift(w: &mut InstructionWriter, op: &ShiftOp<FrameReg>) {
    match &op.rhs {
        Scalar::ConstInt(c) => match &op.lhs {
            Scalar::Var(lhs) => {
                gen_rshift_var_by_const(w, &op.dst, lhs, *c, op.lhs_width, op.lhs_sign)
            }
            Scalar::SymbolOffset(_, _) => panic!("shifts by a symbol must be replaced by intrinsic calls"),
            Scalar::ConstInt(_) => {
                unreachable!("const propagation must be performed before emitting code")
            }
        },
        Scalar::Var(_) => panic!("shifts by a variable must be replaced by intrinsic calls"),
        Scalar::SymbolOffset(_, _) => unimplemented!("wtf"),
    }
}

fn gen_lshift_var_by_const(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    lhs: &VarLocation<FrameReg>,
    rhs: u64,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    let rhs: u16 = if let Ok(rhs) = rhs.try_into() {
        rhs
    } else {
        unreachable!("const propagation must be performed before emitting code")
    };
    if rhs >= (width as u16) * 8 {
        unreachable!("const propagation must be performed before emitting code")
    }
    let byteshift = rhs / 8;
    let bitshift = rhs % 8;
    // 12345678_abcdefgh_ABCDEFGH_12345678 << 3
    // 45678abc_defghABC_DEFGH123_45678000
    //
    // 12345678_abcdefgh_ABCDEFGH_12345678 << (3 + 8)
    // defghABC_DEFGH123_45678000_00000000
    let dst_byte_lo = byteshift;
    let dst_byte_hi = width as u16;
    for dst_byte in (dst_byte_lo..dst_byte_hi).rev() {
        let src_byte = dst_byte - byteshift;
        w.ldi_p_var_location(lhs, src_byte, true);
        w.ld(A);

        if bitshift != 0 {
            if src_byte == 0 {
                for _ in 0..bitshift {
                    w.shl(A);
                }
            } else {
                w.ldi_p_var_location(lhs, src_byte - 1, true);
                w.ld(B);
                shift_ab(w, bitshift);
            }
        }
        w.ldi_p_var_location(dst, dst_byte, true);
        w.st(A);
    }
    if byteshift != 0 {
        w.ldi_const(A, 0, true);

        for dst_byte in (0..byteshift).rev() {
            w.ldi_p_var_location(dst, dst_byte, true);
            w.st(A);
        }
    }
}

fn gen_rshift_var_by_const(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    lhs: &VarLocation<FrameReg>,
    rhs: u64,
    width: Width,
    sign: bool,
) {
    if sign {
        gen_rashift_var_by_const(w, dst, lhs, rhs, width)
    } else {
        gen_rlshift_var_by_const(w, dst, lhs, rhs, width)
    }
}

fn gen_rashift_var_by_const(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    lhs: &VarLocation<FrameReg>,
    rhs: u64,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    let rhs: u16 = if let Ok(rhs) = rhs.try_into() {
        rhs
    } else {
        return sign_fill(w, dst, lhs, width);
    };
    if rhs >= (width as u16) * 8 {
        return sign_fill(w, dst, lhs, width);
    }
    let byteshift = rhs / 8;
    let bitshift = rhs % 8;
    // 12345678_abcdefgh_ABCDEFGH_12345678 >> 3
    // sss12345_678abcde_fghABCDE_FGH12345

    let dst_byte_hi = (width as u16) - byteshift;
    for dst_byte in 0..dst_byte_hi {
        let src_byte = dst_byte + byteshift;
        w.ldi_p_var_location(lhs, src_byte, true);
        w.ld(B);
        if bitshift == 0 {
            w.ldi_p_var_location(dst, dst_byte, true);
            w.st(B);
        } else {
            if src_byte + 1 == width as u16 {
                for _ in 0..bitshift {
                    w.sar(B);
                }
                w.ldi_p_var_location(dst, dst_byte, true);
                w.st(B);
            } else {
                w.ldi_p_var_location(lhs, src_byte + 1, true);
                w.ld(A);
                shift_ab(w, 8 - bitshift);
                w.ldi_p_var_location(dst, dst_byte, true);
                w.st(A);
            }
        }
    }
    if byteshift != 0 {
        // B contains last stored byte
        w.shl(B);
        w.exp(A);
        for dst_byte in dst_byte_hi..(width as u16) {
            w.ldi_p_var_location(dst, dst_byte, true);
            w.st(A);
        }
    }
}

fn gen_rlshift_var_by_const(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    lhs: &VarLocation<FrameReg>,
    rhs: u64,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    let rhs: u16 = if let Ok(rhs) = rhs.try_into() {
        rhs
    } else {
        unreachable!("const propagation must be performed before emitting code")
    };
    if rhs >= (width as u16) * 8 {
        unreachable!("const propagation must be performed before emitting code")
    }
    let byteshift = rhs / 8;
    let bitshift = rhs % 8;
    // 12345678_abcdefgh_ABCDEFGH_12345678 >> 3
    // sss12345_678abcde_fghABCDE_FGH12345

    let dst_byte_hi = (width as u16) - byteshift;
    for dst_byte in 0..dst_byte_hi {
        let src_byte = dst_byte + byteshift;
        w.ldi_p_var_location(lhs, src_byte, true);
        w.ld(B);
        if bitshift == 0 {
            w.ldi_p_var_location(dst, dst_byte, true);
            w.st(B);
        } else {
            if src_byte + 1 == width as u16 {
                for _ in 0..bitshift {
                    w.shr(B);
                }
                w.ldi_p_var_location(dst, dst_byte, true);
                w.st(B);
            } else {
                w.ldi_p_var_location(lhs, src_byte + 1, true);
                w.ld(A);
                shift_ab(w, 8 - bitshift);
                w.ldi_p_var_location(dst, dst_byte, true);
                w.st(A);
            }
        }
    }
    if byteshift != 0 {
        w.ldi_const(A, 0, true);
        for dst_byte in dst_byte_hi..(width as u16) {
            w.ldi_p_var_location(dst, dst_byte, true);
            w.st(A);
        }
    }
}

/**
 * Fill dst with sign of lhs.
 */
fn sign_fill(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    lhs: &VarLocation<FrameReg>,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    w.ldi_p_var_location(lhs, (width as u16) - 1, true);
    w.ld(A);
    w.shl(A);
    w.exp(A);
    for offset in (0..width as u16).rev() {
        w.ldi_p_var_location(dst, offset, true);
        w.st(A);
    }
}

/**
 * Shift A left by bitshift and fill lower bits with upper bits from B.
 */
fn shift_ab(w: &mut InstructionWriter, bitshift: u16) {
    /*
    Two options (example shift by 2):
    1.
        shl a
        shl a
        shr b
        shr b
        shr b
        shr b
        shr b
        shr b
        or  a, b

    2.
        shl a
        shl b
        adc a, 0
        shl a
        shl b
        adc a, 0

    Option 1: (8 + 1) instructions
    Option 2: (3 * bitshift) instructions
    */
    use crate::ccpu::instr::Reg::*;
    if 8 + 1 < 3 * bitshift {
        for _ in 0..bitshift {
            w.shl(A);
        }
        for _ in bitshift..8 {
            w.shr(B);
        }
        w.or(A, B);
    } else {
        for _ in 0..bitshift {
            w.shl(A);
            w.shl(B);
            w.adc(A, Zero);
        }
    }
}
