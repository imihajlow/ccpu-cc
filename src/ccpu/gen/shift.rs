use crate::{
    ccpu::{
        global::get_global_var_label,
        instr::{InstructionWriter, Reg},
        reg::FrameReg,
    },
    generic_ir::{self, GlobalVarId, Scalar, ShiftOp, VarLocation, Width},
};

pub fn gen_lshift(w: &mut InstructionWriter, op: &ShiftOp<FrameReg>) {
    match &op.rhs {
        Scalar::ConstInt(c) => match &op.lhs {
            Scalar::Var(lhs) => gen_lshift_var_by_const(w, &op.dst, lhs, *c, op.lhs_width),
            Scalar::SymbolOffset(_, _) => todo!(),
            Scalar::ConstInt(_) => {
                unreachable!("const propagation must be performed before emitting code")
            }
        },
        Scalar::Var(_) => todo!(),
        Scalar::SymbolOffset(_, _) => unimplemented!("wtf"),
    }
}

pub fn gen_rshift(w: &mut InstructionWriter, op: &ShiftOp<FrameReg>) {
    match op.rhs {
        Scalar::ConstInt(_c) => todo!(),
        Scalar::Var(_) => todo!(),
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
