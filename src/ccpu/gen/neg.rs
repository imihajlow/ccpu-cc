use crate::{
    ccpu::{instr::InstructionWriter, reg::FrameReg},
    generic_ir::{self, Scalar, VarLocation, Width},
};

pub fn gen_neg(w: &mut InstructionWriter, op: &generic_ir::UnaryUnsignedOp<FrameReg>) {
    let src_reg = match &op.src {
        Scalar::Var(v) => v,
        Scalar::ConstInt(_) => {
            unreachable!("const propagation must be performed before emitting code")
        }
        Scalar::SymbolOffset(_, _) => unreachable!("doesn't make sense"),
    };
    match op.width {
        Width::Byte => gen_neg_byte(w, &op.dst, src_reg),
        Width::Word => gen_neg_word(w, &op.dst, src_reg),
        _ => gen_neg_long(w, &op.dst, src_reg, op.width),
    }
}

fn gen_neg_byte(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src: &VarLocation<FrameReg>,
) {
    use crate::ccpu::instr::Reg::*;
    w.ldi_p_var_location(src, 0, true);
    w.ld(A);
    w.neg(A);
    w.ldi_p_var_location(dst, 0, true);
    w.st(A);
}

fn gen_neg_word(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src: &VarLocation<FrameReg>,
) {
    use crate::ccpu::instr::Reg::*;
    w.ldi_p_var_location(src, 0, true);
    w.ld(B);
    w.inc(PL);
    w.ld(A);
    w.not(B);
    w.not(A);
    w.inc(B);
    w.adc(A, Zero);
    w.ldi_p_var_location(dst, 1, true);
    w.st(A);
    w.dec(PL);
    w.st(B);
}

fn gen_neg_long(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src: &VarLocation<FrameReg>,
    width: Width,
) {
    assert!(width as u16 >= 2);
    use crate::ccpu::instr::Reg::*;

    if dst == src {
        for o in 0..(width as u16) {
            w.ldi_p_var_location(src, o, true);
            w.ld(A);
            w.not(A);
            w.st(A);
        }
    } else {
        let words = width as u16 / 2;
        for wo in 0..words {
            w.ldi_p_var_location(src, wo * 2, true);
            w.ld(A);
            w.ldi_p_var_location(src, wo * 2 + 1, true);
            w.ld(B);
            w.not(A);
            w.not(B);
            w.ldi_p_var_location(dst, wo * 2, true);
            w.st(A);
            w.ldi_p_var_location(dst, wo * 2 + 1, true);
            w.st(B);
        }
    }
    for o in 0..(width as u16) {
        w.ldi_p_var_location(dst, o, false);
        w.ld(A);
        if o == 0 {
            w.inc(A);
        } else {
            w.adc(A, Zero);
        }
        w.st(A);
    }
}
