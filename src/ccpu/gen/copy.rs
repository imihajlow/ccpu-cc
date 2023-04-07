use crate::{
    ccpu::{global::get_global_var_label, instr::InstructionWriter, reg::FrameReg},
    generic_ir::{self, GlobalVarId, Scalar, VarLocation, Width},
};

pub fn gen_copy(w: &mut InstructionWriter, op: &generic_ir::UnaryUnsignedOp<FrameReg>) {
    match &op.src {
        Scalar::ConstInt(c) => gen_copy_const(w, &op.dst, *c, op.width),
        Scalar::Var(v) => gen_copy_var(w, &op.dst, &v, op.width),
        Scalar::SymbolOffset(s, o) => gen_copy_sym(w, &op.dst, &s, *o, op.width),
    }
}

fn gen_copy_const(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    mut src: u64,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    let width = width as u16;
    w.ldi_p_var_location(dst, 0);
    for i in 0..width {
        let b = (src & 0xff) as u8;
        w.ldi_const(A, b);
        w.st(A);
        if i != width - 1 {
            w.inc(PL);
        }
        src >>= 8;
    }
}

fn gen_copy_var(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src: &VarLocation<FrameReg>,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    if dst == src {
        return;
    }
    if width == Width::Byte {
        w.ldi_p_var_location(src, 0);
        w.ld(A);
        w.ldi_p_var_location(dst, 0);
        w.st(A);
    } else {
        let width = width as u16;
        let words = width / 2;
        for i in 0..words {
            w.ldi_p_var_location(src, i * 2);
            w.ld(A);
            w.inc(PL);
            w.ld(B);
            w.ldi_p_var_location(dst, i * 2);
            w.st(A);
            w.inc(PL);
            w.st(B);
        }
    }
}

fn gen_copy_sym(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src_sym: &GlobalVarId,
    offset: u16,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    w.ldi_p_var_location(dst, 0);
    w.ldi_lo(A, get_global_var_label(src_sym), offset);
    w.st(A);
    if width > Width::Byte {
        w.inc(PL);
        w.ldi_hi(A, get_global_var_label(src_sym), offset);
        w.st(A);
        let width = width as u16;
        let rest = width - 2;
        if rest > 0 {
            w.mov(A, Zero);
            for _ in 0..rest {
                w.inc(PL);
                w.st(A);
            }
        }
    }
}
