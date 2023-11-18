use crate::generic_ir::{Scalar, Width};
use crate::{
    ccpu::{instr::InstructionWriter, reg::FrameReg},
    generic_ir::{self, VarLocation},
};

pub fn gen_bswap(w: &mut InstructionWriter, op: &generic_ir::UnaryUnsignedOp<FrameReg>) {
    match &op.src {
        Scalar::Var(src) => gen_bswap_var(w, &op.dst, src, op.width),
        Scalar::ConstInt(_) => {
            unreachable!("const propagation must be performed before emitting code")
        }
        Scalar::SymbolOffset(_, _) => unimplemented!("wtf"),
    }
}

fn gen_bswap_var(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src: &VarLocation<FrameReg>,
    width: Width,
) {
    match width {
        Width::Byte => unimplemented!("bswap8"),
        Width::Word => gen_bswap_var_word(w, dst, src),
        Width::Dword => gen_bswap_var_dword(w, dst, src),
        Width::Qword => todo!("bswap64"),
    }
}

fn gen_bswap_var_word(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src: &VarLocation<FrameReg>,
) {
    use crate::ccpu::instr::Reg::*;
    w.ldi_p_var_location(src, 0, true);
    w.ld(A);
    w.inc(PL);
    w.ld(B);
    w.ldi_p_var_location(dst, 1, true);
    w.st(A);
    w.dec(PL);
    w.st(B);
}

fn gen_bswap_var_dword(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src: &VarLocation<FrameReg>,
) {
    use crate::ccpu::instr::Reg::*;
    w.ldi_p_var_location(src, 1, true);
    w.ld(A);
    w.inc(PL);
    w.ld(B);
    w.ldi_p_var_location(dst, 2, true);
    w.st(A);
    w.dec(PL);
    w.st(B);
    w.ldi_p_var_location(src, 0, true);
    w.ld(A);
    w.ldi_p_var_location(src, 3, true);
    w.ld(B);
    w.ldi_p_var_location(dst, 3, true);
    w.st(A);
    w.ldi_p_var_location(dst, 0, true);
    w.st(B);
}
