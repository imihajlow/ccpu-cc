use crate::{
    ccpu::gen::util::load_addr,
    ccpu::{global::get_global_var_label, instr::InstructionWriter, reg::FrameReg},
    generic_ir::{self, Scalar, Width},
};

pub fn gen_load(w: &mut InstructionWriter, op: &generic_ir::LoadOp<FrameReg>) {
    use crate::ccpu::instr::Reg::*;

    if op.width == Width::Byte {
        load_addr(w, &op.src_addr, 0);
        w.ld(A);
        w.ldi_p_var_location(&op.dst, 0, true);
        w.st(A);
    } else {
        let words = op.width as u16 / 2;
        // Reverse order to allow loads to same register
        for word_offset in (0..words).rev() {
            load_addr(w, &op.src_addr, (word_offset * 2) as u16);
            w.ld(A);
            w.inc(PL);
            w.ld(B);
            w.ldi_p_var_location(&op.dst, word_offset * 2, true);
            w.st(A);
            w.inc(PL);
            w.st(B);
        }
    }
}
