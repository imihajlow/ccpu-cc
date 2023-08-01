use crate::{
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

fn load_addr(w: &mut InstructionWriter, addr: &generic_ir::Scalar<FrameReg>, offset: u16) {
    assert!(offset < 256);

    use crate::ccpu::instr::Reg::*;
    match addr {
        Scalar::ConstInt(x) => w.ldi_p_const(*x as u16 + offset, true),
        Scalar::SymbolOffset(id, sym_offset) => {
            let label = get_global_var_label(&id);
            w.ldi_p_sym(label, sym_offset + offset);
        }
        Scalar::Var(v) => {
            w.ldi_p_var_location(&v, 0, true);
            w.ld(A);
            w.inc(PL);
            w.ld(PH);
            if offset != 0 {
                w.ldi_const(PL, offset as u8, true);
                w.add(PL, A);
            } else {
                w.mov(PL, A);
            }
        }
    }
}
