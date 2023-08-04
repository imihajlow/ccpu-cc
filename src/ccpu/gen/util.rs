use crate::{
    ccpu::{global::get_global_var_label, instr::InstructionWriter, reg::FrameReg},
    generic_ir::{self, Scalar},
};

/**
 * Load address into P from scalar. B is not used.
 */
pub fn load_addr(w: &mut InstructionWriter, addr: &generic_ir::Scalar<FrameReg>, offset: u16) {
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

/**
 * Load a byte from scalar into destination register.
 */
pub fn load_scalar(
    w: &mut InstructionWriter,
    dst: crate::ccpu::instr::Reg,
    s: &Scalar<FrameReg>,
    offset: u16,
    allow_incdec: bool,
) {
    match s {
        Scalar::ConstInt(x) => w.ldi_const(dst, (x >> (offset * 8)) as u8, allow_incdec),
        Scalar::SymbolOffset(id, o) => {
            let sym = get_global_var_label(id);
            match offset {
                0 => w.ldi_lo(dst, sym, *o),
                1 => w.ldi_hi(dst, sym, *o),
                _ => w.ldi_const(dst, 0, allow_incdec),
            }
        }
        Scalar::Var(v) => {
            w.ldi_p_var_location(v, offset, allow_incdec);
            w.ld(dst);
        }
    }
}
