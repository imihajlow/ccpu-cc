use crate::{
    ccpu::{global::get_global_var_label, instr::InstructionWriter, reg::FrameReg},
    generic_ir::{self, GlobalVarId, Scalar, VarLocation, Width},
};

pub fn gen_add(w: &mut InstructionWriter, op: &generic_ir::BinaryOp<FrameReg>) {
    match (&op.lhs, &op.rhs) {
        (Scalar::Var(v1), Scalar::Var(v2)) => gen_add_reg_reg(w, &op.dst, v1, v2, op.width),
        (Scalar::Var(v), Scalar::ConstInt(c)) | (Scalar::ConstInt(c), Scalar::Var(v)) => {
            gen_add_reg_const(w, &op.dst, v, *c, op.width)
        }
        (Scalar::Var(v), Scalar::SymbolOffset(s, o))
        | (Scalar::SymbolOffset(s, o), Scalar::Var(v)) => {
            gen_add_reg_sym(w, &op.dst, v, s, *o, op.width)
        }
        (Scalar::ConstInt(_), Scalar::ConstInt(_))
        | (Scalar::ConstInt(_), Scalar::SymbolOffset(_, _))
        | (Scalar::SymbolOffset(_, _), Scalar::ConstInt(_)) => {
            unreachable!("const propagation must be performed before emitting code")
        }
        (Scalar::SymbolOffset(_, _), Scalar::SymbolOffset(_, _)) => {
            unimplemented!("doesn't make sense")
        }
    }
}

pub fn gen_add_reg_reg(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src1: &VarLocation<FrameReg>,
    src2: &VarLocation<FrameReg>,
    width: Width,
) {
    match width {
        Width::Byte => gen_add_reg_reg_byte(w, dst, src1, src2),
        Width::Word => gen_add_reg_reg_word(w, dst, src1, src2),
        Width::Dword => gen_add_reg_reg_long(w, dst, src1, src2, width as u16),
        Width::Qword => gen_add_reg_reg_long(w, dst, src1, src2, width as u16),
    }
}

pub fn gen_add_reg_const(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    r: &VarLocation<FrameReg>,
    mut c: u64,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    if width == Width::Byte {
        return gen_add_reg_const_byte(w, dst, r, c);
    }
    if width == Width::Word {
        return gen_add_reg_const_word(w, dst, r, c);
    }
    let width = width as u16;
    let mut carry_significant = false;
    for offset in 0..width {
        let cur_byte = (c & 0xff) as u8;
        if cur_byte == 0 {
            if carry_significant {
                w.ldi_p_var_location(r, offset, false);
                w.ld(A);
                w.adc(A, Zero);
                w.ldi_p_var_location(dst, offset, false);
                w.st(A);
            } else if dst != r {
                w.ldi_p_var_location(r, offset, true);
                w.ld(B);
                w.ldi_p_var_location(dst, offset, true);
                w.st(B);
            }
        } else {
            if carry_significant {
                w.ldi_p_var_location(r, offset, false);
                w.ld(B);
                w.ldi_const(A, cur_byte, false);
                w.adc(B, A);
                w.ldi_p_var_location(dst, offset, false);
                w.st(B);
            } else {
                if cur_byte == 1 {
                    w.ldi_p_var_location(r, offset, true);
                    w.ld(B);
                    w.inc(B);
                    w.ldi_p_var_location(dst, offset, false);
                    w.st(B);
                } else {
                    w.ldi_p_var_location(r, offset, false);
                    w.ld(B);
                    w.ldi_const(A, cur_byte, false);
                    w.add(B, A);
                    w.ldi_p_var_location(dst, offset, false);
                    w.st(B);
                }
                carry_significant = true;
            }
        }

        c >>= 8;
    }
}

fn gen_add_reg_const_byte(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    r: &VarLocation<FrameReg>,
    c: u64,
) {
    use crate::ccpu::instr::Reg::*;
    let c = (c & 0xff) as u8;
    if c == 0 && dst == r {
        // nothing to do
        return;
    }
    w.ldi_p_var_location(r, 0, true);
    w.ld(B);
    match c {
        0 => (),
        1 => w.inc(B),
        0xff => w.dec(B),
        _ => {
            w.ldi_const(A, c, true);
            w.add(B, A);
        }
    }
    w.ldi_p_var_location(dst, 0, true);
    w.st(B);
}

fn gen_add_reg_const_word(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    r: &VarLocation<FrameReg>,
    c: u64,
) {
    use crate::ccpu::instr::Reg::*;

    let c = (c & 0xffff) as u16;
    if dst == r {
        return gen_add_reg_const_word_inplace(w, dst, c);
    }
    let lo = (c & 0xff) as u8;
    let hi = (c >> 8) as u8;
    if lo == 0 {
        w.ldi_p_var_location(r, 0, true);
        w.ld(A);
        w.inc(PL);
        w.ld(B);
        w.ldi_p_var_location(dst, 0, true);
        w.st(A);
        w.inc(PL);
        match hi {
            0 => (),
            1 => w.inc(B),
            0xff => w.dec(B),
            _ => {
                w.ldi_const(A, hi, true);
                w.add(B, A)
            }
        }
        w.st(B);
    } else if hi == 0 {
        w.ldi_p_var_location(r, 0, true);
        w.ld(B);
        w.inc(PL);
        match lo {
            0 => unreachable!(),
            1 => w.inc(B),
            _ => {
                w.ldi_const(A, lo, true);
                w.add(B, A);
            }
        }
        w.ld(A);
        w.adc(A, Zero);
        w.ldi_p_var_location(dst, 1, true);
        w.st(A);
        w.dec(PL);
        w.st(B);
    } else {
        w.ldi_p_var_location(r, 0, true);
        w.ld(B);
        w.inc(PL);
        match lo {
            0 => unreachable!(),
            1 => w.inc(B),
            _ => {
                w.ldi_const(A, lo, true);
                w.add(B, A);
            }
        }
        w.ld(A);
        w.ldi_const(PL, hi, false);
        w.adc(A, PL);
        w.ldi_p_var_location(dst, 1, true);
        w.st(A);
        w.dec(PL);
        w.st(B);
    }
}

fn gen_add_reg_const_word_inplace(w: &mut InstructionWriter, dst: &VarLocation<FrameReg>, c: u16) {
    use crate::ccpu::instr::Reg::*;
    if c == 0 {
        // nothing to do
        return;
    }
    let lo = (c & 0xff) as u8;
    let hi = (c >> 8) as u8;
    if lo == 0 {
        w.ldi_p_var_location(dst, 1, true);
        w.ld(B);
        match hi {
            0 => unreachable!(),
            1 => w.inc(B),
            0xff => w.dec(B),
            _ => {
                w.ldi_const(A, hi, true);
                w.add(B, A);
            }
        }
        w.st(B);
    } else if hi == 0 {
        w.ldi_p_var_location(dst, 0, true);
        w.ld(B);
        w.inc(PL);
        if lo == 1 {
            w.inc(B);
        } else {
            w.ldi_const(A, lo, true);
            w.add(B, A);
        }
        w.ld(A);
        w.adc(A, Zero);
        w.st(A);
        w.dec(PL);
        w.st(B);
    } else {
        w.ldi_p_var_location(dst, 0, true);
        w.ld(B);
        if lo == 1 {
            w.inc(B);
        } else {
            w.ldi_const(A, lo, true);
            w.add(B, A);
        }
        w.st(B);
        w.ldi_pl_var_location_lo(dst, 1, false);
        w.ld(B);
        w.ldi_const(A, hi, false);
        w.adc(B, A);
        w.st(B);
    }
}

fn gen_add_reg_sym(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    r: &VarLocation<FrameReg>,
    sym: &GlobalVarId,
    sym_offset: u16,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    let width = width as u16;

    let sym = get_global_var_label(sym);

    for offset in 0..width {
        w.ldi_p_var_location(r, offset, offset == 0);
        w.ld(B);
        match offset {
            0 => w.ldi_lo(A, sym.clone(), sym_offset),
            1 => w.ldi_hi(A, sym.clone(), sym_offset),
            _ => w.ldi_const(A, 0, false),
        }
        if offset == 0 {
            w.add(B, A);
        } else {
            w.adc(B, A);
        }
        w.ldi_p_var_location(dst, offset, false);
        w.st(B);
    }
}

fn gen_add_reg_reg_byte(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src1: &VarLocation<FrameReg>,
    src2: &VarLocation<FrameReg>,
) {
    use crate::ccpu::instr::Reg::*;
    let (src1, src2) = if src1 == dst {
        (src2, src1)
    } else {
        (src1, src2)
    };
    w.ldi_p_var_location(src1, 0, true);
    w.ld(A);
    w.ldi_p_var_location(src2, 0, true);
    w.ld(B);
    w.add(A, B);
    w.ldi_p_var_location(dst, 0, true);
    w.st(A);
}

fn gen_add_reg_reg_word(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src1: &VarLocation<FrameReg>,
    src2: &VarLocation<FrameReg>,
) {
    use crate::ccpu::instr::Reg::*;
    let (src1, src2) = if src1 == dst {
        (src2, src1)
    } else {
        (src1, src2)
    };
    w.ldi_p_var_location(src1, 1, true);
    w.ld(B);
    w.dec(PL);
    w.ld(A);
    w.ldi_p_var_location(src2, 0, true);
    w.ld(PL);
    w.add(A, PL);
    w.ldi_p_var_location(dst, 0, false);
    w.st(A);
    w.ldi_p_var_location(src2, 1, false);
    w.ld(A);
    w.adc(A, B);
    w.ldi_p_var_location(dst, 1, true);
    w.st(A);
}

fn gen_add_reg_reg_long(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src1: &VarLocation<FrameReg>,
    src2: &VarLocation<FrameReg>,
    width: u16,
) {
    use crate::ccpu::instr::Reg::*;
    let (src1, src2) = if src1 == dst {
        (src2, src1)
    } else {
        (src1, src2)
    };
    w.ldi_p_var_location(src1, 0, true);
    w.ld(A);
    w.ldi_p_var_location(src2, 0, true);
    w.ld(B);
    w.ldi_p_var_location(dst, 0, true);
    w.add(A, B);
    w.st(A);
    for offset in 1..width {
        w.ldi_p_var_location(src1, offset, false);
        w.ld(A);
        w.ldi_p_var_location(src2, offset, false);
        w.ld(B);
        w.adc(A, B);
        w.ldi_p_var_location(dst, offset, offset == width - 1);
        w.st(A);
    }
}
