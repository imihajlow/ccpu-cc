use crate::{
    ccpu::{global::get_global_var_label, instr::InstructionWriter, reg::FrameReg},
    generic_ir::{self, GlobalVarId, Scalar, VarLocation, Width},
};

pub fn gen_sub(w: &mut InstructionWriter, op: &generic_ir::BinaryOp<FrameReg>) {
    match (&op.lhs, &op.rhs) {
        (Scalar::Var(v1), Scalar::Var(v2)) => gen_sub_reg_reg(w, &op.dst, v1, v2, op.width),
        (Scalar::Var(v), Scalar::ConstInt(c)) => gen_sub_reg_const(w, &op.dst, v, *c, op.width),
        (Scalar::ConstInt(c), Scalar::Var(v)) => gen_sub_const_reg(w, &op.dst, *c, v, op.width),
        (Scalar::Var(v), Scalar::SymbolOffset(s, o)) => {
            gen_sub_reg_sym(w, &op.dst, v, s, *o, op.width)
        }
        (Scalar::SymbolOffset(s, o), Scalar::Var(v)) => {
            gen_sub_sym_reg(w, &op.dst, s, *o, v, op.width)
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

pub fn gen_sub_reg_reg(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src1: &VarLocation<FrameReg>,
    src2: &VarLocation<FrameReg>,
    width: Width,
) {
    match width {
        Width::Byte => gen_sub_reg_reg_byte(w, dst, src1, src2),
        Width::Word => gen_sub_reg_reg_word(w, dst, src1, src2),
        Width::Dword => gen_sub_reg_reg_long(w, dst, src1, src2, width as u16),
        Width::Qword => gen_sub_reg_reg_long(w, dst, src1, src2, width as u16),
    }
}

pub fn gen_sub_reg_const(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    r: &VarLocation<FrameReg>,
    mut c: u64,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    let width = width as u16;
    for offset in 0..width {
        w.ldi_p_var_location(r, offset, offset == 0);
        w.ld(A);
        let cur_byte = (c & 0xff) as u8;
        if cur_byte == 1 && offset == 0 {
            w.dec(B);
        } else if cur_byte == 0 && offset == 0 {
            w.sub(A, Zero);
        } else if cur_byte == 0 && offset != 0 {
            w.sbb(A, Zero);
        } else {
            w.ldi_const(B, cur_byte, offset == 0);
            if offset == 0 {
                w.sub(A, B);
            } else {
                w.sbb(A, B);
            }
        }
        w.ldi_p_var_location(dst, offset, offset == width - 1);
        w.st(A);

        c >>= 8;
    }
}

pub fn gen_sub_const_reg(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    mut c: u64,
    r: &VarLocation<FrameReg>,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    let width = width as u16;
    for offset in 0..width {
        w.ldi_p_var_location(r, offset, offset == 0);
        w.ld(B);
        w.ldi_const(A, (c & 0xff) as u8, offset == 0);
        if offset == 0 {
            w.sub(A, B);
        } else {
            w.sbb(A, B);
        }
        w.ldi_p_var_location(dst, offset, offset == width - 1);
        w.st(A);

        c >>= 8;
    }
}

fn gen_sub_sym_reg(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    sym: &GlobalVarId,
    sym_offset: u16,
    r: &VarLocation<FrameReg>,
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
            w.sub(A, B);
        } else {
            w.sbb(A, B);
        }
        w.ldi_p_var_location(dst, offset, false);
        w.st(A);
    }
}

fn gen_sub_reg_sym(
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
            w.sub(B, A);
        } else {
            w.sbb(B, A);
        }
        w.ldi_p_var_location(dst, offset, false);
        w.st(B);
    }
}

fn gen_sub_reg_reg_byte(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    lhs: &VarLocation<FrameReg>,
    rhs: &VarLocation<FrameReg>,
) {
    use crate::ccpu::instr::Reg::*;
    if lhs == dst {
        w.ldi_p_var_location(rhs, 0, true);
        w.ld(B);
        w.ldi_p_var_location(lhs, 0, true);
        w.ld(A);
    } else {
        w.ldi_p_var_location(lhs, 0, true);
        w.ld(A);
        w.ldi_p_var_location(rhs, 0, true);
        w.ld(B);
    }
    w.sub(A, B);
    w.ldi_p_var_location(dst, 0, true);
    w.st(A);
}

fn gen_sub_reg_reg_word(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    lhs: &VarLocation<FrameReg>,
    rhs: &VarLocation<FrameReg>,
) {
    use crate::ccpu::instr::Reg::*;
    // TODO swap loads to save one instruction when lhs == dst?
    w.ldi_p_var_location(lhs, 1, true);
    w.ld(B);
    w.dec(PL);
    w.ld(A);
    w.ldi_p_var_location(rhs, 0, true);
    w.ld(PL);
    w.sub(A, PL);
    w.ldi_p_var_location(dst, 0, false);
    w.st(A);
    w.ldi_p_var_location(rhs, 1, false);
    w.ld(A);
    w.sbb(B, A);
    w.ldi_p_var_location(dst, 1, true);
    w.st(B);
}

fn gen_sub_reg_reg_long(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    lhs: &VarLocation<FrameReg>,
    rhs: &VarLocation<FrameReg>,
    width: u16,
) {
    use crate::ccpu::instr::Reg::*;
    let swap_load = lhs == dst;
    if swap_load {
        w.ldi_p_var_location(rhs, 0, true);
        w.ld(B);
        w.ldi_p_var_location(lhs, 0, true);
        w.ld(A);
    } else {
        w.ldi_p_var_location(lhs, 0, true);
        w.ld(A);
        w.ldi_p_var_location(rhs, 0, true);
        w.ld(B);
    }
    w.ldi_p_var_location(dst, 0, true);
    w.sub(A, B);
    w.st(A);
    for offset in 1..width {
        if swap_load {
            w.ldi_p_var_location(rhs, offset, false);
            w.ld(B);
            w.ldi_p_var_location(lhs, offset, false);
            w.ld(A);
        } else {
            w.ldi_p_var_location(lhs, offset, false);
            w.ld(A);
            w.ldi_p_var_location(rhs, offset, false);
            w.ld(B);
        }
        w.sbb(A, B);
        w.ldi_p_var_location(dst, offset, offset == width - 1);
        w.st(A);
    }
}
