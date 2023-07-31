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
    let width = width as u16;
    for offset in 0..width {
        w.ldi_p_var_location(r, offset, offset == 0);
        w.ld(B);
        w.ldi_const(A, (c & 0xff) as u8, offset == 0);
        if offset == 0 {
            w.add(B, A);
        } else {
            w.adc(B, A);
        }
        w.ldi_p_var_location(dst, offset, offset == width - 1);
        w.st(B);

        c >>= 8;
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
