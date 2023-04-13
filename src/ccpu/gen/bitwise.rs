use crate::{
    ccpu::{
        global::get_global_var_label,
        instr::{InstructionWriter, Reg},
        reg::FrameReg,
    },
    generic_ir::{self, GlobalVarId, Scalar, VarLocation, Width},
};

pub fn gen_bitwise_and(w: &mut InstructionWriter, op: &generic_ir::BinaryUnsignedOp<FrameReg>) {
    gen_bitwise_common(w, op, BitwiseOpKind::And)
}

pub fn gen_bitwise_or(w: &mut InstructionWriter, op: &generic_ir::BinaryUnsignedOp<FrameReg>) {
    gen_bitwise_common(w, op, BitwiseOpKind::Or)
}

pub fn gen_bitwise_xor(w: &mut InstructionWriter, op: &generic_ir::BinaryUnsignedOp<FrameReg>) {
    gen_bitwise_common(w, op, BitwiseOpKind::Xor)
}

pub fn gen_bitwise_not(w: &mut InstructionWriter, op: &generic_ir::UnaryUnsignedOp<FrameReg>) {
    match &op.src {
        Scalar::Var(src) => gen_bitwise_not_var(w, &op.dst, src, op.width),
        Scalar::ConstInt(_) => {
            unreachable!("const propagation must be performed before emitting code")
        }
        Scalar::SymbolOffset(_, _) => unimplemented!("wtf"),
    }
}

fn gen_bitwise_not_var(
    w: &mut InstructionWriter,
    dst: &generic_ir::VarLocation<FrameReg>,
    src: &generic_ir::VarLocation<FrameReg>,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    if dst == src {
        w.ldi_p_var_location(dst, 0, true);
        for i in 0..(width as u16) {
            if i != 0 {
                w.inc(PL);
            }
            w.ld(A);
            w.not(A);
            w.st(A);
        }
    } else {
        if width == Width::Byte {
            w.ldi_p_var_location(src, 0, true);
            w.ld(A);
            w.not(A);
            w.ldi_p_var_location(dst, 0, true);
            w.st(A);
        } else {
            for i in 0..(width as u16 / 2) {
                w.ldi_p_var_location(src, i * 2, true);
                w.ld(A);
                w.inc(PL);
                w.ld(B);
                w.not(A);
                w.not(B);
                w.ldi_p_var_location(dst, i * 2 + 1, true);
                w.st(B);
                w.dec(PL);
                w.st(A);
            }
        }
    }
}

#[derive(Copy, Clone)]
enum BitwiseOpKind {
    And,
    Or,
    Xor,
}

fn gen_bitwise_common(
    w: &mut InstructionWriter,
    op: &generic_ir::BinaryUnsignedOp<FrameReg>,
    kind: BitwiseOpKind,
) {
    match (&op.lhs, &op.rhs) {
        (Scalar::Var(lhs), Scalar::Var(rhs)) => {
            gen_bitwise_common_reg_reg(w, &op.dst, lhs, rhs, op.width, kind)
        }
        (Scalar::Var(lhs), Scalar::ConstInt(rhs)) | (Scalar::ConstInt(rhs), Scalar::Var(lhs)) => {
            gen_bitwise_common_reg_const(w, &op.dst, lhs, *rhs, op.width, kind)
        }
        (Scalar::Var(lhs), Scalar::SymbolOffset(rhs, rhs_offset))
        | (Scalar::SymbolOffset(rhs, rhs_offset), Scalar::Var(lhs)) => {
            gen_bitwise_common_reg_sym(w, &op.dst, lhs, rhs, *rhs_offset, op.width, kind)
        }
        _ => unreachable!("const propagation must be performed before emitting code"),
    }
}

fn gen_bitwise_common_reg_reg(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    lhs: &VarLocation<FrameReg>,
    rhs: &VarLocation<FrameReg>,
    width: Width,
    kind: BitwiseOpKind,
) {
    use crate::ccpu::instr::Reg::*;
    let (lhs, rhs) = if dst == lhs { (rhs, lhs) } else { (lhs, rhs) };
    let n = width as u16;
    for i in 0..n {
        w.ldi_p_var_location(lhs, i, true);
        w.ld(A);
        w.ldi_p_var_location(rhs, i, true);
        w.ld(B);
        emit_bitwise_op(w, A, B, kind);
        w.ldi_p_var_location(dst, i, true);
        w.st(A);
    }
}

fn gen_bitwise_common_reg_const(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    lhs: &VarLocation<FrameReg>,
    mut rhs: u64,
    width: Width,
    kind: BitwiseOpKind,
) {
    use crate::ccpu::instr::Reg::*;
    let n = width as u16;
    for i in 0..n {
        w.ldi_p_var_location(lhs, i, true);
        w.ld(B);
        w.ldi_const(A, (rhs & 0xff) as u8, true);
        emit_bitwise_op(w, B, A, kind);
        w.ldi_p_var_location(dst, i, true);
        w.st(B);
        rhs >>= 8;
    }
}

fn gen_bitwise_common_reg_sym(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    lhs: &VarLocation<FrameReg>,
    rhs: &GlobalVarId,
    rhs_offset: u16,
    width: Width,
    kind: BitwiseOpKind,
) {
    use crate::ccpu::instr::Reg::*;
    let n = width as u16;
    let sym = get_global_var_label(rhs);
    for i in 0..n {
        w.ldi_p_var_location(lhs, i, true);
        w.ld(B);
        match i {
            0 => w.ldi_lo(A, sym.clone(), rhs_offset),
            1 => w.ldi_hi(A, sym.clone(), rhs_offset),
            _ => w.ldi_const(A, 0, true),
        }
        emit_bitwise_op(w, B, A, kind);
        w.ldi_p_var_location(dst, i, true);
        w.st(B);
    }
}

fn emit_bitwise_op(w: &mut InstructionWriter, dst: Reg, src: Reg, kind: BitwiseOpKind) {
    match kind {
        BitwiseOpKind::And => w.and(dst, src),
        BitwiseOpKind::Or => w.or(dst, src),
        BitwiseOpKind::Xor => w.xor(dst, src),
    }
}
