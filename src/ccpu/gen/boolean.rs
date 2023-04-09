use crate::{
    ccpu::{instr::InstructionWriter, reg::FrameReg},
    generic_ir::{self, Scalar, Width},
};

pub fn gen_bool(w: &mut InstructionWriter, op: &generic_ir::UnaryUnsignedOp<FrameReg>) {
    gen_bool_common(w, op, false);
}

pub fn gen_bool_inv(w: &mut InstructionWriter, op: &generic_ir::UnaryUnsignedOp<FrameReg>) {
    gen_bool_common(w, op, true);
}

fn gen_bool_common(
    w: &mut InstructionWriter,
    op: &generic_ir::UnaryUnsignedOp<FrameReg>,
    inv: bool,
) {
    use crate::ccpu::instr::Reg::*;
    if let Scalar::Var(v) = &op.src {
        w.ldi_p_var_location(v, 0, true);
    } else {
        unreachable!("const propagation must be performed before emitting code");
    }
    w.ld(A);
    if op.width == Width::Byte {
        w.add(A, Zero);
    } else {
        for _ in 1..(op.width as usize) {
            w.inc(PL);
            w.ld(B);
            w.or(A, B);
        }
    }
    if inv {
        let label_zero = w.alloc_label();
        w.ldi_p_sym(label_zero.clone(), 0);
        w.jz();
        w.ldi_const(A, 0xff, true);
        w.label(label_zero);
        w.inc(A);
    } else {
        let label = w.alloc_label();
        w.ldi_p_sym(label.clone(), 0);
        w.jz();
        w.ldi_const(A, 1, true);
        w.label(label);
    }
    w.ldi_p_var_location(&op.dst, 0, true);
    w.st(A);
}
