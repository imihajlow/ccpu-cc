use crate::{
    ccpu::{instr::InstructionWriter, reg::FrameReg},
    generic_ir::{self, Scalar},
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
    w.ld(B);
    for _ in 1..(op.width as usize) {
        w.inc(PL);
        w.ld(A);
        w.or(B, A);
    }
    // If B was not 0, carry will be set after neg B
    if !inv {
        w.mov(A, Zero);
        w.neg(B);
        w.adc(A, Zero);
    } else {
        w.ldi_const(A, 1, true);
        w.neg(B);
        w.sbb(A, Zero);
    }
    w.ldi_p_var_location(&op.dst, 0, true);
    w.st(A);
    if op.width as usize > 1 {
        w.mov(A, Zero);
        for _ in 1..(op.width as usize) {
            w.inc(PL);
            w.st(A);
        }
    }
}
