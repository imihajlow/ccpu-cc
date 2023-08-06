use crate::{
    ccpu::{instr::InstructionWriter, reg::FrameReg},
    generic_ir::{self, IntrinCallVariant, Scalar, VarLocation},
};

pub fn gen_intrin_call(w: &mut InstructionWriter, op: &generic_ir::IntrinCallOp<FrameReg>) {
    match op.variant {
        IntrinCallVariant::Call2R(
            (_, VarLocation::Local(FrameReg::IntrinsicRet)),
            (_, Scalar::Var(VarLocation::Local(FrameReg::IntrinsicArg1))),
            (_, Scalar::Var(VarLocation::Local(FrameReg::IntrinsicArg2))),
        ) => (),
        IntrinCallVariant::Call3(
            (_, Scalar::Var(VarLocation::Local(FrameReg::IntrinsicArg1))),
            (_, Scalar::Var(VarLocation::Local(FrameReg::IntrinsicArg2))),
            (_, Scalar::Var(VarLocation::Local(FrameReg::IntrinsicArg3))),
        ) => (),
        _ => panic!("Intrinsic call arguments are not correctly hinted by register allocator!"),
    }

    w.ldi_p_sym(format!("__cc_{}", op.name), 0);
    w.jmp();
}
