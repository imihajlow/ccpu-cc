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

    w.ldi_p_sym(get_intrin_sym(&op.name), 0);
    w.jmp();
}

pub fn gen_intrin_imports(w: &mut InstructionWriter) {
    w.import(get_intrin_sym("mul_byte"));
    w.import(get_intrin_sym("mul_word"));
    w.import(get_intrin_sym("mul_dword"));
    w.import(get_intrin_sym("udiv_byte"));
    w.import(get_intrin_sym("udiv_word"));
    w.import(get_intrin_sym("div_byte"));
    w.import(get_intrin_sym("div_word"));
    w.import(get_intrin_sym("umod_byte"));
    w.import(get_intrin_sym("umod_word"));
    w.import(get_intrin_sym("mod_byte"));
    w.import(get_intrin_sym("mod_word"));
    w.import(get_intrin_sym("asl_byte"));
    w.import(get_intrin_sym("asl_word"));
    w.import(get_intrin_sym("asl_dword"));
    w.import(get_intrin_sym("asr_word"));
    w.import(get_intrin_sym("asr_dword"));
    w.import(get_intrin_sym("lsr_word"));
    w.import(get_intrin_sym("lsr_dword"));
}

fn get_intrin_sym(name: &str) -> String {
    format!("__cc_{}", name)
}
