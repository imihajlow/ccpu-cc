use crate::{
    ccpu::{
        gen::{copy::gen_copy_var, util::load_addr},
        instr::InstructionWriter,
        reg::FrameReg,
    },
    generic_ir::{self, Scalar, VarLocation},
};

pub fn gen_call(w: &mut InstructionWriter, op: &generic_ir::CallOp<FrameReg>) {
    // check argument registers
    for (i, (arg, _)) in op.args.iter().enumerate() {
        match arg {
            Scalar::Var(VarLocation::Local(FrameReg::FrameB(n))) if *n == i as u16 => (),
            _ => panic!("Call arguments are not correctly hinted by register allocator!"),
        }
    }
    load_addr(w, &op.addr, 0);
    w.jmp();
    if let Some((ret_location, ret_width)) = &op.dst {
        gen_copy_var(w, ret_location, &VarLocation::Return, *ret_width);
    }
}
