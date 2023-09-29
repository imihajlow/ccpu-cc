use crate::{
    ccpu::{
        gen::{copy::gen_copy_var, util::load_addr},
        instr::InstructionWriter,
        reg::{FrameReg, VA_ARGS_START_INDEX},
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
    for (i, (arg, _)) in op.va_args.iter().enumerate() {
        match arg {
            Scalar::Var(VarLocation::Local(FrameReg::FrameB(n)))
                if *n == (i + VA_ARGS_START_INDEX) as u16 =>
            {
                ()
            }
            _ => panic!("Call variadic arguments are not correctly hinted by register allocator!"),
        }
    }
    load_addr(w, &op.addr, 0, false);
    w.jmp();
    if let Some((ret_location, ret_width)) = &op.dst {
        gen_copy_var(w, ret_location, &VarLocation::Return, *ret_width);
    }
}
