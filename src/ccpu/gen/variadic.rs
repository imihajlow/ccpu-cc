use crate::{
    ccpu::{
        instr::InstructionWriter,
        reg::{FrameReg, VA_ARGS_START_INDEX},
        stack::{SP0_ADDR, SP1_ADDR},
    },
    generic_ir::{Scalar, VaArgOp, VaListIncOp, VaStartOp, VarLocation, Width},
};

use super::add::gen_add_reg_const;

/*
va_list is implemented as two bytes:
- first byte is the offset from the variadic arguments base (32 variadic arguments max).
- second byte is the page number.
*/

pub fn gen_va_start(w: &mut InstructionWriter, op: &VaStartOp<FrameReg>) {
    use crate::ccpu::instr::Reg::*;

    w.ldi_p_const(SP0_ADDR, true);
    w.ld(B);
    w.ldi_p_var_location(&op.dst, 1, true);
    w.st(B);
    // va_list is incremented before an argument is taken
    let offset = (256 - 8) as u8;
    w.ldi_const(A, offset, true);
    w.dec(PL);
    w.st(A);
}

pub fn gen_va_list_inc(w: &mut InstructionWriter, op: &VaListIncOp<FrameReg>) {
    let src_reg = if let Scalar::Var(v) = &op.src {
        v
    } else {
        unimplemented!("const va_list");
    };
    gen_add_reg_const(w, &op.dst, src_reg, 8, Width::Byte);
}

pub fn gen_va_arg(w: &mut InstructionWriter, op: &VaArgOp<FrameReg>) {
    use crate::ccpu::instr::Reg::*;

    let dst = &op.dst;
    let use_frame_b = match dst {
        VarLocation::Return | VarLocation::Global(_) => true,
        VarLocation::Local(reg) if reg.is_frame_b() => false,
        VarLocation::Local(_) => true,
    };
    let src_reg = if let Scalar::Var(v) = &op.src_va_list {
        v
    } else {
        unimplemented!("const va_list");
    };

    // Load va_list
    w.ldi_p_var_location(src_reg, 1, true);
    w.ld(B);
    w.dec(PL);
    w.ld(A);

    // b - page number, a - offset
    // Switch page
    if use_frame_b {
        w.ldi_p_const(SP1_ADDR, true);
        w.st(B);
    } else {
        w.ldi_p_const(SP0_ADDR, true);
        w.st(B);
    }

    let base = if use_frame_b {
        FrameReg::FrameB(VA_ARGS_START_INDEX as u16)
    } else {
        FrameReg::FrameA(VA_ARGS_START_INDEX as u16)
    }
    .get_address();
    assert_eq!(base & 0xff, 0);

    let base_hi = (base >> 8) as u8;

    // Copy
    for offset in 0..(op.width as u16) {
        if offset != 0 {
            w.inc(A);
        }
        w.ldi_const(PH, base_hi, true);
        w.mov(PL, A);
        w.ld(B);
        w.ldi_p_var_location(&op.dst, offset, true);
        w.st(B);
    }

    // Switch page back
    if use_frame_b {
        w.ldi_p_const(SP0_ADDR, true);
        w.ld(B);
        w.inc(B);
        w.ldi_p_const(SP1_ADDR, true);
        w.st(B);
    } else {
        w.ldi_p_const(SP1_ADDR, true);
        w.ld(B);
        w.dec(B);
        w.ldi_p_const(SP0_ADDR, true);
        w.st(B);
    }
}
