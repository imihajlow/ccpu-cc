use crate::{
    ccpu::{instr::InstructionWriter, reg::FrameReg},
    generic_ir::{self, Scalar},
};

use super::util::load_addr;

pub fn gen_memcpy(w: &mut InstructionWriter, op: &generic_ir::MemcpyOp<FrameReg>) {
    if op.len >= 256 {
        unimplemented!("inline memcpy for size over 255 bytes");
    }
    match &op.dst_addr {
        Scalar::Var(_) => gen_memcpy_dst_var(w, &op.dst_addr, &op.src_addr, op.len as u8),
        _ => gen_memcpy_dst_const(w, &op.dst_addr, &op.src_addr, op.len as u8),
    }
}

fn gen_memcpy_dst_var(
    w: &mut InstructionWriter,
    dst_addr: &Scalar<FrameReg>,
    src_addr: &Scalar<FrameReg>,
    len: u8,
) {
    use crate::ccpu::instr::Reg::*;
    for offset in 0..len {
        load_addr(w, src_addr, offset, true);
        w.ld(B);
        load_addr(w, dst_addr, offset, true);
        w.st(B);
    }
}

fn gen_memcpy_dst_const(
    w: &mut InstructionWriter,
    dst_addr: &Scalar<FrameReg>,
    src_addr: &Scalar<FrameReg>,
    len: u8,
) {
    use crate::ccpu::instr::Reg::*;
    if let Scalar::Var(_) = dst_addr {
        panic!("This function must be used with constants dst_addr only");
    }
    for word_offset in 0..len / 2 {
        let offset = word_offset * 2;
        load_addr(w, src_addr, offset, true);
        w.ld(B);
        w.ldi_const(A, 0, true);
        w.inc(PL);
        w.adc(PH, A);
        w.ld(A);
        load_addr(w, dst_addr, offset + 1, true);
        w.st(A);
        w.ldi_const(A, 0, true);
        w.dec(PL);
        w.sbb(PH, A);
        w.st(B);
    }

    if len % 2 == 1 {
        load_addr(w, src_addr, len - 1, true);
        w.ld(B);
        load_addr(w, src_addr, len - 1, true);
        w.st(B);
    }
}
