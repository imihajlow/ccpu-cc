use crate::{
    ccpu::{
        gen::util::load_addr, global::get_global_var_label, instr::InstructionWriter, reg::FrameReg,
    },
    generic_ir::{self, GlobalVarId, Scalar, VarLocation, Width},
};

pub fn gen_store(w: &mut InstructionWriter, op: &generic_ir::StoreOp<FrameReg>) {
    match &op.src {
        Scalar::ConstInt(x) => gen_store_const(w, &op.dst_addr, *x, op.width),
        Scalar::SymbolOffset(id, offset) => gen_store_sym(w, &op.dst_addr, id, *offset, op.width),
        Scalar::Var(v) => gen_store_var(w, &op.dst_addr, v, op.width),
    }
}

fn gen_store_const(
    w: &mut InstructionWriter,
    dst_addr: &generic_ir::Scalar<FrameReg>,
    mut src: u64,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    load_addr(w, dst_addr, 0, false);
    for i in 0..(width as u16) {
        if i != 0 {
            w.inc(PL);
        }
        w.ldi_const(A, src as u8, true);
        w.st(A);
        src >>= 8;
    }
}

fn gen_store_sym(
    w: &mut InstructionWriter,
    dst_addr: &generic_ir::Scalar<FrameReg>,
    src_id: &GlobalVarId,
    src_offset: u16,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    load_addr(w, dst_addr, 0, false);
    for i in 0..(width as u16) {
        if i != 0 {
            w.inc(PL);
        }
        match i {
            0 => w.ldi_lo(A, get_global_var_label(src_id), src_offset),
            1 => w.ldi_hi(A, get_global_var_label(src_id), src_offset),
            _ => w.ldi_const(A, 0, true),
        }
        w.st(A);
    }
}

fn gen_store_var(
    w: &mut InstructionWriter,
    dst_addr: &generic_ir::Scalar<FrameReg>,
    src: &VarLocation<FrameReg>,
    width: Width,
) {
    use crate::ccpu::instr::Reg::*;
    for i in 0..(width as u16) {
        w.ldi_p_var_location(src, i, true);
        w.ld(B);
        load_addr(w, dst_addr, i as u8, false);
        w.st(B);
    }
}
