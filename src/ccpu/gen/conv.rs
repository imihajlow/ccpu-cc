use crate::{
    ccpu::{global::get_global_var_label, instr::InstructionWriter, reg::FrameReg},
    generic_ir::{self, GlobalVarId, Scalar, UnaryUnsignedOp, VarLocation, Width},
};

use super::copy::gen_copy;

struct ConvInfo {
    dst_width: Width,
    src_width: Width,
    dst_sign: bool,
    src_sign: bool,
}

pub fn gen_conv(w: &mut InstructionWriter, op: &generic_ir::ConvOp<FrameReg>) {
    let info = ConvInfo::new(op);

    if info.is_widening() {
        match &op.src {
            Scalar::Var(v) => gen_conv_var_widen(w, &op.dst, &v, info),
            Scalar::SymbolOffset(s, o) => gen_conv_sym_widen(w, &op.dst, &s, *o, info),
            Scalar::ConstInt(_) => {
                unreachable!("const propagation must be performed before emitting code")
            }
        }
    } else {
        let copy_op = UnaryUnsignedOp {
            dst: op.dst.clone(),
            src: op.src.clone(),
            width: op.dst_width,
        };
        return gen_copy(w, &copy_op);
    }
}

fn gen_conv_var_widen(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src: &VarLocation<FrameReg>,
    info: ConvInfo,
) {
    use crate::ccpu::instr::Reg::*;

    // 1. copy common part
    if dst != src {
        let src_width = info.src_width as u16;
        if src_width == 1 {
            w.ldi_p_var_location(src, 0, true);
            w.ld(A);
            w.ldi_p_var_location(dst, 0, true);
            w.st(A);
        } else {
            let words = src_width / 2;
            for i in 0..words {
                w.ldi_p_var_location(src, i * 2, true);
                w.ld(B);
                w.inc(PL);
                w.ld(A);
                w.ldi_p_var_location(dst, i * 2, true);
                w.st(B);
                w.inc(PL);
                w.st(A);
            }
        }
    }

    // 2. extend with sign or zero
    let sign_extend = info.src_sign;
    // set A to the extension byte
    if sign_extend {
        if dst == src {
            let offset = info.src_width as u16 - 1;
            w.ldi_p_var_location(dst, offset, true);
            w.ld(A);
        }
        w.shl(A);
        w.exp(A);
    } else {
        if dst == src {
            let offset = info.src_width as u16;
            w.ldi_p_var_location(dst, offset, true);
        }
        w.mov(A, Zero);
    }
    // fill the rest of the dst
    let count = (info.dst_width as u16) - (info.src_width as u16);
    for i in 0..count {
        w.st(A);
        if i != count - 1 {
            w.inc(PL);
        }
    }
}

fn gen_conv_sym_widen(
    w: &mut InstructionWriter,
    dst: &VarLocation<FrameReg>,
    src: &GlobalVarId,
    offset: u16,
    info: ConvInfo,
) {
    use crate::ccpu::instr::Reg::*;

    let src_width = info.src_width as u16;
    w.ldi_p_var_location(dst, 0, true);
    w.ldi_lo(A, get_global_var_label(src), offset);
    w.st(A);
    if src_width > 1 {
        w.inc(PL);
        w.ldi_hi(A, get_global_var_label(src), offset);
        w.st(A);
    }

    let sign_extend = info.src_sign;
    if sign_extend {
        w.shl(A);
        w.exp(A);
    } else {
        w.mov(A, Zero);
    }

    let dst_width = info.dst_width as u16;
    let remainder = if src_width > 1 {
        dst_width - 2
    } else {
        dst_width - 1
    };
    for _ in 0..remainder {
        w.inc(PL);
        w.st(A);
    }
}

impl ConvInfo {
    fn new(op: &generic_ir::ConvOp<FrameReg>) -> Self {
        Self {
            dst_width: op.dst_width,
            src_width: op.src_width,
            dst_sign: op.dst_sign,
            src_sign: op.src_sign,
        }
    }

    fn is_widening(&self) -> bool {
        self.dst_width > self.src_width
    }
}
