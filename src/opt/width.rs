use std::collections::HashMap;

use ir::Width;
use replace_with::replace_with_or_abort_and_return;

use crate::{
    generic_ir::{ArgOp, CallOp, ConvOp, IntrinCallVariant, LoadOp, VaArgOp},
    ir::{self, BinaryOp, BinaryUnsignedOp, CompareOp, ShiftOp, Tail, UnaryUnsignedOp},
};

pub fn optimize_width(blocks: &mut Vec<ir::Block>) -> bool {
    let mut max_width = HashMap::new();
    for block in blocks.iter() {
        for op in &block.ops {
            update_max_width_op(&mut max_width, op);
        }
        update_max_width_phi(&mut max_width, &block.phi);
        update_max_width_tail(&mut max_width, &block.tail);
    }
    let mut modified = false;
    for block in blocks.iter_mut() {
        for op in &mut block.ops {
            modified |= try_shorten_op(&max_width, op);
        }
        modified |= try_shorten_phi(&max_width, &mut block.phi);
    }
    modified
}

fn try_shorten_phi(max_width: &HashMap<ir::VirtualReg, ir::Width>, phi: &mut ir::Phi) -> bool {
    let mut modified = false;
    for (dst_reg, (w, _)) in phi.srcs.iter_mut() {
        if let Some(max_width) = max_width.get(dst_reg) {
            if *max_width < *w {
                *w = *max_width;
                modified = true;
            }
        }
    }
    modified
}

fn try_shorten_op(max_width: &HashMap<ir::VirtualReg, ir::Width>, op: &mut ir::Op) -> bool {
    use ir::Op;

    // convert - special case, check source
    if let Op::Conv(conv_op) = op {
        if let Some(src_reg) = conv_op.src.get_reg() {
            if let Some(w) = max_width.get(&src_reg) {
                if *w < conv_op.src_width {
                    conv_op.src_width = *w;
                }
            }
        }
    }

    let max_width = if let Some(dst_reg) = op.get_dst_reg() {
        if let Some(w) = max_width.get(&dst_reg) {
            *w
        } else {
            return false;
        }
    } else {
        return false;
    };
    replace_with_or_abort_and_return(op, |op| match op {
        Op::Copy(op) => {
            if op.width > max_width {
                // narrowing conversion, sign doesn't matter
                (
                    true,
                    Op::Conv(ConvOp {
                        dst: op.dst,
                        dst_width: max_width,
                        dst_sign: false,
                        src: op.src,
                        src_width: op.width,
                        src_sign: false,
                    }),
                )
            } else {
                (false, Op::Copy(op))
            }
        }
        Op::Bool(_) | Op::BoolInv(_) => (false, op),
        Op::Add(op) => {
            let (modified, op) = try_shorten_binary_op(max_width, op);
            (modified, Op::Add(op))
        }
        Op::Sub(op) => {
            let (modified, op) = try_shorten_binary_op(max_width, op);
            (modified, Op::Sub(op))
        }
        Op::Mul(op) => {
            let (modified, op) = try_shorten_binary_op(max_width, op);
            (modified, Op::Mul(op))
        }
        Op::Div(_) | Op::Mod(_) => (false, op),
        Op::BAnd(op) => {
            let (modified, op) = try_shorten_binary_unsigned_op(max_width, op);
            (modified, Op::BAnd(op))
        }
        Op::BOr(op) => {
            let (modified, op) = try_shorten_binary_unsigned_op(max_width, op);
            (modified, Op::BOr(op))
        }
        Op::BXor(op) => {
            let (modified, op) = try_shorten_binary_unsigned_op(max_width, op);
            (modified, Op::BXor(op))
        }
        Op::LShift(op) => {
            if max_width < op.lhs_width {
                (
                    true,
                    Op::LShift(ShiftOp {
                        lhs_width: max_width,
                        ..op
                    }),
                )
            } else {
                (false, Op::LShift(op))
            }
        }
        Op::RShift(_) => (false, op),
        Op::Neg(op) => {
            let (modified, op) = try_shorten_unary_unsigned_op(max_width, op);
            (modified, Op::Neg(op))
        }
        Op::Not(op) => {
            let (modified, op) = try_shorten_unary_unsigned_op(max_width, op);
            (modified, Op::Not(op))
        }
        Op::Compare(op) => {
            if max_width < op.dst_width {
                (
                    true,
                    Op::Compare(CompareOp {
                        dst_width: max_width,
                        ..op
                    }),
                )
            } else {
                (false, Op::Compare(op))
            }
        }
        Op::Conv(op) => {
            if max_width < op.dst_width {
                (
                    true,
                    Op::Conv(ConvOp {
                        dst_width: max_width,
                        ..op
                    }),
                )
            } else {
                (false, Op::Conv(op))
            }
        }
        Op::Store(_) => unreachable!(),
        Op::Load(op) => {
            if max_width < op.width {
                (
                    true,
                    Op::Load(LoadOp {
                        width: max_width,
                        ..op
                    }),
                )
            } else {
                (false, Op::Load(op))
            }
        }
        Op::Call(op) => {
            if let Some((dst, dst_width)) = op.dst {
                if max_width < dst_width {
                    (
                        true,
                        Op::Call(CallOp {
                            dst: Some((dst, max_width)),
                            ..op
                        }),
                    )
                } else {
                    (
                        false,
                        Op::Call(CallOp {
                            dst: Some((dst, dst_width)),
                            ..op
                        }),
                    )
                }
            } else {
                unreachable!()
            }
        }
        Op::Memcpy(_) => (false, op),
        Op::IntrinCall(_) => {
            unreachable!("Intrinsic calls should be introduced after this optimization step")
        }
        Op::Undefined(_) => (false, op),
        Op::Arg(op) => {
            if max_width < op.width {
                (
                    true,
                    Op::Arg(ArgOp {
                        width: max_width,
                        ..op
                    }),
                )
            } else {
                (false, Op::Arg(op))
            }
        }
        Op::FramePointer(_) => (false, op),
        Op::VaStart(_) => (false, op),
        Op::VaArg(op) => {
            if max_width < op.width {
                (
                    true,
                    Op::VaArg(VaArgOp {
                        width: max_width,
                        ..op
                    }),
                )
            } else {
                (false, Op::VaArg(op))
            }
        }
        Op::VaListInc(_) => (false, op),
        #[cfg(test)]
        Op::Dummy(_) => (false, op),
    })
}

fn try_shorten_binary_op(max_width: Width, op: BinaryOp) -> (bool, BinaryOp) {
    if max_width < op.width {
        (
            true,
            BinaryOp {
                width: max_width,
                ..op
            },
        )
    } else {
        (false, op)
    }
}

fn try_shorten_binary_unsigned_op(
    max_width: Width,
    op: BinaryUnsignedOp,
) -> (bool, BinaryUnsignedOp) {
    if max_width < op.width {
        (
            true,
            BinaryUnsignedOp {
                width: max_width,
                ..op
            },
        )
    } else {
        (false, op)
    }
}

fn try_shorten_unary_unsigned_op(max_width: Width, op: UnaryUnsignedOp) -> (bool, UnaryUnsignedOp) {
    if max_width < op.width {
        (
            true,
            UnaryUnsignedOp {
                width: max_width,
                ..op
            },
        )
    } else {
        (false, op)
    }
}

fn update_max_width_phi(max_width: &mut HashMap<ir::VirtualReg, ir::Width>, phi: &ir::Phi) {
    for (_, (w, v)) in &phi.srcs {
        for (_, s) in v {
            update_max_width_scalar(max_width, s, *w);
        }
    }
}

fn update_max_width_tail(max_width: &mut HashMap<ir::VirtualReg, ir::Width>, tail: &ir::Tail) {
    match tail {
        Tail::Jump(_) => (),
        Tail::Cond(s, _, _) => update_max_width_scalar(max_width, s, ir::Width::Byte),
        Tail::Ret => (),
        Tail::Switch(s, w, _, _) => update_max_width_scalar(max_width, s, *w),
    }
}

fn update_max_width_op(max_width: &mut HashMap<ir::VirtualReg, ir::Width>, op: &ir::Op) {
    use ir::Op;
    match op {
        Op::Copy(op) => update_max_width_scalar(max_width, &op.src, op.width),
        Op::Bool(op) | Op::BoolInv(op) => update_max_width_scalar(max_width, &op.src, op.width),
        Op::Add(op) | Op::Sub(op) | Op::Mul(op) | Op::Div(op) | Op::Mod(op) => {
            update_max_width_scalar(max_width, &op.lhs, op.width);
            update_max_width_scalar(max_width, &op.rhs, op.width);
        }
        Op::BAnd(op) | Op::BOr(op) | Op::BXor(op) => {
            update_max_width_scalar(max_width, &op.lhs, op.width);
            update_max_width_scalar(max_width, &op.rhs, op.width);
        }
        Op::LShift(op) | Op::RShift(op) => {
            update_max_width_scalar(max_width, &op.lhs, op.lhs_width);
            update_max_width_scalar(max_width, &op.rhs, ir::Width::Byte);
        }
        Op::Neg(op) | Op::Not(op) => update_max_width_scalar(max_width, &op.src, op.width),
        Op::Compare(op) => {
            update_max_width_scalar(max_width, &op.lhs, op.width);
            update_max_width_scalar(max_width, &op.rhs, op.width);
        }
        Op::Conv(op) => {
            let w = if op.dst_width < op.src_width {
                op.dst_width
            } else {
                op.src_width
            };
            update_max_width_scalar(max_width, &op.src, w);
        }
        Op::Store(op) => {
            update_max_width_scalar(max_width, &op.src, op.width);
            update_max_width_scalar(max_width, &op.dst_addr, ir::Width::PTR_WIDTH);
        }
        Op::Load(op) => {
            update_max_width_scalar(max_width, &op.src_addr, ir::Width::PTR_WIDTH);
        }
        Op::Call(op) => {
            for (s, w) in &op.args {
                update_max_width_scalar(max_width, s, *w);
            }
            update_max_width_scalar(max_width, &op.addr, ir::Width::PTR_WIDTH);
        }
        Op::Memcpy(op) => {
            update_max_width_scalar(max_width, &op.dst_addr, ir::Width::PTR_WIDTH);
            update_max_width_scalar(max_width, &op.src_addr, ir::Width::PTR_WIDTH);
        }
        Op::IntrinCall(op) => match &op.variant {
            IntrinCallVariant::Call2R(_, (w1, s1), (w2, s2)) => {
                update_max_width_scalar(max_width, s1, *w1);
                update_max_width_scalar(max_width, s2, *w2);
            }
            IntrinCallVariant::Call3((w1, s1), (w2, s2), (w3, s3)) => {
                update_max_width_scalar(max_width, s1, *w1);
                update_max_width_scalar(max_width, s2, *w2);
                update_max_width_scalar(max_width, s3, *w3);
            }
        },
        Op::Undefined(_) => (),
        Op::Arg(_) => (),
        Op::FramePointer(_) => (),
        Op::VaStart(_) => (),
        Op::VaArg(op) => {
            update_max_width_scalar(max_width, &op.src_va_list, ir::Width::VA_LIST_WIDTH)
        }
        Op::VaListInc(op) => update_max_width_scalar(max_width, &op.src, ir::Width::VA_LIST_WIDTH),
        #[cfg(test)]
        Op::Dummy(_) => (),
    }
}

fn update_max_width_scalar(
    max_width: &mut HashMap<ir::VirtualReg, ir::Width>,
    s: &ir::Scalar,
    w: ir::Width,
) {
    if let ir::Scalar::Var(ir::VarLocation::Local(reg)) = s {
        update_max_width(max_width, *reg, w)
    }
}

fn update_max_width(
    max_width: &mut HashMap<ir::VirtualReg, ir::Width>,
    reg: ir::VirtualReg,
    w: ir::Width,
) {
    if let Some(v) = max_width.get_mut(&reg) {
        if w > *v {
            *v = w;
        }
    } else {
        max_width.insert(reg, w);
    }
}
