use std::collections::HashMap;

use replace_with::replace_with_or_abort_and_return;

use crate::{
    generic_ir::{Op, Scalar, VarLocation},
    ir::{self, UnaryUnsignedOp},
};

pub fn reduce_copies(blocks: &mut Vec<ir::Block>) -> bool {
    let mut modified = false;
    for block in blocks.iter_mut() {
        for op in block.ops.iter_mut() {
            modified |= replace_convert(op);
        }
    }
    let mut copy_map = HashMap::new();
    for block in blocks.iter() {
        for op in block.ops.iter() {
            if let Op::Copy(copy_op) = op {
                match (&copy_op.dst, copy_op.src.get_reg()) {
                    (VarLocation::Local(dst_reg), Some(src_reg)) => {
                        let src_reg = if let Some(r) = copy_map.get(&src_reg) {
                            *r
                        } else {
                            src_reg
                        };
                        copy_map.insert(*dst_reg, src_reg);
                    }
                    _ => (),
                }
            }
        }
    }
    let copy_scalar_map: HashMap<_, _> = copy_map
        .into_iter()
        .map(|(k, v)| (k, Scalar::Var(VarLocation::Local(v))))
        .collect();
    for block in blocks.iter_mut() {
        modified |= replace_with_or_abort_and_return(&mut block.ops, |ops| {
            let mut new_ops = Vec::new();
            let mut modified = false;
            for mut op in ops.into_iter() {
                if let Op::Copy(copy_op) = &op {
                    match (&copy_op.dst, copy_op.src.get_reg()) {
                        (VarLocation::Local(_), Some(_)) => modified = true,
                        _ => {
                            modified |= op.subs_src_regs(&copy_scalar_map);
                            new_ops.push(op);
                        }
                    }
                } else {
                    modified |= op.subs_src_regs(&copy_scalar_map);
                    new_ops.push(op);
                }
            }
            (modified, new_ops)
        });
        modified |= block.tail.subs_src_regs(&copy_scalar_map);
        modified |= block.phi.subs_src_regs(&copy_scalar_map);
    }
    modified
}

/// Replace narrowing/preserving conversion with copy.
fn replace_convert(op: &mut ir::Op) -> bool {
    replace_with_or_abort_and_return(op, |op| match op {
        Op::Conv(conv_op) if conv_op.dst_width <= conv_op.src_width => (
            true,
            ir::Op::Copy(UnaryUnsignedOp {
                dst: conv_op.dst,
                src: conv_op.src,
                width: conv_op.dst_width,
            }),
        ),
        _ => (false, op),
    })
}
