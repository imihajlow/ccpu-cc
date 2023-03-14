use std::collections::HashSet;

use replace_with::replace_with_or_abort;

use crate::ir;

pub fn delete_unused_regs(blocks: &mut Vec<ir::Block>) -> bool {
    let mut total_set_regs = HashSet::new();
    let mut total_read_regs = HashSet::new();
    for block in blocks.iter() {
        collect_read_regs(block, &mut total_read_regs);
        collect_set_regs(block, &mut total_set_regs);
    }
    let unused_regs = total_set_regs
        .difference(&total_read_regs)
        .copied()
        .collect();
    let mut modified = false;
    for block in blocks.iter_mut() {
        modified |= block.phi.delete_dsts_from_set(&unused_regs);
        let old_len = block.ops.len();
        replace_with_or_abort(&mut block.ops, |ops| {
            ops.into_iter()
                .filter(|op| {
                    op.has_side_effects()
                        || op
                            .get_dst_reg()
                            .map(|reg| !unused_regs.contains(&reg))
                            .unwrap_or(true)
                })
                .collect()
        });
        modified |= old_len != block.ops.len()
    }
    modified
}

fn collect_read_regs(block: &ir::Block, refs: &mut HashSet<ir::Reg>) {
    block.phi.collect_read_regs(refs);
    for op in &block.ops {
        op.collect_read_regs(refs);
    }
    block.tail.collect_read_regs(refs);
}

fn collect_set_regs(block: &ir::Block, refs: &mut HashSet<ir::Reg>) {
    block.phi.collect_set_regs(refs);
    for op in &block.ops {
        op.collect_set_regs(refs);
    }
}
