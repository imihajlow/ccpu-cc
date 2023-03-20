use std::collections::{HashSet, VecDeque};

use replace_with::replace_with_or_abort;

use crate::{graph::ObjectGraph, ir};

pub fn delete_unused_regs(blocks: &mut Vec<ir::Block>) -> bool {
    let mut useful_regs = HashSet::new();
    let mut phi_graph = ObjectGraph::new();
    for block in blocks.iter() {
        for op in block.ops.iter() {
            op.collect_read_regs(&mut useful_regs);
        }
        block.tail.collect_read_regs(&mut useful_regs);

        for (dst, (_, srcs)) in block.phi.srcs.iter() {
            for (_, src) in srcs.iter() {
                if let Some(src) = src.get_reg() {
                    phi_graph.add_edge(dst, &src);
                }
            }
        }
    }

    let mut to_visit = VecDeque::from_iter(useful_regs.iter().copied());
    while let Some(reg) = to_visit.pop_front() {
        if let Some(edges_iter) = phi_graph.get_edges(&reg) {
            for source in edges_iter {
                if useful_regs.insert(*source) {
                    to_visit.push_back(*source);
                }
            }
        }
    }
    let mut total_set_regs = HashSet::new();
    for block in blocks.iter() {
        collect_set_regs(block, &mut total_set_regs);
    }
    let unused_regs = total_set_regs.difference(&useful_regs).copied().collect();
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

fn collect_set_regs(block: &ir::Block, refs: &mut HashSet<ir::Reg>) {
    block.phi.collect_set_regs(refs);
    for op in &block.ops {
        op.collect_set_regs(refs);
    }
}
