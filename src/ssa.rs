use std::collections::{HashMap, HashSet};

use replace_with::replace_with_or_abort;

use crate::graph::ObjectGraph;
use crate::{ir, name_scope::NameScope};

pub fn enforce_ssa(blocks: &mut Vec<ir::Block>, scope: &mut NameScope) {
    let live_regs = find_live_regs(blocks);

    // find width of data in each register to use in phi
    let mut reg_width = HashMap::new();
    for block in blocks.iter() {
        for op in block.ops.iter() {
            if let Some(reg) = op.get_dst_reg() {
                if let Some(width) = op.get_dst_data_width() {
                    reg_width.insert(reg, width);
                }
            }
        }
    }

    // mappings from original register number to new register number on block entry
    let entry_mappings = {
        let mut entry_mappings = Vec::with_capacity(blocks.len());
        for live in &live_regs {
            let mut map = HashMap::new();
            for reg in live {
                let new_reg = scope.alloc_reg();
                map.insert(*reg, new_reg);
            }
            entry_mappings.push(map);
        }
        entry_mappings
    };

    // updated mappings from original register number to new register number for each block
    let mut local_mappings = entry_mappings.clone();
    for cur_block_index in 0..blocks.len() {
        let map = &mut local_mappings[cur_block_index];
        for op in &mut blocks[cur_block_index].ops {
            replace_with_or_abort(op, |op| op.remap_regs(map, Some(scope)));
        }
        replace_with_or_abort(&mut blocks[cur_block_index].tail, |tail| {
            tail.remap_regs(map)
        });
    }

    for cur_block_index in 0..blocks.len() {
        let map = &mut local_mappings[cur_block_index];
        for dst_index in blocks[cur_block_index].tail.get_connections() {
            update_phi(
                blocks,
                dst_index,
                cur_block_index,
                &live_regs,
                &entry_mappings[dst_index],
                &map,
                &reg_width,
            );
        }
    }
}

fn build_block_graph(blocks: &Vec<ir::Block>) -> ObjectGraph<usize> {
    let mut g = ObjectGraph::new();
    for (i, block) in blocks.iter().enumerate() {
        g.add_node_unique(&i);
        match &block.tail {
            ir::Tail::Jump(n) => g.add_edge_unique(&i, n),
            ir::Tail::Cond(_, n, m) => {
                g.add_edge_unique(&i, n);
                g.add_edge_unique(&i, m);
            }
            ir::Tail::Switch(_, _, cases, default) => {
                for (_, n) in cases {
                    g.add_edge_unique(&i, n);
                }
                g.add_edge_unique(&i, default);
            }
            ir::Tail::Ret => (),
        }
    }
    g
}

fn find_live_regs(blocks: &Vec<ir::Block>) -> Vec<HashSet<ir::VirtualReg>> {
    let g = build_block_graph(blocks);
    let scc = g.find_strongly_connected();

    let topo_order = scc.clone().inverse_topsort().unwrap(); // no cycles in an SCC graph
    let scc_t = scc.clone().transposed();

    let mut referenced_regs: Vec<HashSet<ir::VirtualReg>> =
        vec![HashSet::new(); scc.get_node_count()];
    for group in topo_order.iter() {
        let node_index = scc_t.get_node_index(&group).unwrap();
        for block_id in group {
            collect_read_regs(&blocks[*block_id], &mut referenced_regs[node_index]);
        }
        for connected in scc_t.get_edges_from_index(node_index) {
            referenced_regs[connected] = referenced_regs[connected]
                .union(&referenced_regs[node_index])
                .copied()
                .collect();
        }
    }

    let mut defined_regs: Vec<HashSet<ir::VirtualReg>> = vec![HashSet::new(); scc.get_node_count()];
    for group in topo_order.iter().rev() {
        let node_index = scc.get_node_index(&group).unwrap();
        for block_id in group {
            collect_set_regs(&blocks[*block_id], &mut defined_regs[node_index]);
        }
        for connected in scc.get_edges_from_index(node_index) {
            defined_regs[connected] = defined_regs[connected]
                .union(&defined_regs[node_index])
                .copied()
                .collect();
        }
    }

    let live_regs_scc: Vec<HashSet<ir::VirtualReg>> = referenced_regs
        .into_iter()
        .zip(defined_regs.into_iter())
        .map(|(r, d)| r.intersection(&d).copied().collect())
        .collect();

    let mut live_regs = vec![HashSet::new(); blocks.len()];
    for (scc_index, set) in live_regs_scc.into_iter().enumerate() {
        let group = scc.get_object(scc_index).unwrap();
        for block_index in group {
            live_regs[*block_index] = set.clone();
        }
    }

    for (i, set) in live_regs.iter().enumerate() {
        println!("{}: {:?}", i, set);
    }
    live_regs
}

fn update_phi(
    blocks: &mut Vec<ir::Block>,
    dst_index: usize,
    src_index: usize,
    live: &Vec<HashSet<ir::VirtualReg>>, // live registers per block (original register indexes)
    dst_map: &HashMap<ir::VirtualReg, ir::VirtualReg>, // map from original indexes to indexes in destination block
    src_map: &HashMap<ir::VirtualReg, ir::VirtualReg>, // map from original indexes to indexes in source block
    orig_reg_width: &HashMap<ir::VirtualReg, ir::Width>, // width of data in registers (original reg indexes)
) {
    let block = &mut blocks[dst_index];
    for orig_reg in live[dst_index].intersection(&live[src_index]) {
        let dst_reg = dst_map.get(orig_reg).unwrap();
        let src_reg = src_map.get(orig_reg).unwrap();
        let width = if let Some(w) = orig_reg_width.get(&orig_reg) {
            *w
        } else {
            panic!(
                "width for phi sources should be known, orig_reg = {}, src_reg = {}, dst_reg = {}",
                orig_reg, src_reg, dst_reg
            );
        };
        block.phi.add_binding(dst_reg, src_reg, src_index, width);
    }
}

fn collect_read_regs(block: &ir::Block, refs: &mut HashSet<ir::VirtualReg>) {
    for op in &block.ops {
        op.collect_read_regs(refs);
    }
    block.tail.collect_read_regs(refs);
}

fn collect_set_regs(block: &ir::Block, refs: &mut HashSet<ir::VirtualReg>) {
    for op in &block.ops {
        op.collect_set_regs(refs);
    }
}
