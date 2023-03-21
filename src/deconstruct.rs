use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
};

use crate::{ccpu::reg::FrameReg, generic_ir, graph::ObjectGraph, ir, temp_reg::TempReg};

pub fn deconstruct_ssa(
    body: Vec<ir::Block>,
    map: &HashMap<ir::VirtualReg, FrameReg>,
) -> Vec<generic_ir::Block<FrameReg>> {
    // Rename registers in body and tail
    let mut body: Vec<generic_ir::Block<FrameReg>> = body
        .into_iter()
        .map(|block| generic_ir::Block {
            ops: block.ops.into_iter().map(|op| op.remap_regs(map)).collect(),
            tail: block.tail.remap_regs(map),
            phi: block.phi.remap_regs(map),
            loop_depth: block.loop_depth,
        })
        .collect();

    let mut copies = CopyOps::new();

    // Convert phi nodes to copies
    for (dst_block_id, block) in body.iter_mut().enumerate() {
        for (src_block_id, dst, src, width) in phi_to_copies(&block.phi).into_iter() {
            copies.push_copy(dst_block_id, src_block_id, dst, src, width);
        }
        block.phi = generic_ir::Phi::new();
    }

    println!("copies = {:#?}", copies);

    // Insert copies to the ends of corresponding blocks
    for (block_id, copies) in copies.into_iter() {
        let mut tail_regs = HashSet::new();
        body[block_id].tail.collect_read_regs(&mut tail_regs);

        let (original_copies, moved_copies) = copies.into_lists(tail_regs);

        for (dst, src, width) in original_copies.into_iter() {
            body[block_id].ops.push(get_copy_op(dst, src, width));
        }

        for (dst_block_id, copies) in moved_copies.into_iter() {
            let new_block = generic_ir::Block {
                phi: generic_ir::Phi::new(),
                tail: generic_ir::Tail::Jump(dst_block_id),
                ops: copies
                    .into_iter()
                    .map(|(dst, src, width)| get_copy_op(dst, src, width))
                    .collect(),
                loop_depth: body[dst_block_id].loop_depth,
            };
            body.push(new_block);
            let new_block_id = body.len() - 1;
            body[block_id]
                .tail
                .replace_block_id(dst_block_id, new_block_id);
        }
    }
    body
}

fn get_copy_op<Reg>(dst: Reg, src: Reg, width: ir::Width) -> generic_ir::Op<Reg> {
    generic_ir::Op::Copy(generic_ir::UnaryUnsignedOp {
        dst: generic_ir::VarLocation::Local(dst),
        src: generic_ir::Scalar::Var(generic_ir::VarLocation::Local(src)),
        width,
    })
}

/**
 * Build graphs for each source block connecting destination registers to source registers.
 * Additionally, build a map of used width in destination registers.
 */
fn build_phi_graphs<Reg: Copy + Eq + Hash>(
    phi: &generic_ir::Phi<Reg>,
) -> (HashMap<usize, ObjectGraph<Reg>>, HashMap<Reg, ir::Width>) {
    let mut graphs = HashMap::new();
    let mut widths = HashMap::new();
    for (dst, (w, srcs)) in &phi.srcs {
        widths.insert(*dst, *w);
        for (block_id, src) in srcs {
            if let Some(src_reg) = src.get_reg() {
                let graph = if let Some(g) = graphs.get_mut(block_id) {
                    g
                } else {
                    graphs.insert(*block_id, ObjectGraph::new());
                    graphs.get_mut(block_id).unwrap()
                };
                graph.add_edge(dst, &src_reg);
            } else {
                todo!("scalars in phi");
            }
        }
    }
    (graphs, widths)
}

fn phi_to_copies<Reg: Copy + Eq + Hash + TempReg + Debug>(
    phi: &generic_ir::Phi<Reg>,
) -> Vec<(usize, Reg, Reg, ir::Width)> {
    let (graphs, widths) = build_phi_graphs(phi);
    let mut copies = Vec::new();
    for (src_block, g) in graphs {
        // Each node in g has at most one incoming and one outgoing edge.
        // This means if there is a cycle in g, no edges lead out of it or into it.
        let scc = g.find_strongly_connected();
        let order = scc.inverse_topsort().unwrap();
        for group in order.iter().rev() {
            if group.len() == 1 {
                // simple copy
                let dst_reg = group[0];
                let idx = g.get_node_index(&dst_reg).unwrap();
                if let Some(src_idx) = g.get_edges_from_index(idx).next() {
                    let src_reg = *g.get_object(src_idx).unwrap();
                    if src_reg != dst_reg {
                        let width = *widths.get(&dst_reg).unwrap();
                        copies.push((src_block, dst_reg, src_reg, width));
                    }
                } else {
                    // End of chain.
                }
            } else {
                // a cycle - swap is necessary
                assert_ne!(group.len(), 0);
                let first_reg = group[0];
                let mut first_reg_node_idx = g.get_node_index(&first_reg).unwrap();
                let mut loop_copies = Vec::new();
                for _ in 0..(group.len() - 1) {
                    let src_reg_node_idx =
                        g.get_edges_from_index(first_reg_node_idx).next().unwrap();
                    let dst_reg = g.get_object(first_reg_node_idx).unwrap();
                    let src_reg = g.get_object(src_reg_node_idx).unwrap();
                    let width = widths.get(dst_reg).unwrap();
                    loop_copies.push((src_block, *dst_reg, *src_reg, *width));
                    first_reg_node_idx = src_reg_node_idx;
                }
                let dst_reg = g.get_object(first_reg_node_idx).unwrap();
                let width = *widths.get(dst_reg).unwrap();
                loop_copies.push((src_block, *dst_reg, Reg::get_temp_register(), width));

                copies.push((src_block, Reg::get_temp_register(), first_reg, width));
                copies.extend(loop_copies.into_iter());
            }
        }
    }
    copies
}

#[derive(Debug)]
struct CopyOps(HashMap<usize, BlockCopyOps>);

#[derive(Debug)]
struct BlockCopyOps(HashMap<usize, Vec<(FrameReg, FrameReg, ir::Width)>>);

impl CopyOps {
    fn new() -> Self {
        CopyOps(HashMap::new())
    }

    fn push_copy(
        &mut self,
        dst_block_id: usize,
        src_block_id: usize,
        dst_reg: FrameReg,
        src_reg: FrameReg,
        width: ir::Width,
    ) {
        let src_map = if let Some(m) = self.0.get_mut(&src_block_id) {
            m
        } else {
            let m = BlockCopyOps::new();
            self.0.insert(src_block_id, m);
            self.0.get_mut(&src_block_id).unwrap()
        };
        src_map.push_copy(dst_block_id, dst_reg, src_reg, width);
    }

    fn into_iter(self) -> impl Iterator<Item = (usize, BlockCopyOps)> {
        self.0.into_iter()
    }
}

impl BlockCopyOps {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn push_copy(
        &mut self,
        dst_block_id: usize,
        dst_reg: FrameReg,
        src_reg: FrameReg,
        width: ir::Width,
    ) {
        let dst_vec = if let Some(m) = self.0.get_mut(&dst_block_id) {
            m
        } else {
            let m = Vec::new();
            self.0.insert(dst_block_id, m);
            self.0.get_mut(&dst_block_id).unwrap()
        };

        dst_vec.push((dst_reg, src_reg, width));
    }

    /**
     * Produce lists of copy operations.
     * First list indicates operations which should go to the original block,
     * copy operations from other lists should be separated into new blocks to avoid conflicts.
     */
    fn into_lists(
        self,
        tail_regs: HashSet<FrameReg>,
    ) -> (
        Vec<(FrameReg, FrameReg, ir::Width)>,
        Vec<(usize, Vec<(FrameReg, FrameReg, ir::Width)>)>,
    ) {
        let conflicting_dsts = self.find_conflicts(&tail_regs);
        let mut original_copies = Vec::new();
        let mut moved_copies = Vec::new();
        for (dst_block_id, copies) in self.0.into_iter() {
            if conflicting_dsts.contains(&dst_block_id) {
                moved_copies.push((dst_block_id, copies));
            } else {
                original_copies.extend(copies.into_iter());
            }
        }
        (original_copies, moved_copies)
    }

    /**
     * Find set of destination block ids for given source block id
     * such that copies for those destination blocks conflict with each other
     * or with registers used in block's tail.
     */
    fn find_conflicts(&self, tail_regs: &HashSet<FrameReg>) -> HashSet<usize> {
        #[derive(Copy, Clone, PartialEq, Eq, Hash)]
        enum Usage {
            Tail,
            Block(usize),
        }
        let mut result = HashSet::new();
        loop {
            let mut usages = HashMap::new();
            for tail_reg in tail_regs.into_iter() {
                usages.insert(tail_reg, HashSet::from([Usage::Tail]));
            }
            for (dst_block_id, copies) in self.0.iter() {
                if !result.contains(dst_block_id) {
                    for (dst_reg, _, _) in copies.iter() {
                        if let Some(v) = usages.get_mut(&dst_reg) {
                            v.insert(Usage::Block(*dst_block_id));
                        } else {
                            usages.insert(dst_reg, HashSet::from([Usage::Block(*dst_block_id)]));
                        }
                    }
                }
            }
            let mut conflict_counts = HashMap::new();
            for (_, set) in usages.into_iter() {
                if set.len() > 1 {
                    for item in set.into_iter() {
                        if let Usage::Block(block_id) = item {
                            if let Some(count) = conflict_counts.get_mut(&block_id) {
                                *count += 1;
                            } else {
                                conflict_counts.insert(block_id, 1);
                            }
                        }
                    }
                }
            }
            if let Some((block_id, _)) = conflict_counts.into_iter().max_by_key(|(_, count)| *count)
            {
                result.insert(block_id);
            } else {
                break;
            }
        }
        result
    }
}

#[cfg(test)]
mod test {
    use std::assert_matches::assert_matches;

    use super::*;
    use ir::Width::*;

    const TMP_REG_INDEX: u32 = 10000;
    impl TempReg for u32 {
        fn get_temp_register() -> Self {
            TMP_REG_INDEX
        }
    }

    fn index_of<Reg: Copy + Eq + Hash>(
        copies: &Vec<(usize, Reg, Reg, ir::Width)>,
        el: (usize, Reg, Reg, ir::Width),
    ) -> usize {
        copies.iter().position(|e| e == &el).unwrap()
    }

    #[test]
    fn test_phi_to_copies_1() {
        let mut phi = ir::Phi::new();
        phi.add_binding(&0, &1, 0, Byte);
        phi.add_binding(&1, &2, 0, Word);
        phi.add_binding(&2, &3, 0, Dword);
        phi.add_binding(&4, &5, 0, Qword);
        let copies = phi_to_copies(&phi);
        assert_eq!(copies.len(), 4);
        assert!(copies.contains(&(0, 0, 1, Byte)));
        assert!(copies.contains(&(0, 1, 2, Word)));
        assert!(copies.contains(&(0, 2, 3, Dword)));
        assert!(copies.contains(&(0, 4, 5, Qword)));

        assert!(index_of(&copies, (0, 0, 1, Byte)) < index_of(&copies, (0, 1, 2, Word)));
        assert!(index_of(&copies, (0, 1, 2, Word)) < index_of(&copies, (0, 2, 3, Dword)));
    }

    #[test]
    fn test_phi_to_copies_2() {
        let mut phi = ir::Phi::new();
        phi.add_binding(&0, &0, 0, Byte);
        phi.add_binding(&1, &1, 0, Word);
        phi.add_binding(&2, &3, 0, Dword);
        phi.add_binding(&3, &4, 0, Qword);
        phi.add_binding(&4, &2, 0, Byte);
        let copies = phi_to_copies(&phi);
        println!("{:?}", copies);
        assert_eq!(copies.len(), 4);
        for (_, dst, _, width) in &copies {
            match (*dst, *width) {
                (2, Dword) | (3, Qword) | (4, Byte) => (),
                (TMP_REG_INDEX, _) => (),
                _ => panic!("wrong width"),
            }
        }
        assert_matches!(copies[0], (0, TMP_REG_INDEX, _, _));
        assert_matches!(copies[3], (0, _, TMP_REG_INDEX, _));
        assert_eq!(copies[0].3, copies[3].3);
    }
}
