use std::mem;
use std::{collections::HashMap, hash::Hash};

use replace_with::replace_with_or_abort;

use crate::{generic_ir, ir};

/**
 * Find and delete unreachable blocks.
 * The first argument of the return tuple indicates if any changes have been made.
 */
pub fn drop_orphan_blocks<Reg: Hash + Eq + Copy>(
    blocks: Vec<generic_ir::Block<Reg>>,
) -> (bool, Vec<generic_ir::Block<Reg>>) {
    assert!(!blocks.is_empty());
    let mut visited = vec![false; blocks.len()];
    visit_blocks(0, &blocks, &mut visited);
    if visited.iter().all(|x| *x) {
        return (false, blocks);
    }
    let mut offsets = Vec::with_capacity(blocks.len());
    let mut offset = 0;
    for i in 0..blocks.len() {
        if !visited[i] {
            offset += 1;
        }
        offsets.push(offset);
    }

    let mut result = Vec::new();
    for (i, block) in blocks.into_iter().enumerate() {
        if visited[i] {
            result.push(adjust_block_ids(block, &offsets));
        }
    }
    (true, result)
}

/**
 * If all branches of a conditional jump or a switch lead to the same block id, replace that tail with an unconditional jump.
 *
 * Returns true if any changes have been made.
 */
pub fn simplify_jumps<Reg: Copy + Hash + Eq>(blocks: &mut Vec<generic_ir::Block<Reg>>) -> bool {
    use generic_ir::Tail;
    let mut result = false;
    for block in blocks.iter_mut() {
        replace_with_or_abort(&mut block.tail, |tail| match tail {
            Tail::Cond(_, t, e) if t == e => {
                result = true;
                Tail::Jump(t)
            }
            Tail::Cond(generic_ir::Scalar::SymbolOffset(_, _), t, _) => {
                result = true;
                Tail::Jump(t)
            }
            Tail::Cond(generic_ir::Scalar::ConstInt(c), t, e) => {
                result = true;
                if c & 0xff == 0 {
                    Tail::Jump(e)
                } else {
                    Tail::Jump(t)
                }
            }
            Tail::Switch(s, w, cases, default) => {
                if cases.iter().all(|(_, id)| *id == default) {
                    result = true;
                    Tail::Jump(default)
                } else {
                    if let generic_ir::Scalar::ConstInt(c) = s {
                        let target = cases
                            .iter()
                            .find(|(v, _)| *v == c)
                            .map(|(_, block_id)| *block_id)
                            .unwrap_or(default);
                        Tail::Jump(target)
                    } else {
                        Tail::Switch(s, w, cases, default)
                    }
                }
            }
            Tail::Cond(_, _, _) | Tail::Jump(_) | Tail::Ret => tail,
        });
    }
    result
}

/**
 * If a block is preceeded excatly by one block in the chain, merge those two blocks into one.
 *
 * Returns true if any changes have been made.
 */
pub fn merge_chains<Reg: Copy + Hash + Eq>(blocks: &mut Vec<generic_ir::Block<Reg>>) -> bool {
    assert!(!blocks.is_empty());
    let mut ref_counts = vec![0; blocks.len()];
    ref_counts[0] = 1;
    for block in blocks.iter() {
        for next in block.tail.get_connections() {
            ref_counts[next] += 1;
        }
    }
    let mut merge_with = Vec::with_capacity(blocks.len());
    for block in blocks.iter() {
        merge_with.push(match &block.tail {
            generic_ir::Tail::Jump(n) if ref_counts[*n] == 1 => Some(*n),
            _ => None,
        });
    }
    let mut result = false;
    let mut done = false;
    while !done {
        done = true;
        for i in 0..blocks.len() {
            if let Some(n) = merge_with[i] {
                if merge_with[n].is_none() {
                    let mut phi_map = HashMap::new();
                    let phi_srcs = mem::replace(&mut blocks[n].phi.srcs, HashMap::new());
                    for (dst, (_, srcs)) in phi_srcs {
                        assert_eq!(srcs.len(), 1);
                        let src = srcs.first().unwrap().1.clone();
                        phi_map.insert(dst, src);
                    }
                    result = true;
                    let mut ops = mem::replace(&mut blocks[n].ops, Vec::new());
                    for op in &mut ops {
                        op.subs_src_regs(&phi_map);
                    }
                    blocks[i].ops.append(&mut ops);
                    blocks[i].tail = blocks[n].tail.clone();
                    blocks[i].tail.subs_src_regs(&phi_map);
                    for id in blocks[i].tail.get_connections() {
                        blocks[id].phi.subs_src_regs(&phi_map);
                    }
                    merge_with[i] = None;
                } else {
                    done = false;
                }
            }
        }
    }
    result
}

fn visit_blocks<Reg: Copy + Hash + Eq>(
    index: usize,
    blocks: &Vec<generic_ir::Block<Reg>>,
    visited: &mut Vec<bool>,
) {
    assert_eq!(blocks.len(), visited.len());

    if visited[index] {
        return;
    }

    visited[index] = true;
    let block = &blocks[index];
    match &block.tail {
        generic_ir::Tail::Jump(i) => visit_blocks(*i, blocks, visited),
        generic_ir::Tail::Cond(_, then_id, else_id) => {
            visit_blocks(*then_id, blocks, visited);
            visit_blocks(*else_id, blocks, visited);
        }
        generic_ir::Tail::Switch(_, _, cases, default) => {
            for (_, i) in cases {
                visit_blocks(*i, blocks, visited);
            }
            visit_blocks(*default, blocks, visited);
        }
        generic_ir::Tail::Ret => (),
    }
}

fn adjust_block_ids<Reg: Copy + Hash + Eq>(
    block: generic_ir::Block<Reg>,
    offsets: &Vec<usize>,
) -> generic_ir::Block<Reg> {
    let tail = match block.tail {
        generic_ir::Tail::Jump(n) => generic_ir::Tail::Jump(n - offsets[n]),
        generic_ir::Tail::Cond(c, then_id, else_id) => {
            generic_ir::Tail::Cond(c, then_id - offsets[then_id], else_id - offsets[else_id])
        }
        generic_ir::Tail::Switch(c, w, cases, default) => {
            let cases = cases
                .into_iter()
                .map(|(v, id)| (v, id - offsets[id]))
                .collect();
            let default = default - offsets[default];
            generic_ir::Tail::Switch(c, w, cases, default)
        }
        generic_ir::Tail::Ret => generic_ir::Tail::Ret,
    };
    let phi = block.phi.with_adjusted_block_ids(offsets);
    generic_ir::Block { tail, phi, ..block }
}

#[cfg(test)]
mod test {
    use crate::ir::Phi;

    use super::*;

    #[test]
    fn test_visit() {
        /*
        0 -> 2 -> 4 -> 5
             ^----
        */
        let blocks = vec![
            ir::Block {
                phi: Phi::new(),
                ops: vec![ir::Op::Dummy(0)],
                tail: ir::Tail::Jump(2),
                loop_depth: 0,
            },
            ir::Block {
                phi: Phi::new(),
                ops: vec![ir::Op::Dummy(1)],
                tail: ir::Tail::Cond(ir::Scalar::ConstInt(1), 0, 2),
                loop_depth: 0,
            },
            ir::Block {
                phi: Phi::new(),
                ops: vec![ir::Op::Dummy(2)],
                tail: ir::Tail::Jump(4),
                loop_depth: 0,
            },
            ir::Block {
                phi: Phi::new(),
                ops: vec![ir::Op::Dummy(3)],
                tail: ir::Tail::Jump(4),
                loop_depth: 0,
            },
            ir::Block {
                phi: Phi::new(),
                ops: vec![ir::Op::Dummy(4)],
                tail: ir::Tail::Cond(ir::Scalar::ConstInt(1), 2, 5),
                loop_depth: 0,
            },
            ir::Block {
                phi: Phi::new(),
                ops: vec![ir::Op::Dummy(5)],
                tail: ir::Tail::Jump(0),
                loop_depth: 0,
            },
        ];

        let (modified, new_blocks) = drop_orphan_blocks(blocks);
        assert!(modified);
        assert_eq!(
            new_blocks,
            vec![
                ir::Block {
                    phi: Phi::new(),
                    ops: vec![ir::Op::Dummy(0)],
                    tail: ir::Tail::Jump(1),
                    loop_depth: 0,
                },
                ir::Block {
                    phi: Phi::new(),
                    ops: vec![ir::Op::Dummy(2)],
                    tail: ir::Tail::Jump(2),
                    loop_depth: 0,
                },
                ir::Block {
                    phi: Phi::new(),
                    ops: vec![ir::Op::Dummy(4)],
                    tail: ir::Tail::Cond(ir::Scalar::ConstInt(1), 1, 3),
                    loop_depth: 0,
                },
                ir::Block {
                    phi: Phi::new(),
                    ops: vec![ir::Op::Dummy(5)],
                    tail: ir::Tail::Jump(0),
                    loop_depth: 0,
                },
            ]
        )
    }
}
