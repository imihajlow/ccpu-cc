use std::mem;

use replace_with::replace_with_or_abort;

use crate::ir;

/**
 * Find and delete unreachable blocks.
 * The first argument of the return tuple indicates if any changes have been made.
 */
pub fn drop_orphan_blocks(blocks: Vec<ir::Block>) -> (bool, Vec<ir::Block>) {
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
pub fn simplify_jumps(blocks: &mut Vec<ir::Block>) -> bool {
    let mut result = false;
    for block in blocks.iter_mut() {
        replace_with_or_abort(&mut block.tail, |tail| match tail {
            ir::Tail::Cond(_, t, e) if t == e => {
                result = true;
                ir::Tail::Jump(t)
            }
            ir::Tail::Switch(s, w, cases, default) => {
                if cases.iter().all(|(_, id)| *id == default) {
                    result = true;
                    ir::Tail::Jump(default)
                } else {
                    ir::Tail::Switch(s, w, cases, default)
                }
            }
            ir::Tail::Cond(_, _, _) | ir::Tail::Jump(_) | ir::Tail::Ret => tail,
        });
    }
    result
}

/**
 * If a block is preceeded excatly by one block in the chain, merge those two blocks into one.
 *
 * Returns true if any changes have been made.
 */
pub fn merge_chains(blocks: &mut Vec<ir::Block>) -> bool {
    assert!(!blocks.is_empty());
    let mut ref_counts = vec![0; blocks.len()];
    ref_counts[0] = 1;
    for block in blocks.iter() {
        match &block.tail {
            ir::Tail::Jump(n) => ref_counts[*n] += 1,
            ir::Tail::Cond(_, then_id, else_id) => {
                ref_counts[*then_id] += 1;
                ref_counts[*else_id] += 1;
            }
            ir::Tail::Switch(_, _, cases, default) => {
                for (_, id) in cases {
                    ref_counts[*id] += 1;
                }
                ref_counts[*default] += 1;
            }
            ir::Tail::Ret => (),
        }
    }
    let mut merge_with = Vec::with_capacity(blocks.len());
    for block in blocks.iter() {
        merge_with.push(match &block.tail {
            ir::Tail::Jump(n) if ref_counts[*n] == 1 => Some(*n),
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
                    result = true;
                    let mut ops = mem::replace(&mut blocks[n].ops, Vec::new());
                    blocks[i].ops.append(&mut ops);
                    blocks[i].tail = blocks[n].tail.clone();
                    merge_with[i] = None;
                } else {
                    done = false;
                }
            }
        }
    }
    result
}

fn visit_blocks(index: usize, blocks: &Vec<ir::Block>, visited: &mut Vec<bool>) {
    assert_eq!(blocks.len(), visited.len());

    if visited[index] {
        return;
    }

    visited[index] = true;
    let block = &blocks[index];
    match &block.tail {
        ir::Tail::Jump(i) => visit_blocks(*i, blocks, visited),
        ir::Tail::Cond(_, then_id, else_id) => {
            visit_blocks(*then_id, blocks, visited);
            visit_blocks(*else_id, blocks, visited);
        }
        ir::Tail::Switch(_, _, cases, default) => {
            for (_, i) in cases {
                visit_blocks(*i, blocks, visited);
            }
            visit_blocks(*default, blocks, visited);
        }
        ir::Tail::Ret => (),
    }
}

fn adjust_block_ids(block: ir::Block, offsets: &Vec<usize>) -> ir::Block {
    let tail = match block.tail {
        ir::Tail::Jump(n) => ir::Tail::Jump(n - offsets[n]),
        ir::Tail::Cond(c, then_id, else_id) => {
            ir::Tail::Cond(c, then_id - offsets[then_id], else_id - offsets[else_id])
        }
        ir::Tail::Switch(c, w, cases, default) => {
            let cases = cases
                .into_iter()
                .map(|(v, id)| (v, id - offsets[id]))
                .collect();
            let default = default - offsets[default];
            ir::Tail::Switch(c, w, cases, default)
        }
        ir::Tail::Ret => ir::Tail::Ret,
    };
    ir::Block { tail, ..block }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_visit() {
        /*
        0 -> 2 -> 4 -> 5
             ^----
        */
        let blocks = vec![
            ir::Block {
                phi: Vec::new(),
                ops: vec![ir::Op::Dummy(0)],
                tail: ir::Tail::Jump(2),
            },
            ir::Block {
                phi: Vec::new(),
                ops: vec![ir::Op::Dummy(1)],
                tail: ir::Tail::Cond(ir::Scalar::ConstInt(1), 0, 2),
            },
            ir::Block {
                phi: Vec::new(),
                ops: vec![ir::Op::Dummy(2)],
                tail: ir::Tail::Jump(4),
            },
            ir::Block {
                phi: Vec::new(),
                ops: vec![ir::Op::Dummy(3)],
                tail: ir::Tail::Jump(4),
            },
            ir::Block {
                phi: Vec::new(),
                ops: vec![ir::Op::Dummy(4)],
                tail: ir::Tail::Cond(ir::Scalar::ConstInt(1), 2, 5),
            },
            ir::Block {
                phi: Vec::new(),
                ops: vec![ir::Op::Dummy(5)],
                tail: ir::Tail::Jump(0),
            },
        ];

        let (modified, new_blocks) = drop_orphan_blocks(blocks);
        assert!(modified);
        assert_eq!(
            new_blocks,
            vec![
                ir::Block {
                    phi: Vec::new(),
                    ops: vec![ir::Op::Dummy(0)],
                    tail: ir::Tail::Jump(1),
                },
                ir::Block {
                    phi: Vec::new(),
                    ops: vec![ir::Op::Dummy(2)],
                    tail: ir::Tail::Jump(2),
                },
                ir::Block {
                    phi: Vec::new(),
                    ops: vec![ir::Op::Dummy(4)],
                    tail: ir::Tail::Cond(ir::Scalar::ConstInt(1), 1, 3),
                },
                ir::Block {
                    phi: Vec::new(),
                    ops: vec![ir::Op::Dummy(5)],
                    tail: ir::Tail::Jump(0),
                },
            ]
        )
    }
}
