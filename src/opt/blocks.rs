use crate::ir;

/**
 * Find and delete unreachable blocks.
 * The first argument of the return tuple indicates if any changes have been made.
 */
pub fn drop_orphan_blocks(blocks: Vec<ir::Block>) -> (bool, Vec<ir::Block>) {
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
