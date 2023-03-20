use std::assert_matches::assert_matches;
use std::collections::HashMap;

use crate::ir::{Block, Op, Phi, Scalar, Tail, UnaryUnsignedOp, VarLocation, Width};

use crate::deconstruct::*;

#[test]
fn test_deconstruct_1() {
    let mut blocks = vec![
        Block {
            phi: Phi::new(),
            ops: vec![],
            tail: Tail::Jump(2),
            loop_depth: 0,
        },
        Block {
            phi: Phi::new(),
            ops: vec![],
            tail: Tail::Jump(2),
            loop_depth: 0,
        },
        Block {
            phi: Phi {
                srcs: HashMap::from([
                    (
                        38,
                        (
                            Width::Dword,
                            vec![
                                (0, Scalar::Var(VarLocation::Local(22))),
                                (1, Scalar::Var(VarLocation::Local(24))),
                            ],
                        ),
                    ),
                    (
                        39,
                        (
                            Width::Dword,
                            vec![
                                (0, Scalar::Var(VarLocation::Local(23))),
                                (1, Scalar::Var(VarLocation::Local(25))),
                            ],
                        ),
                    ),
                ]),
            },
            ops: vec![],
            tail: Tail::Jump(0),
            loop_depth: 0,
        },
    ];
    let mut map = HashMap::from([(38, 0), (39, 1), (22, 0), (23, 1), (24, 4), (25, 5)]);
    deconstruct_ssa(&mut blocks, &mut map);
    assert_eq!(blocks.len(), 3);
    assert_eq!(blocks[0].ops, vec![]);
    assert_eq!(blocks[1].ops.len(), 2);
    assert!(blocks[1].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(0),
        src: Scalar::Var(VarLocation::Local(4)),
        width: Width::Dword,
    })));

    assert!(blocks[1].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(1),
        src: Scalar::Var(VarLocation::Local(5)),
        width: Width::Dword,
    })));
}

#[test]
fn test_deconstruct_2() {
    let mut blocks = vec![
        Block {
            phi: Phi::new(),
            ops: vec![],
            tail: Tail::Cond(Scalar::Var(VarLocation::Local(10)), 1, 2),
            loop_depth: 0,
        },
        Block {
            phi: Phi::new(),
            ops: vec![],
            tail: Tail::Jump(2),
            loop_depth: 0,
        },
        Block {
            phi: Phi {
                srcs: HashMap::from([
                    (
                        38,
                        (
                            Width::Dword,
                            vec![
                                (0, Scalar::Var(VarLocation::Local(22))),
                                (1, Scalar::Var(VarLocation::Local(24))),
                            ],
                        ),
                    ),
                    (
                        39,
                        (
                            Width::Dword,
                            vec![
                                (0, Scalar::Var(VarLocation::Local(23))),
                                (1, Scalar::Var(VarLocation::Local(25))),
                            ],
                        ),
                    ),
                ]),
            },
            ops: vec![],
            tail: Tail::Jump(0),
            loop_depth: 0,
        },
    ];
    let mut map = HashMap::from([
        (10, 0),
        (38, 0),
        (39, 1),
        (22, 2),
        (23, 3),
        (24, 4),
        (25, 5),
    ]);
    deconstruct_ssa(&mut blocks, &mut map);
    println!("{:#?}", blocks);
    assert_eq!(blocks.len(), 4);
    assert_eq!(blocks[0].ops, vec![]);
    assert_matches!(
        blocks[0].tail,
        Tail::Cond(Scalar::Var(VarLocation::Local(0)), 1, 3)
    );

    assert_eq!(blocks[1].ops.len(), 2);
    assert!(blocks[1].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(0),
        src: Scalar::Var(VarLocation::Local(4)),
        width: Width::Dword,
    })));
    assert!(blocks[1].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(1),
        src: Scalar::Var(VarLocation::Local(5)),
        width: Width::Dword,
    })));

    assert_eq!(blocks[3].ops.len(), 2);
    assert!(blocks[3].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(0),
        src: Scalar::Var(VarLocation::Local(2)),
        width: Width::Dword,
    })));
    assert!(blocks[3].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(1),
        src: Scalar::Var(VarLocation::Local(3)),
        width: Width::Dword,
    })));
    assert_matches!(blocks[3].tail, Tail::Jump(2));
}
