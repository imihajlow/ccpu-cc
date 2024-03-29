use std::assert_matches::assert_matches;
use std::collections::HashMap;

use crate::ccpu::reg::FrameReg;
use crate::generic_ir::{
    Block, JumpCondition, Op, Phi, Scalar, Tail, UnaryUnsignedOp, VarLocation, Width,
};

use crate::deconstruct::*;

#[test]
fn test_deconstruct_1() {
    let blocks = vec![
        Block {
            phi: Phi::new(),
            ops: vec![],
            tail: Tail::Jump(2),
            loop_depth: 0,
            original_id: 0,
        },
        Block {
            phi: Phi::new(),
            ops: vec![],
            tail: Tail::Jump(2),
            loop_depth: 0,
            original_id: 1,
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
            original_id: 2,
        },
    ];
    let map = HashMap::from([
        (38, FrameReg::FrameA(0)),
        (39, FrameReg::FrameA(1)),
        (22, FrameReg::FrameA(0)),
        (23, FrameReg::FrameA(1)),
        (24, FrameReg::FrameA(4)),
        (25, FrameReg::FrameA(5)),
    ]);
    let (blocks, _) = deconstruct_ssa(blocks, &map);
    assert_eq!(blocks.len(), 3);
    assert_eq!(blocks[0].ops.len(), 2);
    assert!(blocks[0].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(FrameReg::FrameA(0)),
        src: Scalar::Var(VarLocation::Local(FrameReg::FrameA(0))),
        width: Width::Dword,
    })));
    assert!(blocks[0].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(FrameReg::FrameA(1)),
        src: Scalar::Var(VarLocation::Local(FrameReg::FrameA(1))),
        width: Width::Dword,
    })));

    assert_eq!(blocks[1].ops.len(), 2);
    assert!(blocks[1].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(FrameReg::FrameA(0)),
        src: Scalar::Var(VarLocation::Local(FrameReg::FrameA(4))),
        width: Width::Dword,
    })));

    // assert!(blocks[1].ops.contains(&Op::Copy(UnaryUnsignedOp {
    //     dst: VarLocation::Local(FrameReg::FrameA(1),)
    //     src: Scalar::Var(VarLocation::Local(FrameReg::FrameA(5))),
    //     width: Width::Dword,
    // })));
}

#[test]
fn test_deconstruct_2() {
    let blocks = vec![
        Block {
            phi: Phi::new(),
            ops: vec![],
            tail: Tail::Cond(
                JumpCondition::StrictBool(Scalar::Var(VarLocation::Local(10))),
                1,
                2,
            ),
            loop_depth: 0,
            original_id: 0,
        },
        Block {
            phi: Phi::new(),
            ops: vec![],
            tail: Tail::Jump(2),
            loop_depth: 0,
            original_id: 1,
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
            original_id: 2,
        },
    ];
    let map = HashMap::from([
        (10, FrameReg::FrameA(0)),
        (38, FrameReg::FrameA(0)),
        (39, FrameReg::FrameA(1)),
        (22, FrameReg::FrameA(2)),
        (23, FrameReg::FrameA(3)),
        (24, FrameReg::FrameA(4)),
        (25, FrameReg::FrameA(5)),
    ]);
    let (blocks, _) = deconstruct_ssa(blocks, &map);
    println!("{:#?}", blocks);
    assert_eq!(blocks.len(), 4);
    assert_eq!(blocks[0].ops, vec![]);
    assert_matches!(
        blocks[0].tail,
        Tail::Cond(
            JumpCondition::StrictBool(Scalar::Var(VarLocation::Local(FrameReg::FrameA(0)))),
            1,
            3
        )
    );

    assert_eq!(blocks[1].ops.len(), 2);
    assert!(blocks[1].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(FrameReg::FrameA(0)),
        src: Scalar::Var(VarLocation::Local(FrameReg::FrameA(4))),
        width: Width::Dword,
    })));
    assert!(blocks[1].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(FrameReg::FrameA(1)),
        src: Scalar::Var(VarLocation::Local(FrameReg::FrameA(5))),
        width: Width::Dword,
    })));

    assert_eq!(blocks[3].ops.len(), 2);
    assert!(blocks[3].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(FrameReg::FrameA(0)),
        src: Scalar::Var(VarLocation::Local(FrameReg::FrameA(2))),
        width: Width::Dword,
    })));
    assert!(blocks[3].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(FrameReg::FrameA(1)),
        src: Scalar::Var(VarLocation::Local(FrameReg::FrameA(3))),
        width: Width::Dword,
    })));
    assert_matches!(blocks[3].tail, Tail::Jump(2));
}

#[test]
fn test_deconstruct_3() {
    let blocks = vec![
        Block {
            phi: Phi::new(),
            ops: vec![],
            tail: Tail::Cond(
                JumpCondition::StrictBool(Scalar::Var(VarLocation::Local(01))),
                1,
                2,
            ),
            loop_depth: 0,
            original_id: 0,
        },
        Block {
            phi: Phi {
                srcs: HashMap::from([
                    (
                        11,
                        (Width::Dword, vec![(0, Scalar::Var(VarLocation::Local(01)))]),
                    ),
                    (
                        12,
                        (Width::Dword, vec![(0, Scalar::Var(VarLocation::Local(02)))]),
                    ),
                ]),
            },
            ops: vec![],
            tail: Tail::Ret,
            loop_depth: 0,
            original_id: 1,
        },
        Block {
            phi: Phi {
                srcs: HashMap::from([
                    (
                        21,
                        (Width::Dword, vec![(0, Scalar::Var(VarLocation::Local(01)))]),
                    ),
                    (
                        22,
                        (Width::Dword, vec![(0, Scalar::Var(VarLocation::Local(02)))]),
                    ),
                ]),
            },
            ops: vec![],
            tail: Tail::Ret,
            loop_depth: 0,
            original_id: 2,
        },
    ];
    let map = HashMap::from([
        (01, FrameReg::FrameA(1)),
        (02, FrameReg::FrameA(2)),
        (11, FrameReg::FrameA(1)),
        (12, FrameReg::FrameA(2)),
        (21, FrameReg::FrameA(3)),
        (22, FrameReg::FrameA(4)),
    ]);
    let (blocks, _) = deconstruct_ssa(blocks, &map);
    println!("{:#?}", blocks);
    assert_eq!(blocks.len(), 5);
    assert_eq!(blocks[0].ops, vec![]);
    let (new_block_1, new_block_2) = match blocks[0].tail {
        Tail::Cond(
            JumpCondition::StrictBool(Scalar::Var(VarLocation::Local(FrameReg::FrameA(1)))),
            a,
            b,
        ) => (a, b),
        _ => panic!("Wrong tail!"),
    };

    assert_eq!(blocks[1].ops.len(), 0);
    assert_eq!(blocks[2].ops.len(), 0);
    assert_eq!(blocks[new_block_1].ops.len(), 2);
    assert_eq!(blocks[new_block_2].ops.len(), 2);

    assert!(blocks[new_block_1].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(FrameReg::FrameA(1)),
        src: Scalar::Var(VarLocation::Local(FrameReg::FrameA(1))),
        width: Width::Dword,
    })));
    assert!(blocks[new_block_1].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(FrameReg::FrameA(2)),
        src: Scalar::Var(VarLocation::Local(FrameReg::FrameA(2))),
        width: Width::Dword,
    })));
    assert_matches!(blocks[new_block_1].tail, Tail::Jump(1));

    assert!(blocks[new_block_2].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(FrameReg::FrameA(3)),
        src: Scalar::Var(VarLocation::Local(FrameReg::FrameA(1))),
        width: Width::Dword,
    })));
    assert!(blocks[new_block_2].ops.contains(&Op::Copy(UnaryUnsignedOp {
        dst: VarLocation::Local(FrameReg::FrameA(4)),
        src: Scalar::Var(VarLocation::Local(FrameReg::FrameA(2))),
        width: Width::Dword,
    })));
    assert_matches!(blocks[new_block_2].tail, Tail::Jump(2));
}
