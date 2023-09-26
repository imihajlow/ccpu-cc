use crate::ccpu::reg::FrameReg;
use crate::ir::*;
use crate::{
    ir::{self, Tail},
    regalloc::allocate_registers,
};

#[test]
fn test_regalloc_args_1() {
    let body = [ir::Block {
        phi: Phi::new(),
        ops: vec![
            Op::Copy(UnaryUnsignedOp {
                dst: VarLocation::Local(13),
                src: Scalar::ConstInt(13),
                width: Width::Byte,
            }),
            Op::Arg(ArgOp {
                dst_reg: 42,
                arg_number: 1,
                width: Width::Word,
            }),
            Op::Arg(ArgOp {
                dst_reg: 239,
                arg_number: 0,
                width: Width::Word,
            }),
        ],
        tail: Tail::Ret,
        loop_depth: 0,
        original_id: 0,
    }];
    let map = allocate_registers(&body);
    assert_eq!(map.len(), 3);
    assert_eq!(map.get(&239), Some(&FrameReg::FrameA(0)));
    assert_eq!(map.get(&42), Some(&FrameReg::FrameA(1)));
    assert_eq!(map.get(&13), Some(&FrameReg::FrameA(2)));
}
