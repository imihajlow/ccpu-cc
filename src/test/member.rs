use crate::ir::{self, VarLocation};

use super::util::*;

#[test]
fn test_member_1() {
    let (tu, ec) =
        compile("struct X { long a; int b; }; void foo(void) { struct X x; int y; y = x.b; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::FramePointer(0),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(1),
                width: ir::Width::PTR_WIDTH,
                sign: false,
                lhs: ir::Scalar::Var(VarLocation::Local(0)),
                rhs: ir::Scalar::ConstInt(0)
            }),
        ]
    );
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Undefined(2),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(3),
                width: ir::Width::PTR_WIDTH,
                sign: false,
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::ConstInt(4)
            }),
            ir::Op::Load(ir::LoadOp {
                dst: VarLocation::Local(4),
                src_addr: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word,
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(2),
                src: ir::Scalar::Var(VarLocation::Local(4)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_member_2() {
    let (tu, ec) =
        compile("struct X { long a; int b; }; void foo(void) { struct X *x; int y; y = x->b; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(body[0].ops, vec![ir::Op::FramePointer(0),]);
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Undefined(1),
            ir::Op::Undefined(2),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(3),
                width: ir::Width::PTR_WIDTH,
                sign: false,
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::ConstInt(4)
            }),
            ir::Op::Load(ir::LoadOp {
                dst: VarLocation::Local(4),
                src_addr: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word,
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(2),
                src: ir::Scalar::Var(VarLocation::Local(4)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_member_3() {
    let (tu, ec) = compile("struct X { long a; }; struct Y { int i; struct X x; }; void foo(void) { struct X x; struct Y y; x = y.x; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::FramePointer(0),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(1),
                width: ir::Width::PTR_WIDTH,
                sign: false,
                lhs: ir::Scalar::Var(VarLocation::Local(0)),
                rhs: ir::Scalar::ConstInt(0)
            }),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(2),
                width: ir::Width::PTR_WIDTH,
                sign: false,
                lhs: ir::Scalar::Var(VarLocation::Local(0)),
                rhs: ir::Scalar::ConstInt(4)
            }),
        ]
    );
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(3),
                width: ir::Width::PTR_WIDTH,
                sign: false,
                lhs: ir::Scalar::Var(VarLocation::Local(2)),
                rhs: ir::Scalar::ConstInt(4)
            }),
            ir::Op::Memcpy(ir::MemcpyOp {
                dst_addr: ir::Scalar::Var(VarLocation::Local(1)),
                src_addr: ir::Scalar::Var(VarLocation::Local(3)),
                len: 4
            })
        ]
    );
}

#[test]
fn test_member_4() {
    let (tu, ec) = compile("struct X { long a; }; struct Y { int i; struct X x; }; void foo(void) { struct X x; struct Y *y; x = y->x; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::FramePointer(0),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(1),
                width: ir::Width::PTR_WIDTH,
                sign: false,
                lhs: ir::Scalar::Var(VarLocation::Local(0)),
                rhs: ir::Scalar::ConstInt(0)
            }),
        ]
    );
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Undefined(2),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(3),
                width: ir::Width::PTR_WIDTH,
                sign: false,
                lhs: ir::Scalar::Var(VarLocation::Local(2)),
                rhs: ir::Scalar::ConstInt(4)
            }),
            ir::Op::Memcpy(ir::MemcpyOp {
                dst_addr: ir::Scalar::Var(VarLocation::Local(1)),
                src_addr: ir::Scalar::Var(VarLocation::Local(3)),
                len: 4
            })
        ]
    );
}
