use crate::ir::{self, VarLocation};

use super::util::*;

#[test]
fn test_lvalue_1() {
    let (tu, ec) = compile("void foo(void) { int *x, y; *x = y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Undefined(1),
            ir::Op::Undefined(2),
            ir::Op::Store(ir::StoreOp {
                dst_addr: ir::Scalar::Var(VarLocation::Local(1)),
                src: ir::Scalar::Var(VarLocation::Local(2)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_lvalue_2() {
    let (tu, ec) = compile("void foo(void) { int *x, y, z; x[y] = z; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Undefined(1),
            ir::Op::Undefined(2),
            ir::Op::Undefined(3),
            ir::Op::Mul(ir::BinaryOp {
                dst: VarLocation::Local(4),
                lhs: ir::Scalar::Var(VarLocation::Local(2)),
                rhs: ir::Scalar::ConstInt(2),
                width: ir::Width::Word,
                sign: true,
            }),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(5),
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::Var(VarLocation::Local(4)),
                width: ir::Width::Word,
                sign: false,
            }),
            ir::Op::Store(ir::StoreOp {
                dst_addr: ir::Scalar::Var(VarLocation::Local(5)),
                src: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_lvalue_3() {
    let (tu, ec) = compile("void foo(void) { int *x; char y; int z; x[y] = z; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Undefined(1),
            ir::Op::Undefined(2),
            ir::Op::Undefined(3),
            ir::Op::Conv(ir::ConvOp {
                dst: VarLocation::Local(4),
                dst_width: ir::Width::Word,
                dst_sign: true,
                src: ir::Scalar::Var(VarLocation::Local(2)),
                src_width: ir::Width::Byte,
                src_sign: false,
            }),
            ir::Op::Mul(ir::BinaryOp {
                dst: VarLocation::Local(5),
                lhs: ir::Scalar::Var(VarLocation::Local(4)),
                rhs: ir::Scalar::ConstInt(2),
                width: ir::Width::Word,
                sign: true,
            }),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(6),
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::Var(VarLocation::Local(5)),
                width: ir::Width::Word,
                sign: false,
            }),
            ir::Op::Store(ir::StoreOp {
                dst_addr: ir::Scalar::Var(VarLocation::Local(6)),
                src: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_lvalue_member_1() {
    let (tu, ec) =
        compile("struct X { long a; int b; }; void foo(void) { struct X x; int y; x.b = y; }");
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
            ir::Op::Store(ir::StoreOp {
                dst_addr: ir::Scalar::Var(VarLocation::Local(3)),
                src: ir::Scalar::Var(VarLocation::Local(2)),
                width: ir::Width::Word,
            })
        ]
    );
}

#[test]
fn test_lvalue_member_2() {
    let (tu, ec) =
        compile("struct X { long a; int b; }; void foo(void) { struct X *x; int y; x->b = y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(body[0].ops, vec![ir::Op::FramePointer(0)]);
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
            ir::Op::Store(ir::StoreOp {
                src: ir::Scalar::Var(VarLocation::Local(2)),
                dst_addr: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word,
            })
        ]
    );
}

#[test]
fn test_lvalue_member_3() {
    let (tu, ec) = compile("struct X { long a; }; struct Y { int i; struct X x; }; void foo(void) { struct X x; struct Y y; y.x = x; }");
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
                width: ir::Width::Word,
                sign: false,
                lhs: ir::Scalar::Var(VarLocation::Local(2)),
                rhs: ir::Scalar::ConstInt(4)
            }),
            ir::Op::Memcpy(ir::MemcpyOp {
                dst_addr: ir::Scalar::Var(VarLocation::Local(3)),
                src_addr: ir::Scalar::Var(VarLocation::Local(1)),
                len: 4
            })
        ]
    );
}

#[test]
fn test_lvalue_member_4() {
    let (tu, ec) = compile("struct X { long a; }; struct Y { int i; struct X x; }; void foo(void) { struct X x; struct Y *y; y->x = x; }");
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
                width: ir::Width::Word,
                sign: false,
                lhs: ir::Scalar::Var(VarLocation::Local(2)),
                rhs: ir::Scalar::ConstInt(4)
            }),
            ir::Op::Memcpy(ir::MemcpyOp {
                src_addr: ir::Scalar::Var(VarLocation::Local(1)),
                dst_addr: ir::Scalar::Var(VarLocation::Local(3)),
                len: 4
            })
        ]
    );
}
