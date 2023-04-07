use crate::ir::{self, VarLocation};

use super::util::*;

#[test]
fn test_add_1() {
    let (tu, ec) = compile("void foo(void) { int x, y, z; x = y + z; }");
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
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(4),
                width: ir::Width::Word,
                sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(2)),
                rhs: ir::Scalar::Var(VarLocation::Local(3))
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::Var(VarLocation::Local(4)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_add_2() {
    let (tu, ec) = compile("void foo(void) { int x, y; char z; x = y + z; }");
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
                src: ir::Scalar::Var(VarLocation::Local(3)),
                dst_width: ir::Width::Word,
                dst_sign: true,
                src_width: ir::Width::Byte,
                src_sign: false,
            }),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(5),
                width: ir::Width::Word,
                sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(2)),
                rhs: ir::Scalar::Var(VarLocation::Local(4))
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::Var(VarLocation::Local(5)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_add_ptr_1() {
    let (tu, ec) = compile("void foo(void) { int *x, *y, z; x = y + z; }");
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
                width: ir::Width::Word,
                sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(3)),
                rhs: ir::Scalar::ConstInt(2)
            }),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(5),
                width: ir::Width::Word,
                sign: false,
                lhs: ir::Scalar::Var(VarLocation::Local(2)),
                rhs: ir::Scalar::Var(VarLocation::Local(4))
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::Var(VarLocation::Local(5)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_assign_add_1() {
    let (tu, ec) = compile("void foo(void) { int x, y; x += y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Undefined(1),
            ir::Op::Undefined(2),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(3),
                width: ir::Width::Word,
                sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::Var(VarLocation::Local(2))
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_assign_add_2() {
    let (tu, ec) = compile("void foo(void) { int *x, y; *x += y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Undefined(1),
            ir::Op::Undefined(2),
            ir::Op::Load(ir::LoadOp {
                dst: VarLocation::Local(3),
                src_addr: ir::Scalar::Var(VarLocation::Local(1)),
                width: ir::Width::Word,
            }),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(4),
                width: ir::Width::Word,
                sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(3)),
                rhs: ir::Scalar::Var(VarLocation::Local(2))
            }),
            ir::Op::Store(ir::StoreOp {
                dst_addr: ir::Scalar::Var(VarLocation::Local(1)),
                src: ir::Scalar::Var(VarLocation::Local(4)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_index_1() {
    let (tu, ec) = compile("void foo(void) { long *x; int y; x[y]; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Undefined(1),
            ir::Op::Undefined(2),
            ir::Op::Mul(ir::BinaryOp {
                dst: VarLocation::Local(3),
                width: ir::Width::Word,
                sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(2)),
                rhs: ir::Scalar::ConstInt(4)
            }),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(4),
                width: ir::Width::Word,
                sign: false,
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::Var(VarLocation::Local(3))
            }),
            ir::Op::Load(ir::LoadOp {
                dst: VarLocation::Local(5),
                src_addr: ir::Scalar::Var(VarLocation::Local(4)),
                width: ir::Width::Dword,
            }),
        ]
    );
}
