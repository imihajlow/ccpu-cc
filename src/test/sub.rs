use crate::ir::{self, VarLocation};

use super::util::*;

#[test]
fn test_sub_1() {
    let (tu, ec) = compile("void foo(void) { int x, y, z; x = y - z; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Sub(ir::BinaryOp {
                dst: VarLocation::Local(3),
                width: ir::Width::Word,
                sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::Var(VarLocation::Local(2))
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_sub_ptr_1() {
    let (tu, ec) = compile("void foo(void) { int *x, *y, z; x = y - z; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Mul(ir::BinaryOp {
                dst: VarLocation::Local(3),
                width: ir::Width::Word,
                sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(2)),
                rhs: ir::Scalar::ConstInt(2)
            }),
            ir::Op::Sub(ir::BinaryOp {
                dst: VarLocation::Local(4),
                width: ir::Width::Word,
                sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::Var(VarLocation::Local(3))
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::Var(VarLocation::Local(4)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_add_ptr_2() {
    let (tu, ec) = compile("void foo(void) { long *x, *y; int z; z = y - x; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Sub(ir::BinaryOp {
                dst: VarLocation::Local(3),
                width: ir::Width::Word,
                sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::Var(VarLocation::Local(0))
            }),
            ir::Op::Div(ir::BinaryOp {
                dst: VarLocation::Local(4),
                width: ir::Width::Word,
                sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(3)),
                rhs: ir::Scalar::ConstInt(4)
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
fn test_assign_sub_1() {
    let (tu, ec) = compile("void foo(void) { int x, y; x -= y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Sub(ir::BinaryOp {
                dst: VarLocation::Local(2),
                width: ir::Width::Word,
                sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(0)),
                rhs: ir::Scalar::Var(VarLocation::Local(1))
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::Var(VarLocation::Local(2)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_assign_sub_2() {
    let (tu, ec) = compile("void foo(void) { int *x, y; *x -= y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Load(ir::LoadOp {
                dst: VarLocation::Local(2),
                src_addr: ir::Scalar::Var(VarLocation::Local(0)),
                width: ir::Width::Word,
            }),
            ir::Op::Sub(ir::BinaryOp {
                dst: VarLocation::Local(3),
                width: ir::Width::Word,
                sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(2)),
                rhs: ir::Scalar::Var(VarLocation::Local(1))
            }),
            ir::Op::Store(ir::StoreOp {
                dst_addr: ir::Scalar::Var(VarLocation::Local(0)),
                src: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word
            })
        ]
    );
}
