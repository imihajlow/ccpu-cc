use crate::ir;
use crate::ir::VarLocation;

use super::util::*;

#[test]
fn test_struct_1() {
    let (tu, ec) = compile("struct X { int x; }; void foo(void) { struct X x; struct X *p = &x; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
}

#[test]
fn test_array_1() {
    let (tu, ec) = compile("void foo(void) { long a[15]; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let f = get_first_function(&tu);
    assert_eq!(f.get_frame_size(), 15 * 4);
}

#[test]
fn test_array_2() {
    let (tu, ec) = compile("void foo(void) { int a[15]; int x = sizeof(a); }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(0),
                width: ir::Width::PTR_WIDTH,
                sign: false,
                lhs: ir::Scalar::FramePointer,
                rhs: ir::Scalar::ConstInt(0)
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::ConstInt(15 * 2),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_array_3() {
    let (tu, ec) = compile("void foo(void) { int a[15]; int *x = a; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(0),
                width: ir::Width::PTR_WIDTH,
                sign: false,
                lhs: ir::Scalar::FramePointer,
                rhs: ir::Scalar::ConstInt(0)
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::Var(VarLocation::Local(0)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_array_4() {
    let (tu, ec) = compile("void foo(void) { int a[15]; int *x = &a; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 1);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(0),
                width: ir::Width::PTR_WIDTH,
                sign: false,
                lhs: ir::Scalar::FramePointer,
                rhs: ir::Scalar::ConstInt(0)
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::Var(VarLocation::Local(0)),
                width: ir::Width::Word
            })
        ]
    );
}

#[test]
fn test_array_5() {
    let (tu, ec) = compile("void foo(void) { int a[15]; int *x = a + 1; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(0),
                width: ir::Width::PTR_WIDTH,
                sign: false,
                lhs: ir::Scalar::FramePointer,
                rhs: ir::Scalar::ConstInt(0)
            }),
            ir::Op::Mul(ir::BinaryOp {
                dst: VarLocation::Local(2),
                lhs: ir::Scalar::ConstInt(1),
                rhs: ir::Scalar::ConstInt(2),
                width: ir::Width::Word,
                sign: true,
            }),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(3),
                lhs: ir::Scalar::Var(VarLocation::Local(0)),
                rhs: ir::Scalar::Var(VarLocation::Local(2)),
                width: ir::Width::Word,
                sign: false,
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
fn test_array_6() {
    let (tu, ec) = compile("void foo(void) { int a[15]; int *x = &a + 1; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 1);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(0),
                width: ir::Width::PTR_WIDTH,
                sign: false,
                lhs: ir::Scalar::FramePointer,
                rhs: ir::Scalar::ConstInt(0)
            }),
            ir::Op::Mul(ir::BinaryOp {
                dst: VarLocation::Local(2),
                lhs: ir::Scalar::ConstInt(1),
                rhs: ir::Scalar::ConstInt(15 * 2),
                width: ir::Width::Word,
                sign: true,
            }),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(3),
                lhs: ir::Scalar::Var(VarLocation::Local(0)),
                rhs: ir::Scalar::Var(VarLocation::Local(2)),
                width: ir::Width::Word,
                sign: false,
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word
            })
        ]
    );
}
