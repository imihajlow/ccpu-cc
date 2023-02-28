use crate::ir::{self, VarLocation};

use super::util::*;

#[test]
fn test_assign_1() {
    let (tu, ec) = compile("void foo(void) { int x; x = 15; }");
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::ConstInt(15),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_assign_2() {
    let (tu, ec) = compile("void foo(void) { char x; x = 15; }");
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Conv(ir::ConvOp {
                dst: VarLocation::Local(1),
                dst_sign: false,
                dst_width: ir::Width::Byte,
                src: ir::Scalar::ConstInt(15),
                src_sign: true,
                src_width: ir::Width::Word
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::Var(VarLocation::Local(1)),
                width: ir::Width::Byte
            }),
        ]
    );
}

#[test]
fn test_assign_3() {
    let (tu, ec) = compile("void foo(void) { int *x; *x = 15; }");
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Store(ir::StoreOp {
            dst_addr: ir::Scalar::Var(VarLocation::Local(0)),
            src: ir::Scalar::ConstInt(15),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_assign_4() {
    let (tu, ec) = compile("void foo(void) { int x; x = 15; }");
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::ConstInt(15),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_assign_5() {
    let (tu, ec) = compile("void foo(void) { int *x; int *y; x = y; }");
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::Var(VarLocation::Local(1)),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_assign_6() {
    let (tu, ec) = compile("void foo(void) { char *x; char y[43]; x = y; }");
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::FrameOffset(0),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_assign_7() {
    let (tu, ec) = compile("void foo(void) { int *x; unsigned int y; x = y; }");
    assert_eq!(ec.get_warning_count(), 1);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::Var(VarLocation::Local(1)),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_assign_8() {
    let (tu, ec) = compile("void foo(void) { int *x; char *y; x = y; }");
    assert_eq!(ec.get_warning_count(), 1);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::Var(VarLocation::Local(1)),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_assign_9() {
    let (tu, ec) = compile("void foo(void) { int *x; void *y; x = y; }");
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::Var(VarLocation::Local(1)),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_assign_10() {
    let (tu, ec) = compile("void foo(void) { void *x; int *y; x = y; }");
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::Var(VarLocation::Local(1)),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_assign_11() {
    let (tu, ec) = compile("void foo(void) { int *x; const int *y; x = y; }");
    assert_eq!(ec.get_warning_count(), 1);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::Var(VarLocation::Local(1)),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_assign_12() {
    assert_compile_error("void foo(void) { int *x; float y; x = y; }");
}

#[test]
fn test_assign_13() {
    assert_compile_error("struct X { int y; }; void foo(void) { struct X x; int y; x = y; }");
}

#[test]
fn test_assign_14() {
    assert_compile_error("struct X { int y; }; struct Y { int x; }; void foo(void) { struct X x; struct Y y; x = y; }");
}

#[test]
fn test_assign_15() {
    let (tu, ec) =
        compile("struct X { int x; long long y; }; void foo(void) { struct X x, y; x = y; }");
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Memcpy(ir::MemcpyOp {
            dst_addr: ir::Scalar::FrameOffset(0),
            src_addr: ir::Scalar::FrameOffset(16),
            len: 16
        })]
    );
}
