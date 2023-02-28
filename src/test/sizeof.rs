use crate::ir::{self, VarLocation};

use super::util::*;

#[test]
fn test_sizof_val_1() {
    let (tu, ec) = compile("int bar(int x); void foo(void) { int x = sizeof(bar(5)); }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::ConstInt(2),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_sizof_ty_1() {
    let (tu, ec) = compile("void foo(void) { int x = sizeof(const unsigned long long int); }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::ConstInt(8),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_sizof_ty_2() {
    let (tu, ec) = compile("void foo(void) { int x = sizeof(struct { int x; long long y; }); }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::ConstInt(16),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_alignof_1() {
    let (tu, ec) = compile("void foo(void) { int x = _Alignof(struct { int x; long long y; }); }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::ConstInt(8),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_alignof_2() {
    let (tu, ec) = compile("void foo(void) { int x = _Alignof(char *); }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::ConstInt(2),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_alignof_3() {
    let (tu, ec) = compile("void foo(void) { int x = _Alignof(union { int x; long long y; }); }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::ConstInt(8),
            width: ir::Width::Word
        })]
    );
}
