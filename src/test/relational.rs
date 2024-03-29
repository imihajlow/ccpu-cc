use crate::ir::{self, VarLocation};

use super::util::*;

#[test]
fn test_rel_1() {
    let (tu, ec) = compile("void foo(void) { int x, y, z; x = y < z; }");
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
            ir::Op::Compare(ir::CompareOp {
                dst_width: ir::Width::Word,
                dst: VarLocation::Local(4),
                desc: ir::CompareDesc {
                    kind: ir::CompareKind::LessThan,
                    width: ir::Width::Word,
                    sign: true,
                    lhs: ir::Scalar::Var(VarLocation::Local(2)),
                    rhs: ir::Scalar::Var(VarLocation::Local(3))
                }
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
fn test_rel_2() {
    let (tu, ec) = compile("void foo(void) { int x, y; unsigned long z; x = y >= z; }");
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
                dst_width: ir::Width::Dword,
                dst_sign: false,
                src_width: ir::Width::Word,
                src_sign: true,
                src: ir::Scalar::Var(VarLocation::Local(2))
            }),
            ir::Op::Compare(ir::CompareOp {
                dst_width: ir::Width::Word,
                dst: VarLocation::Local(5),
                desc: ir::CompareDesc {
                    kind: ir::CompareKind::GreaterOrEqual,
                    width: ir::Width::Dword,
                    sign: false,
                    lhs: ir::Scalar::Var(VarLocation::Local(4)),
                    rhs: ir::Scalar::Var(VarLocation::Local(3))
                }
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
fn test_rel_3() {
    let (tu, ec) = compile("void foo(void) { int x, *y; void *z; x = y <= z; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 1);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Undefined(1),
            ir::Op::Undefined(2),
            ir::Op::Undefined(3),
            ir::Op::Compare(ir::CompareOp {
                dst_width: ir::Width::Word,
                dst: VarLocation::Local(4),
                desc: ir::CompareDesc {
                    kind: ir::CompareKind::LessOrEqual,
                    width: ir::Width::Word,
                    sign: false,
                    lhs: ir::Scalar::Var(VarLocation::Local(2)),
                    rhs: ir::Scalar::Var(VarLocation::Local(3))
                }
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
fn test_rel_4() {
    let (tu, ec) = compile("void foo(void) { int x, *y, * const z; x = y > z; }");
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
            ir::Op::Compare(ir::CompareOp {
                dst_width: ir::Width::Word,
                dst: VarLocation::Local(4),
                desc: ir::CompareDesc {
                    kind: ir::CompareKind::GreaterThan,
                    width: ir::Width::Word,
                    sign: false,
                    lhs: ir::Scalar::Var(VarLocation::Local(2)),
                    rhs: ir::Scalar::Var(VarLocation::Local(3))
                }
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
fn test_rel_5() {
    let (tu, ec) = compile("void foo(void) { int x, *y; void * const z; x = y == z; }");
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
            ir::Op::Compare(ir::CompareOp {
                dst_width: ir::Width::Word,
                dst: VarLocation::Local(4),
                desc: ir::CompareDesc {
                    kind: ir::CompareKind::Equal,
                    width: ir::Width::Word,
                    sign: false,
                    lhs: ir::Scalar::Var(VarLocation::Local(2)),
                    rhs: ir::Scalar::Var(VarLocation::Local(3))
                }
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
fn test_rel_6() {
    let (tu, ec) = compile("void foo(void) { int x, *y; char * z; x = y != z; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 1);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Undefined(1),
            ir::Op::Undefined(2),
            ir::Op::Undefined(3),
            ir::Op::Compare(ir::CompareOp {
                dst_width: ir::Width::Word,
                dst: VarLocation::Local(4),
                desc: ir::CompareDesc {
                    kind: ir::CompareKind::NotEqual,
                    width: ir::Width::Word,
                    sign: false,
                    lhs: ir::Scalar::Var(VarLocation::Local(2)),
                    rhs: ir::Scalar::Var(VarLocation::Local(3))
                }
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::Var(VarLocation::Local(4)),
                width: ir::Width::Word
            })
        ]
    );
}
