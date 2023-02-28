use crate::ir::{self, VarLocation};

use super::util::*;

#[test]
fn test_unary_1() {
    let (tu, ec) = compile("void foo(void) { int x, y; x = +y; }");
    ec.print_issues();
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
fn test_unary_2() {
    let (tu, ec) = compile("void foo(void) { long x; unsigned int y; x = -y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Neg(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(2),
                src: ir::Scalar::Var(VarLocation::Local(1)),
                width: ir::Width::Word,
            }),
            ir::Op::Conv(ir::ConvOp {
                dst: VarLocation::Local(3),
                src: ir::Scalar::Var(VarLocation::Local(2)),
                dst_width: ir::Width::Dword,
                src_width: ir::Width::Word,
                dst_sign: true,
                src_sign: false
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Dword
            })
        ]
    );
}

#[test]
fn test_unary_3() {
    let (tu, ec) = compile("void foo(void) { int x, y; x = ~y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Not(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(2),
                src: ir::Scalar::Var(VarLocation::Local(1)),
                width: ir::Width::Word,
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
fn test_unary_4() {
    let (tu, ec) = compile("void foo(void) { int x, y; x = !y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::BoolInv(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(2),
                src: ir::Scalar::Var(VarLocation::Local(1)),
                width: ir::Width::Word,
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
fn test_unary_5() {
    let (tu, ec) = compile("void foo(void) { int x, y; x = ++y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(2),
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::ConstInt(1),
                width: ir::Width::Word,
                sign: true,
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::Var(VarLocation::Local(2)),
                width: ir::Width::Word
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
fn test_unary_6() {
    let (tu, ec) = compile("void foo(void) { long int *x, *y; x = --y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Mul(ir::BinaryOp {
                dst: VarLocation::Local(2),
                lhs: ir::Scalar::ConstInt(1),
                rhs: ir::Scalar::ConstInt(4),
                width: ir::Width::Word,
                sign: true,
            }),
            ir::Op::Sub(ir::BinaryOp {
                dst: VarLocation::Local(3),
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::Var(VarLocation::Local(2)),
                width: ir::Width::Word,
                sign: true,
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word
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
fn test_unary_7() {
    let (tu, ec) = compile("void foo(void) { int x, y; x = y++; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(2),
                src: ir::Scalar::Var(VarLocation::Local(1)),
                width: ir::Width::Word
            }),
            ir::Op::Add(ir::BinaryOp {
                dst: VarLocation::Local(3),
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::ConstInt(1),
                width: ir::Width::Word,
                sign: true,
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word
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
fn test_unary_8() {
    let (tu, ec) = compile("void foo(void) { long int *x, *y; x = y--; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(2),
                src: ir::Scalar::Var(VarLocation::Local(1)),
                width: ir::Width::Word
            }),
            ir::Op::Mul(ir::BinaryOp {
                dst: VarLocation::Local(3),
                lhs: ir::Scalar::ConstInt(1),
                rhs: ir::Scalar::ConstInt(4),
                width: ir::Width::Word,
                sign: true,
            }),
            ir::Op::Sub(ir::BinaryOp {
                dst: VarLocation::Local(4),
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word,
                sign: true,
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::Var(VarLocation::Local(4)),
                width: ir::Width::Word
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
fn test_addr_1() {
    let (tu, ec) = compile("void foo(void) { int *x, y; x = &y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    let fixed_regs = tu.scope.get_fixed_regs();
    assert_eq!(fixed_regs.len(), 1);
    assert!(fixed_regs.contains(&1));
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::LoadAddr(ir::LoadAddrOp {
                dst: VarLocation::Local(2),
                src: VarLocation::Local(1),
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
fn test_addr_2() {
    let (tu, ec) = compile("void foo(void) { int *x, *y; x = &*y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    let fixed_regs = tu.scope.get_fixed_regs();
    assert_eq!(fixed_regs.len(), 0);
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
fn test_deref_1() {
    let (tu, ec) = compile("void foo(void) { int x, *y; x = *y; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    let fixed_regs = tu.scope.get_fixed_regs();
    assert_eq!(fixed_regs.len(), 0);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Load(ir::LoadOp {
                dst: VarLocation::Local(2),
                src_addr: ir::Scalar::Var(VarLocation::Local(1)),
                width: ir::Width::Word,
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::Var(VarLocation::Local(2)),
                width: ir::Width::Word
            })
        ]
    );
}
