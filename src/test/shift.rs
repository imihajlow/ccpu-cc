use crate::ir::{self, VarLocation};

use super::util::*;

#[test]
fn test_shift_1() {
    let (tu, ec) = compile("void foo(void) { long x, y; int z; x = y << z; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::LShift(ir::ShiftOp {
                dst: VarLocation::Local(3),
                lhs_width: ir::Width::Dword,
                lhs_sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(1)),
                rhs: ir::Scalar::Var(VarLocation::Local(2))
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
fn test_shift_2() {
    let (tu, ec) = compile("void foo(void) { long x; int z; x >>= z; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::RShift(ir::ShiftOp {
                dst: VarLocation::Local(2),
                lhs_width: ir::Width::Dword,
                lhs_sign: true,
                lhs: ir::Scalar::Var(VarLocation::Local(0)),
                rhs: ir::Scalar::Var(VarLocation::Local(1))
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::Var(VarLocation::Local(2)),
                width: ir::Width::Dword
            })
        ]
    );
}
