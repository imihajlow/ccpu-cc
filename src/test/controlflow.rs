use crate::ir::{self, VarLocation};

use super::util::*;

#[test]
fn test_ternary_1() {
    let (tu, ec) = compile("void foo(void) { int x, y, z; x = x ? y : z; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 4);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Undefined(0),
            ir::Op::Undefined(1),
            ir::Op::Undefined(2),
            ir::Op::Bool(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(3),
                src: ir::Scalar::Var(VarLocation::Local(0)),
                width: ir::Width::Word
            })
        ]
    );
    assert_eq!(
        body[0].tail,
        ir::Tail::Cond(ir::Scalar::Var(VarLocation::Local(3)), 1, 2)
    );
    assert_eq!(
        body[1].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(4),
            src: ir::Scalar::Var(VarLocation::Local(1)),
            width: ir::Width::Word
        })]
    );
    assert_eq!(body[1].tail, ir::Tail::Jump(3));
    assert_eq!(
        body[2].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(4),
            src: ir::Scalar::Var(VarLocation::Local(2)),
            width: ir::Width::Word
        })]
    );
    assert_eq!(body[2].tail, ir::Tail::Jump(3));
    assert_eq!(
        body[3].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(0),
            src: ir::Scalar::Var(VarLocation::Local(4)),
            width: ir::Width::Word
        })]
    );
}

#[test]
fn test_call_1() {
    let (tu, ec) = compile("void bar(int x, unsigned char y); void foo(void) { bar(10, 20); }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::ConstInt(10),
                width: ir::Width::Word
            }),
            ir::Op::Conv(ir::ConvOp {
                dst: VarLocation::Local(1),
                dst_width: ir::Width::Byte,
                dst_sign: false,
                src: ir::Scalar::ConstInt(20),
                src_width: ir::Width::Word,
                src_sign: true,
            }),
            ir::Op::Call(ir::CallOp {
                addr: ir::Scalar::Var(VarLocation::Global(ir::GlobalVarId("bar".to_string(), 0))),
                dst: None,
                args: vec![
                    (ir::Scalar::Var(VarLocation::Local(0)), ir::Width::Word),
                    (ir::Scalar::Var(VarLocation::Local(1)), ir::Width::Byte)
                ]
            })
        ]
    );
}

#[test]
fn test_call_2() {
    let (tu, ec) = compile("long bar(int x); void foo(void) { int x = bar(10); }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
    assert_eq!(
        body[0].ops,
        vec![
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(1),
                src: ir::Scalar::ConstInt(10),
                width: ir::Width::Word
            }),
            ir::Op::Call(ir::CallOp {
                addr: ir::Scalar::Var(VarLocation::Global(ir::GlobalVarId("bar".to_string(), 0))),
                dst: Some((VarLocation::Local(2), ir::Width::Dword)),
                args: vec![(ir::Scalar::Var(VarLocation::Local(1)), ir::Width::Word),]
            }),
            ir::Op::Conv(ir::ConvOp {
                dst: VarLocation::Local(3),
                dst_width: ir::Width::Word,
                dst_sign: true,
                src: ir::Scalar::Var(VarLocation::Local(2)),
                src_width: ir::Width::Dword,
                src_sign: true,
            }),
            ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(0),
                src: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word
            }),
        ]
    );
}
