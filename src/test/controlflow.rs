use crate::ir::{self, VarLocation};

use super::util::*;

#[test]
fn test_ternary_1() {
    let (tu, ec) = compile("void foo(void) { int x, y, z; x = x ? y : z; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 5);
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Undefined(1),
            ir::Op::Undefined(2),
            ir::Op::Undefined(3),
            ir::Op::Bool(ir::UnaryUnsignedOp {
                dst: VarLocation::Local(4),
                src: ir::Scalar::Var(VarLocation::Local(1)),
                width: ir::Width::Word
            })
        ]
    );
    assert_eq!(
        body[1].tail,
        ir::Tail::Cond(ir::Scalar::Var(VarLocation::Local(4)), 2, 3)
    );
    assert_eq!(
        body[2].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(5),
            src: ir::Scalar::Var(VarLocation::Local(2)),
            width: ir::Width::Word
        })]
    );
    assert_eq!(body[2].tail, ir::Tail::Jump(4));
    assert_eq!(
        body[3].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(5),
            src: ir::Scalar::Var(VarLocation::Local(3)),
            width: ir::Width::Word
        })]
    );
    assert_eq!(body[3].tail, ir::Tail::Jump(4));
    assert_eq!(
        body[4].ops,
        vec![ir::Op::Copy(ir::UnaryUnsignedOp {
            dst: VarLocation::Local(1),
            src: ir::Scalar::Var(VarLocation::Local(5)),
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
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Conv(ir::ConvOp {
                dst: VarLocation::Local(1),
                dst_width: ir::Width::Byte,
                dst_sign: false,
                src: ir::Scalar::ConstInt(20),
                src_width: ir::Width::Word,
                src_sign: true,
            }),
            ir::Op::Call(ir::CallOp {
                addr: ir::Scalar::SymbolOffset(ir::GlobalVarId::Global("bar".to_string()), 0),
                dst: None,
                args: vec![
                    (ir::Scalar::ConstInt(10), ir::Width::Word),
                    (ir::Scalar::Var(VarLocation::Local(1)), ir::Width::Byte)
                ],
                va_args: vec![]
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
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Call(ir::CallOp {
                addr: ir::Scalar::SymbolOffset(ir::GlobalVarId::Global("bar".to_string()), 0),
                dst: Some((VarLocation::Local(2), ir::Width::Dword)),
                args: vec![(ir::Scalar::ConstInt(10), ir::Width::Word),],
                va_args: vec![]
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
                dst: VarLocation::Local(1),
                src: ir::Scalar::Var(VarLocation::Local(3)),
                width: ir::Width::Word
            }),
        ]
    );
}

#[test]
fn test_call_3() {
    let (tu, ec) = compile("void (*bar)(int x); void foo(void) { bar(10); }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[1].ops,
        vec![ir::Op::Call(ir::CallOp {
            addr: ir::Scalar::Var(VarLocation::Global(ir::GlobalVarId::Global(
                "bar".to_string()
            ))),
            dst: None,
            args: vec![(ir::Scalar::ConstInt(10), ir::Width::Word),],
            va_args: vec![]
        }),]
    );
}

#[test]
fn test_call_4() {
    let (tu, ec) = compile("void (*bar)(int x); void foo(void) { (*bar)(10); }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[1].ops,
        vec![ir::Op::Call(ir::CallOp {
            addr: ir::Scalar::Var(VarLocation::Global(ir::GlobalVarId::Global(
                "bar".to_string()
            ))),
            dst: None,
            args: vec![(ir::Scalar::ConstInt(10), ir::Width::Word),],
            va_args: vec![]
        }),]
    );
}
