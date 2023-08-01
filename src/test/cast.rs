use crate::ir::{self};

use super::util::*;

#[test]
fn test_cast_1() {
    let (tu, ec) = compile("static int x; void foo(void) { *(char*)&x = 4; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 2);
    assert_eq!(
        body[1].ops,
        vec![
            ir::Op::Conv(ir::ConvOp {
                src: ir::Scalar::ConstInt(4),
                dst: ir::VarLocation::Local(1),
                dst_sign: false,
                dst_width: ir::Width::Byte,
                src_sign: true,
                src_width: ir::Width::Word,
            }),
            ir::Op::Store(ir::StoreOp {
                dst_addr: ir::Scalar::SymbolOffset(ir::GlobalVarId::Static("x".to_string()), 0),
                src: ir::Scalar::Var(ir::VarLocation::Local(1)),
                width: ir::Width::Byte
            })
        ]
    );
}
