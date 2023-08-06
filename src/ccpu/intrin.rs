use replace_with::replace_with_or_abort;

use crate::{
    generic_ir::{IntrinCallOp, IntrinCallVariant, Op, Width},
    ir,
};

pub fn replace_with_intrinsic(op: &mut Op<ir::VirtualReg>) {
    replace_with_or_abort(op, |op| match op {
        Op::Mul(op) => {
            let name = match op.width {
                Width::Byte => "mul_byte",
                Width::Word => "mul_word",
                Width::Dword => "mul_dword",
                Width::Qword => "mul_qword",
            }
            .to_string();
            Op::IntrinCall(IntrinCallOp {
                name,
                variant: IntrinCallVariant::Call2R(
                    (op.width, op.dst),
                    (op.width, op.lhs),
                    (op.width, op.rhs),
                ),
            })
        }
        _ => op,
    });
}
