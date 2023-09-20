use replace_with::replace_with_or_abort;

use crate::{
    generic_ir::{IntrinCallOp, IntrinCallVariant, Op, Scalar, Width},
    ir,
};

pub fn replace_with_intrinsic(op: &mut Op<ir::VirtualReg>) {
    replace_with_or_abort(op, |op| match op {
        Op::Mul(op) => {
            let name = match op.width {
                Width::Byte => "mul_byte",
                Width::Word => "mul_word",
                Width::Dword => "mul_dword",
                Width::Qword => unimplemented!("64-bit multiplication"),
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
        Op::Div(op) => {
            let name = match (op.sign, op.width) {
                (false, Width::Byte) => "udiv_byte",
                (false, Width::Word) => "udiv_word",
                (false, Width::Dword) => "udiv_dword",
                (true, Width::Byte) => "div_byte",
                (true, Width::Word) => "div_word",
                (true, Width::Dword) => unimplemented!("32-bit signed division"),
                (_, Width::Qword) => unimplemented!("64-bit division"),
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
        Op::Mod(op) => {
            let name = match (op.sign, op.width) {
                (false, Width::Byte) => "umod_byte",
                (false, Width::Word) => "umod_word",
                (false, Width::Dword) => "umod_dword",
                (true, Width::Byte) => "mod_byte",
                (true, Width::Word) => "mod_word",
                (true, Width::Dword) => unimplemented!("32-bit signed division"),
                (_, Width::Qword) => unimplemented!("64-bit division"),
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
        Op::LShift(op) => match op.rhs {
            Scalar::SymbolOffset(_, _) | Scalar::Var(_) => {
                let name = match op.lhs_width {
                    Width::Byte => "asl_byte",
                    Width::Word => "asl_word",
                    Width::Dword => "asl_dword",
                    Width::Qword => unimplemented!("left shift of a qword"),
                }
                .to_string();
                Op::IntrinCall(IntrinCallOp {
                    name,
                    variant: IntrinCallVariant::Call2R(
                        (op.lhs_width, op.dst),
                        (op.lhs_width, op.lhs),
                        (Width::Byte, op.rhs),
                    ),
                })
            }
            Scalar::ConstInt(_) => Op::LShift(op),
        },
        Op::RShift(op) => match op.rhs {
            Scalar::SymbolOffset(_, _) | Scalar::Var(_) => {
                let name = match (op.lhs_sign, op.lhs_width) {
                    (_, Width::Byte) => unimplemented!("right shift of a byte"),
                    (true, Width::Word) => "asr_word",
                    (true, Width::Dword) => "asr_dword",
                    (false, Width::Word) => "lsr_word",
                    (false, Width::Dword) => "lsr_dword",
                    (_, Width::Qword) => unimplemented!("right shift of a qword"),
                }
                .to_string();
                Op::IntrinCall(IntrinCallOp {
                    name,
                    variant: IntrinCallVariant::Call2R(
                        (op.lhs_width, op.dst),
                        (op.lhs_width, op.lhs),
                        (Width::Byte, op.rhs),
                    ),
                })
            }
            Scalar::ConstInt(_) => Op::RShift(op),
        },
        _ => op,
    });
}
