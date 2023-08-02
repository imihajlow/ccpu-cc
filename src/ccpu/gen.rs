use crate::{
    function::Function,
    generic_ir::{self, ArgOp, Scalar, VarLocation},
    translation_unit::TranslationUnit,
};

use super::{
    global,
    instr::{InstructionWriter, Reg, Reg::*},
    reg::FrameReg,
    stack,
};

mod add;
mod bitwise;
mod boolean;
mod call;
mod conv;
mod copy;
mod load;
mod neg;
mod shift;
mod store;
mod sub;
mod util;

pub fn gen_tu(tu: TranslationUnit<FrameReg>) -> InstructionWriter {
    let mut w = InstructionWriter::new();
    w.import(global::RET_VALUE_REG_SYMBOL.to_string());
    for f in tu.functions.into_iter() {
        gen_function(&mut w, f);
    }
    w
}

fn gen_function(w: &mut InstructionWriter, f: Function<FrameReg>) {
    w.begin_function(f.get_name().to_string());
    // save return address
    w.mov(A, PL);
    w.mov(B, A);
    w.mov(A, PH);
    w.ldi_p_const(FrameReg::RetAddr.get_address_callee().unwrap(), true);
    w.st(B);
    w.inc(PL);
    w.st(A);
    // push stack frames
    w.ldi_p_const(stack::SP_INCDEC_ADDR, true);
    assert_eq!(
        stack::SP_INCDEC_INC0 & stack::SP_INCDEC_INC1,
        (stack::SP_INCDEC_ADDR >> 8) as u8
    );
    w.mov(A, PH);
    w.st(A);

    for (i, block) in f.get_body().iter().enumerate() {
        w.label(make_block_label(f.get_name(), i));
        for op in block.ops.iter() {
            w.comment(format!("{}", op));
            gen_op(w, op, f.get_name());
        }
        w.comment(format!("{}", block.tail));
        gen_tail(w, &block.tail, i, f.get_name());
    }
}

fn gen_op(w: &mut InstructionWriter, op: &generic_ir::Op<FrameReg>, function_name: &str) {
    use generic_ir::Op::*;
    match op {
        Undefined(_) => (),
        Arg(op) => check_arg(op),
        Copy(op) => copy::gen_copy(w, op),
        Bool(op) => boolean::gen_bool(w, op),
        BoolInv(op) => boolean::gen_bool_inv(w, op),
        Add(op) => add::gen_add(w, op),
        Sub(op) => sub::gen_sub(w, op),
        Mul(op) => todo!(),
        Div(op) => todo!(),
        Mod(op) => todo!(),
        BAnd(op) => bitwise::gen_bitwise_and(w, op),
        BOr(op) => bitwise::gen_bitwise_or(w, op),
        BXor(op) => bitwise::gen_bitwise_xor(w, op),
        LShift(op) => shift::gen_lshift(w, op),
        RShift(op) => shift::gen_rshift(w, op),
        Neg(op) => neg::gen_neg(w, op),
        Not(op) => bitwise::gen_bitwise_not(w, op),
        Compare(op) => todo!(),
        Conv(op) => conv::gen_conv(w, op),
        Store(op) => store::gen_store(w, op),
        Load(op) => load::gen_load(w, op),
        Call(op) => call::gen_call(w, op),
        Memcpy(op) => todo!(),
        FramePointer(_) => {
            unreachable!("Frame pointer expansion step must be performed before generating code")
        }
        #[cfg(test)]
        Dummy(_) => (),
    }
}

fn gen_tail(
    w: &mut InstructionWriter,
    tail: &generic_ir::Tail<FrameReg>,
    block_idx: usize,
    function_name: &str,
) {
    use generic_ir::Tail;
    match tail {
        Tail::Ret => gen_return(w),
        Tail::Jump(n) => gen_jump(w, block_idx, *n, function_name),
        Tail::Cond(c, n1, n2) => gen_cond_jump(w, c, block_idx, *n1, *n2, function_name),
        Tail::Switch(_, _, _, _) => todo!(),
    }
}

fn gen_cond_jump(
    w: &mut InstructionWriter,
    c: &Scalar<FrameReg>,
    cur_idx: usize,
    if_idx: usize,
    else_idx: usize,
    function_name: &str,
) {
    match c {
        Scalar::ConstInt(0) => gen_jump(w, cur_idx, else_idx, function_name),
        Scalar::ConstInt(_) => gen_jump(w, cur_idx, if_idx, function_name),
        Scalar::Var(v) => {
            gen_load_var_8(w, A, v);
            w.add(A, Zero);
            if if_idx == cur_idx + 1 {
                w.ldi_p_sym(make_block_label(function_name, else_idx), 0);
                w.jz();
            } else if else_idx == cur_idx + 1 {
                w.ldi_p_sym(make_block_label(function_name, if_idx), 0);
                w.jnz();
            } else {
                w.ldi_p_sym(make_block_label(function_name, if_idx), 0);
                w.jnz();
                w.ldi_p_sym(make_block_label(function_name, else_idx), 0);
                w.jmp();
            }
        }
        Scalar::SymbolOffset(_, _) => unimplemented!(),
    }
}

fn gen_jump(w: &mut InstructionWriter, cur_idx: usize, target_idx: usize, function_name: &str) {
    if target_idx != cur_idx + 1 {
        w.ldi_p_sym(make_block_label(function_name, target_idx), 0);
        w.jmp();
    }
}

fn gen_return(w: &mut InstructionWriter) {
    // pop stack frames
    w.ldi_p_const(stack::SP_INCDEC_ADDR, true);
    w.ldi_const(A, stack::SP_INCDEC_DEC0 & stack::SP_INCDEC_DEC1, true);
    w.st(A);

    // restore return address and jump
    w.ldi_p_const(FrameReg::RetAddr.get_address_callee().unwrap(), true);
    w.ld(A);
    w.inc(PL);
    w.ld(PH);
    w.mov(PL, A);
    w.jmp();
}

fn gen_load_var_8(w: &mut InstructionWriter, dst: Reg, v: &VarLocation<FrameReg>) {
    match v {
        VarLocation::Local(reg) => {
            w.ldi_p_const(reg.get_address(), true);
            w.ld(dst);
        }
        VarLocation::Global(name) => {
            w.ldi_p_sym(name.to_string(), 0);
            w.ld(dst);
        }
        VarLocation::Return => {
            w.ldi_p_sym(global::RET_VALUE_REG_SYMBOL.to_string(), 0);
            w.ld(dst);
        }
    }
}

fn make_block_label(function_name: &str, block_index: usize) -> String {
    format!("__{}_{}", function_name, block_index)
}

fn check_arg(op: &ArgOp<FrameReg>) {
    match op.dst_reg {
        FrameReg::FrameA(n) if n as usize == op.arg_number => (),
        _ => panic!(
            "Wrong argument register {} for arg {}",
            op.dst_reg, op.arg_number
        ),
    }
}
