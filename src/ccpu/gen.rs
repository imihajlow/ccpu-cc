use crate::{
    function::Function,
    generic_ir::{self, Scalar, VarLocation},
    opt::blocks,
    translation_unit::TranslationUnit,
};

use super::{
    instr::{InstructionWriter, Reg, Reg::*},
    reg::FrameReg,
    stack,
};

pub fn gen_tu(tu: TranslationUnit<FrameReg>) -> InstructionWriter {
    let mut w = InstructionWriter::new();
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
    w.ldi_p_const(FrameReg::RetAddr.get_address_callee().unwrap());
    w.st(B);
    w.inc(PL);
    w.st(A);
    // push stack frames
    w.ldi_p_const(stack::SP_INCDEC_ADDR);
    assert_eq!(
        stack::SP_INCDEC_INC0 & stack::SP_INCDEC_INC1,
        (stack::SP_INCDEC_ADDR >> 8) as u8
    );
    w.mov(A, PH);
    w.st(A);

    for (i, block) in f.get_body().iter().enumerate() {
        w.label(make_block_label(f.get_name(), i));
        gen_tail(w, &block.tail, i, f.get_name());
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
        Scalar::ConstInt(_) | Scalar::FramePointer => gen_jump(w, cur_idx, if_idx, function_name),
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
    w.ldi_p_const(stack::SP_INCDEC_ADDR);
    w.ldi_const(A, stack::SP_INCDEC_DEC0 & stack::SP_INCDEC_DEC1);
    w.st(A);

    // restore return address and jump
    w.ldi_p_const(FrameReg::RetAddr.get_address_callee().unwrap());
    w.ld(A);
    w.inc(PL);
    w.ld(PH);
    w.mov(PL, A);
    w.jmp();
}

fn gen_load_var_8(w: &mut InstructionWriter, dst: Reg, v: &VarLocation<FrameReg>) {
    match v {
        VarLocation::Local(reg) => {
            w.ldi_p_const(reg.get_address());
            w.ld(dst);
        }
        VarLocation::Global(name) => {
            w.ldi_p_sym(name.to_string(), 0);
            w.ld(dst);
        }
        VarLocation::Return => {
            todo!()
        }
        VarLocation::Frame(offset) => todo!(),
    }
}

fn make_block_label(function_name: &str, block_index: usize) -> String {
    format!("__{}_{}", function_name, block_index)
}
