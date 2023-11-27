use lang_c::span::Span;

use crate::ccpu::gen::switch::gen_switch;
use crate::ccpu::global::{get_global_var_label, get_static_frame_symbol};
use crate::error::{CompileError, ErrorCollector};
use crate::generic_ir::{GlobalVarId, JumpCondition, Width};
use crate::initializer::{Constant, TypedConstant};
use crate::name_scope::{ExportClass, NameScope};
use crate::{
    function::Function,
    generic_ir::{self, ArgOp, Scalar, VarLocation},
    translation_unit::TranslationUnit,
};

use self::compare::gen_compare_tail;

use super::{
    global,
    instr::{InstructionWriter, Reg, Reg::*},
    reg::FrameReg,
    stack,
};

mod add;
mod bitwise;
mod boolean;
mod bswap;
mod call;
mod compare;
mod conv;
mod copy;
mod intrin;
mod load;
mod memcpy;
mod neg;
mod shift;
mod store;
mod sub;
mod switch;
mod util;
mod variadic;

pub fn gen_tu(
    tu: TranslationUnit<FrameReg>,
    ec: &mut ErrorCollector,
) -> Result<InstructionWriter, ()> {
    let mut w = InstructionWriter::new();
    w.import(global::RET_VALUE_REG_SYMBOL.to_string());
    intrin::gen_intrin_imports(&mut w);

    for sym in tu.scope.get_import_symbols() {
        w.import(get_global_var_label(&sym));
    }

    for (sym, class) in tu.scope.get_export_symbols() {
        match class {
            ExportClass::Normal => w.export(get_global_var_label(&sym)),
            ExportClass::Weak => w.export_weak(get_global_var_label(&sym)),
        }
    }

    for f in tu.functions.into_iter() {
        gen_function(&mut w, f, ec)?;
    }

    for (id, (val, span)) in &tu.scope.static_initializers {
        gen_static_data(&mut w, id, val, *span, &tu.scope, ec)?;
    }

    for (idx, data) in tu.scope.literals.iter().enumerate() {
        gen_ro_data(&mut w, idx, data, 1);
    }
    Ok(w)
}

fn gen_function(
    w: &mut InstructionWriter,
    f: Function<FrameReg>,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    let name = f.get_id();
    w.begin_function(&name);
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

    // allocate static frame
    let frame_size = check_16bit(f.get_frame_size(), f.get_span(), ec)?;
    if frame_size != 0 {
        let id = get_static_frame_symbol(f.get_name());
        let label = get_global_var_label(&id);
        w.bss(label, frame_size, 8);
    }
    Ok(())
}

fn gen_op(w: &mut InstructionWriter, op: &generic_ir::Op<FrameReg>, _function_name: &str) {
    use generic_ir::Op::*;
    match op {
        Undefined(_) => (),
        Arg(op) => check_arg(op),
        Copy(op) => copy::gen_copy(w, op),
        Bool(op) => boolean::gen_bool(w, op),
        BoolInv(op) => boolean::gen_bool_inv(w, op),
        Add(op) => add::gen_add(w, op),
        Sub(op) => sub::gen_sub(w, op),
        Mul(_) => unreachable!("multiplication must be replaced by an intrinsic call"),
        Div(_) => unreachable!("division must be replaced by an intrinsic call"),
        Mod(_) => unreachable!("division must be replaced by an intrinsic call"),
        BAnd(op) => bitwise::gen_bitwise_and(w, op),
        BOr(op) => bitwise::gen_bitwise_or(w, op),
        BXor(op) => bitwise::gen_bitwise_xor(w, op),
        LShift(op) => shift::gen_lshift(w, op),
        RShift(op) => shift::gen_rshift(w, op),
        Neg(op) => neg::gen_neg(w, op),
        Not(op) => bitwise::gen_bitwise_not(w, op),
        Compare(op) => compare::gen_compare(w, op),
        Conv(op) => conv::gen_conv(w, op),
        Store(op) => store::gen_store(w, op),
        Load(op) => load::gen_load(w, op),
        Call(op) => call::gen_call(w, op),
        Memcpy(op) => memcpy::gen_memcpy(w, op),
        IntrinCall(op) => intrin::gen_intrin_call(w, op),
        VaStart(op) => variadic::gen_va_start(w, op),
        VaArg(op) => variadic::gen_va_arg(w, op),
        VaListInc(op) => variadic::gen_va_list_inc(w, op),
        ByteSwap(op) => bswap::gen_bswap(w, op),
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
        Tail::Switch(s, width, cases, default) => {
            gen_switch(w, function_name, block_idx, s, *width, cases, *default)
        }
    }
}

fn gen_cond_jump(
    w: &mut InstructionWriter,
    c: &JumpCondition<FrameReg>,
    cur_idx: usize,
    if_idx: usize,
    else_idx: usize,
    function_name: &str,
) {
    match c {
        JumpCondition::RelaxedBool(Scalar::ConstInt(0), _)
        | JumpCondition::StrictBool(Scalar::ConstInt(0)) => {
            gen_jump(w, cur_idx, else_idx, function_name)
        }
        JumpCondition::RelaxedBool(Scalar::ConstInt(_), _)
        | JumpCondition::StrictBool(Scalar::ConstInt(_)) => {
            gen_jump(w, cur_idx, if_idx, function_name)
        }
        JumpCondition::StrictBool(Scalar::Var(v)) => {
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
        JumpCondition::RelaxedBool(Scalar::Var(v), width) => {
            w.ldi_p_var_location(v, 0, true);
            w.ld(A);
            if *width == Width::Byte {
                w.add(A, Zero);
            } else {
                for _ in 1..(*width as u16) {
                    w.inc(PL);
                    w.ld(B);
                    w.or(A, B);
                }
            }
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
        JumpCondition::Compare(desc) => {
            gen_compare_tail(w, desc, cur_idx, if_idx, else_idx, function_name)
        }
        JumpCondition::StrictBool(Scalar::SymbolOffset(_, _)) => unimplemented!(),
        JumpCondition::RelaxedBool(Scalar::SymbolOffset(_, _), _) => unimplemented!(),
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

fn gen_static_data(
    w: &mut InstructionWriter,
    id: &GlobalVarId,
    val: &TypedConstant,
    span: Span,
    scope: &NameScope,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    let label = get_global_var_label(id);
    let size = check_16bit(val.t.t.sizeof(scope, span, ec)?, span, ec)?;
    let align = check_16bit(val.t.t.alignof(scope, span, ec)?, span, ec)?;
    if val.is_bss() {
        w.bss(label, size, align as usize);
    } else {
        match val.val {
            Constant::Int(x) => {
                let width = Width::new(size as u8);
                w.data_int(label, x as u64, width, align as usize, val.is_const());
            }
            Constant::Array(_, _) | Constant::Struct(_) => {
                let bytes = constant_to_bytes(&val, span, scope, ec)?;
                w.data_vec(label, bytes, align as usize, val.is_const());
            }
            Constant::Void | Constant::Zero => {
                // is_bss
                unreachable!()
            }
        }
    }
    Ok(())
}

fn constant_to_bytes(
    val: &TypedConstant,
    span: Span,
    scope: &NameScope,
    ec: &mut ErrorCollector,
) -> Result<Vec<u8>, ()> {
    match &val.val {
        Constant::Int(x) => {
            let size = check_16bit(val.t.t.sizeof(scope, span, ec)?, span, ec)? as usize;
            let bytes = x.to_le_bytes()[0..size].to_vec();
            Ok(bytes)
        }
        Constant::Zero => {
            let size = check_16bit(val.t.t.sizeof(scope, span, ec)?, span, ec)? as usize;
            Ok(vec![0; size])
        }
        Constant::Void => {
            assert!(val.t.t.is_void());
            Ok(Vec::new())
        }
        Constant::Array(el_type, els) => {
            let mut r = Vec::new();
            for el in els {
                let mut el_bytes = constant_to_bytes(
                    &TypedConstant {
                        t: el_type.clone(),
                        val: el.clone(),
                    },
                    span,
                    scope,
                    ec,
                )?;
                r.append(&mut el_bytes);
            }
            Ok(r)
        }
        Constant::Struct(initializers) => {
            let size = check_16bit(val.t.t.sizeof(scope, span, ec)?, span, ec)? as usize;
            let mut data = vec![0; size];
            for initializer in initializers {
                let sub_data = constant_to_bytes(&initializer.value, initializer.span, scope, ec)?;
                assert!(initializer.offset as usize + sub_data.len() <= data.len());
                let dst = &mut data
                    [(initializer.offset as usize)..(initializer.offset as usize + sub_data.len())];
                dst.copy_from_slice(&sub_data);
            }
            Ok(data)
        }
    }
}

fn gen_ro_data(w: &mut InstructionWriter, idx: usize, buf: &Vec<u8>, align: usize) {
    let id = GlobalVarId::Literal(idx);
    let label = get_global_var_label(&id);
    w.data_vec(label, buf.clone(), align, true);
}

fn check_16bit(val: u32, span: Span, ec: &mut ErrorCollector) -> Result<u16, ()> {
    let v = val.try_into();
    match v {
        Ok(v) => Ok(v),
        Err(_) => {
            ec.record_error(CompileError::ObjectTooLarge(val as usize), span)?;
            unreachable!();
        }
    }
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

pub fn make_block_label(function_name: &str, block_index: usize) -> String {
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
