use crate::{
    ccpu::{
        gen::{make_block_label, util::load_scalar},
        instr::InstructionWriter,
        reg::FrameReg,
    },
    generic_ir::{
        self,
        CompareKind::{self, *},
        Scalar,
    },
};

pub fn gen_compare(w: &mut InstructionWriter, op: &generic_ir::CompareOp<FrameReg>) {
    match (op.desc.kind, op.desc.sign) {
        (Equal, _) => gen_compare_eq(w, op),
        (NotEqual, _) => gen_compare_ne(w, op),
        (_, false) => gen_compare_unsigned(w, op),
        (_, true) => gen_compare_signed(w, op),
    }
}

pub fn gen_compare_tail(
    w: &mut InstructionWriter,
    desc: &generic_ir::CompareDesc<FrameReg>,
    cur_block_id: usize,
    if_true: usize,
    if_false: usize,
    function_name: &str,
) {
    match (desc.kind, desc.sign) {
        (Equal, _) => gen_compare_tail_eq(w, desc, cur_block_id, if_true, if_false, function_name),
        (NotEqual, _) => {
            gen_compare_tail_eq(w, desc, cur_block_id, if_false, if_true, function_name)
        }
        (_, false) => {
            gen_compare_tail_unsigned(w, desc, cur_block_id, if_true, if_false, function_name)
        }
        (_, true) => {
            gen_compare_tail_signed(w, desc, cur_block_id, if_true, if_false, function_name)
        }
    }
}

fn gen_compare_eq(w: &mut InstructionWriter, op: &generic_ir::CompareOp<FrameReg>) {
    assert_eq!(op.desc.kind, Equal);
    use crate::ccpu::instr::Reg::*;

    // Let constant be loaded into A to give `mov a, 0` a chance
    let (lhs, rhs) = if let Scalar::ConstInt(_) = &op.desc.rhs {
        (&op.desc.rhs, &op.desc.lhs)
    } else {
        (&op.desc.lhs, &op.desc.rhs)
    };

    let label_ne = w.alloc_label();
    let label_end = w.alloc_label();
    for o in 0..(op.desc.width as u16) {
        load_scalar(w, A, lhs, o, true);
        load_scalar(w, B, rhs, o, true);
        w.sub(A, B);
        w.ldi_p_sym(label_ne.clone(), 0);
        w.jnz();
    }
    w.inc(A);
    w.ldi_p_sym(label_end.clone(), 0);
    w.jmp();
    w.label(label_ne);
    w.mov(A, Zero);
    w.label(label_end);
    w.ldi_p_var_location(&op.dst, 0, true);
    w.st(A);
    if op.desc.width as u16 > 1 {
        w.mov(A, Zero);
        for _ in 1..(op.desc.width as u16) {
            w.inc(PL);
            w.st(A);
        }
    }
}

fn gen_compare_tail_eq(
    w: &mut InstructionWriter,
    desc: &generic_ir::CompareDesc<FrameReg>,
    cur_block_id: usize,
    if_true: usize,
    if_false: usize,
    function_name: &str,
) {
    assert!(desc.kind == Equal || desc.kind == NotEqual);
    use crate::ccpu::instr::Reg::*;

    let label_eq = make_block_label(function_name, if_true);
    let label_ne = make_block_label(function_name, if_false);

    for o in 0..(desc.width as u16) {
        load_scalar(w, A, &desc.lhs, o, true);
        load_scalar(w, B, &desc.rhs, o, true);
        w.sub(A, B);
        if cur_block_id + 1 == if_false && o == (desc.width as u16) - 1 {
            w.ldi_p_sym(label_eq.clone(), 0);
            w.jz();
        } else {
            w.ldi_p_sym(label_ne.clone(), 0);
            w.jnz();
        }
    }
    if cur_block_id + 1 != if_true && cur_block_id + 1 != if_false {
        w.ldi_p_sym(label_eq, 0);
        w.jmp();
    }
}

fn gen_compare_ne(w: &mut InstructionWriter, op: &generic_ir::CompareOp<FrameReg>) {
    assert_eq!(op.desc.kind, NotEqual);
    use crate::ccpu::instr::Reg::*;

    // Let constant be loaded into A to give `mov a, 0` a chance
    let (lhs, rhs) = if let Scalar::ConstInt(_) = &op.desc.rhs {
        (&op.desc.rhs, &op.desc.lhs)
    } else {
        (&op.desc.lhs, &op.desc.rhs)
    };

    let label_ne = w.alloc_label();
    let label_end = w.alloc_label();
    for offset in 0..(op.desc.width as u16) {
        load_scalar(w, A, lhs, offset, true);
        load_scalar(w, B, rhs, offset, true);
        w.sub(A, B);
        if offset != (op.desc.width as u16) - 1 {
            w.ldi_p_sym(label_ne.clone(), 0);
            w.jnz();
        } else {
            w.ldi_p_sym(label_end.clone(), 0);
            w.jz();
        }
    }
    w.label(label_ne);
    w.ldi_const(A, 1, true);
    w.label(label_end);
    w.ldi_p_var_location(&op.dst, 0, true);
    w.st(A);
    if op.desc.width as u16 > 1 {
        w.mov(A, Zero);
        for _ in 1..(op.desc.width as u16) {
            w.inc(PL);
            w.st(A);
        }
    }
}

/*
    unsigned:
        lt: S           easy
        le: S | eq
        ge: NS          easy
        gt: NS & neq
*/
fn gen_compare_unsigned(w: &mut InstructionWriter, op: &generic_ir::CompareOp<FrameReg>) {
    assert_eq!(op.desc.sign, false);
    use crate::ccpu::instr::Reg::*;

    let (lhs, rhs, kind) = match op.desc.kind {
        CompareKind::LessThan | CompareKind::GreaterOrEqual => {
            (&op.desc.lhs, &op.desc.rhs, op.desc.kind)
        }
        CompareKind::LessOrEqual | CompareKind::GreaterThan => {
            (&op.desc.rhs, &op.desc.lhs, op.desc.kind.flip())
        }
        _ => unreachable!(),
    };

    for offset in 0..(op.desc.width as u16) {
        load_scalar(w, A, lhs, offset, offset == 0);
        load_scalar(w, B, rhs, offset, offset == 0);
        if offset == 0 {
            w.sub(A, B);
        } else {
            w.sbb(A, B);
        }
    }
    let label = w.alloc_label();
    w.mov(A, Zero);
    w.ldi_p_sym(label.clone(), 0);
    match kind {
        CompareKind::LessThan => w.jnc(),
        CompareKind::GreaterOrEqual => w.jc(),
        _ => unreachable!(),
    };
    w.inc(A);
    w.label(label);
    w.ldi_p_var_location(&op.dst, 0, true);
    w.st(A);
    if op.desc.width as u16 > 1 {
        w.mov(A, Zero);
        for _ in 1..(op.desc.width as u16) {
            w.inc(PL);
            w.st(A);
        }
    }
}

fn gen_compare_tail_unsigned(
    w: &mut InstructionWriter,
    desc: &generic_ir::CompareDesc<FrameReg>,
    cur_block_id: usize,
    if_true: usize,
    if_false: usize,
    function_name: &str,
) {
    assert_eq!(desc.sign, false);
    use crate::ccpu::instr::Reg::*;

    let (lhs, rhs, kind) = match desc.kind {
        CompareKind::LessThan | CompareKind::GreaterOrEqual => (&desc.lhs, &desc.rhs, desc.kind),
        CompareKind::LessOrEqual | CompareKind::GreaterThan => {
            (&desc.rhs, &desc.lhs, desc.kind.flip())
        }
        _ => unreachable!(),
    };

    // subtract lhs - rhs
    for offset in 0..(desc.width as u16) {
        load_scalar(w, A, lhs, offset, offset == 0);
        load_scalar(w, B, rhs, offset, offset == 0);
        if offset == 0 {
            w.sub(A, B);
        } else {
            w.sbb(A, B);
        }
    }
    let label_true = make_block_label(function_name, if_true);
    let label_false = make_block_label(function_name, if_false);

    // lhs < rhs    C is set
    // lhs >= rhs   C is not set
    match kind {
        CompareKind::LessThan => {
            // on C go to label_true
            if cur_block_id + 1 == if_true {
                w.ldi_p_sym(label_false, 0);
                w.jnc();
            } else {
                w.ldi_p_sym(label_true, 0);
                w.jc();
                if cur_block_id + 1 != if_false {
                    w.ldi_p_sym(label_false, 0);
                    w.jmp();
                }
            }
        }
        CompareKind::GreaterOrEqual => {
            // on C go to label_false
            if cur_block_id + 1 == if_true {
                w.ldi_p_sym(label_false, 0);
                w.jc();
            } else {
                w.ldi_p_sym(label_true, 0);
                w.jnc();
                if cur_block_id + 1 != if_false {
                    w.ldi_p_sym(label_false, 0);
                    w.jmp();
                }
            }
        }
        _ => unreachable!(),
    };
}

/*
    signed:
        lt: S != O        easy
        le: eq | S != O
        ge: S == O        easy
        gt: neq & S == O
*/
fn gen_compare_signed(w: &mut InstructionWriter, op: &generic_ir::CompareOp<FrameReg>) {
    assert_eq!(op.desc.sign, true);
    use crate::ccpu::instr::Reg::*;

    let (lhs, rhs, kind) = match op.desc.kind {
        CompareKind::LessThan | CompareKind::GreaterOrEqual => {
            (&op.desc.lhs, &op.desc.rhs, op.desc.kind)
        }
        CompareKind::LessOrEqual | CompareKind::GreaterThan => {
            (&op.desc.rhs, &op.desc.lhs, op.desc.kind.flip())
        }
        _ => unreachable!(),
    };

    for offset in 0..(op.desc.width as u16) {
        load_scalar(w, A, lhs, offset, offset == 0);
        load_scalar(w, B, rhs, offset, offset == 0);
        if offset == 0 {
            w.sub(A, B);
        } else {
            w.sbb(A, B);
        }
    }

    /*
        case for lt:

        mov a, 0
        jno  label_no
        js  label_end
        inc  a
        jmp  label_end
    label_no:
        jns   label_end
        inc a
    label_end:
    */
    {
        let label_no = w.alloc_label();
        let label_end = w.alloc_label();
        w.mov(A, Zero);
        w.ldi_p_sym(label_no.clone(), 0);
        w.jno();
        w.ldi_p_sym(label_end.clone(), 0);
        match kind {
            CompareKind::LessThan => w.js(),
            CompareKind::GreaterOrEqual => w.jns(),
            _ => unreachable!(),
        };
        w.inc(A);
        w.ldi_p_sym(label_end.clone(), 0);
        w.jmp();
        w.label(label_no);
        w.ldi_p_sym(label_end.clone(), 0);
        match kind {
            CompareKind::LessThan => w.jns(),
            CompareKind::GreaterOrEqual => w.js(),
            _ => unreachable!(),
        };
        w.inc(A);
        w.label(label_end);
    }

    w.ldi_p_var_location(&op.dst, 0, true);
    w.st(A);
    if op.desc.width as u16 > 1 {
        w.mov(A, Zero);
        for _ in 1..(op.desc.width as u16) {
            w.inc(PL);
            w.st(A);
        }
    }
}

fn gen_compare_tail_signed(
    w: &mut InstructionWriter,
    desc: &generic_ir::CompareDesc<FrameReg>,
    cur_block_id: usize,
    if_true: usize,
    if_false: usize,
    function_name: &str,
) {
    assert_eq!(desc.sign, true);
    use crate::ccpu::instr::Reg::*;

    let (lhs, rhs, kind) = match desc.kind {
        CompareKind::LessThan | CompareKind::GreaterOrEqual => (&desc.lhs, &desc.rhs, desc.kind),
        CompareKind::LessOrEqual | CompareKind::GreaterThan => {
            (&desc.rhs, &desc.lhs, desc.kind.flip())
        }
        _ => unreachable!(),
    };

    // subtract lhs - rhs
    for offset in 0..(desc.width as u16) {
        load_scalar(w, A, lhs, offset, offset == 0);
        load_scalar(w, B, rhs, offset, offset == 0);
        if offset == 0 {
            w.sub(A, B);
        } else {
            w.sbb(A, B);
        }
    }

    /*
        case for lt (S != O):

        jno  label_no
        js   if_false
        jmp  if_true
    label_no:
        jns  if_false
    */
    let label_no = w.alloc_label();
    let label_true = make_block_label(function_name, if_true);
    let label_false = make_block_label(function_name, if_false);
    w.ldi_p_sym(label_no.clone(), 0);
    w.jno();
    w.ldi_p_sym(label_false.clone(), 0);
    match kind {
        CompareKind::LessThan => w.js(),
        CompareKind::GreaterOrEqual => w.jns(),
        _ => unreachable!(),
    }
    w.ldi_p_sym(label_true.clone(), 0);
    w.jmp();
    w.label(label_no);
    if cur_block_id + 1 == if_true {
        // next block is if_true
        w.ldi_p_sym(label_false, 0);
        match kind {
            CompareKind::LessThan => w.jns(),
            CompareKind::GreaterOrEqual => w.js(),
            _ => unreachable!(),
        }
    } else {
        // next block is not if_true
        w.ldi_p_sym(label_true, 0);
        match kind {
            CompareKind::LessThan => w.js(),
            CompareKind::GreaterOrEqual => w.jns(),
            _ => unreachable!(),
        }
        if cur_block_id + 1 != if_false {
            w.ldi_p_sym(label_false, 0);
            w.jmp();
        }
    }
}
