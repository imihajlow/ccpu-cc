use crate::{
    ccpu::{gen::util::load_scalar, instr::InstructionWriter, reg::FrameReg},
    generic_ir::{
        self,
        CompareKind::{self, *},
        Scalar,
    },
};

pub fn gen_compare(w: &mut InstructionWriter, op: &generic_ir::CompareOp<FrameReg>) {
    match (op.kind, op.sign) {
        (Equal, _) => gen_compare_eq(w, op),
        (NotEqual, _) => gen_compare_ne(w, op),
        (_, false) => gen_compare_unsigned(w, op),
        (_, true) => gen_compare_signed(w, op),
    }
}

fn gen_compare_eq(w: &mut InstructionWriter, op: &generic_ir::CompareOp<FrameReg>) {
    assert_eq!(op.kind, Equal);
    use crate::ccpu::instr::Reg::*;

    // Let constant be loaded into A to give `mov a, 0` a chance
    let (lhs, rhs) = if let Scalar::ConstInt(_) = &op.rhs {
        (&op.rhs, &op.lhs)
    } else {
        (&op.lhs, &op.rhs)
    };

    let label_ne = w.alloc_label();
    let label_end = w.alloc_label();
    for o in 0..(op.width as u16) {
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
    if op.width as u16 > 1 {
        w.mov(A, Zero);
        for _ in 1..(op.width as u16) {
            w.inc(PL);
            w.st(A);
        }
    }
}

fn gen_compare_ne(w: &mut InstructionWriter, op: &generic_ir::CompareOp<FrameReg>) {
    assert_eq!(op.kind, NotEqual);
    use crate::ccpu::instr::Reg::*;

    // Let constant be loaded into A to give `mov a, 0` a chance
    let (lhs, rhs) = if let Scalar::ConstInt(_) = &op.rhs {
        (&op.rhs, &op.lhs)
    } else {
        (&op.lhs, &op.rhs)
    };

    let label_ne = w.alloc_label();
    let label_end = w.alloc_label();
    for offset in 0..(op.width as u16) {
        load_scalar(w, A, lhs, offset, true);
        load_scalar(w, B, rhs, offset, true);
        w.sub(A, B);
        if offset != (op.width as u16) - 1 {
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
    if op.width as u16 > 1 {
        w.mov(A, Zero);
        for _ in 1..(op.width as u16) {
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
    assert_eq!(op.sign, false);
    use crate::ccpu::instr::Reg::*;

    let (lhs, rhs, kind) = match op.kind {
        CompareKind::LessThan | CompareKind::GreaterOrEqual => (&op.lhs, &op.rhs, op.kind),
        CompareKind::LessOrEqual | CompareKind::GreaterThan => (&op.rhs, &op.lhs, op.kind.flip()),
        _ => unreachable!(),
    };

    for offset in 0..(op.width as u16) {
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
        CompareKind::LessThan => w.jns(),
        CompareKind::GreaterOrEqual => w.js(),
        _ => unreachable!(),
    };
    w.inc(A);
    w.label(label);
    w.ldi_p_var_location(&op.dst, 0, true);
    w.st(A);
    if op.width as u16 > 1 {
        w.mov(A, Zero);
        for _ in 1..(op.width as u16) {
            w.inc(PL);
            w.st(A);
        }
    }
}

/*
    signed:
        lt: S != O        easy
        le: eq | S != O
        ge: S == O        easy
        gt: neq & S == O
*/
fn gen_compare_signed(w: &mut InstructionWriter, op: &generic_ir::CompareOp<FrameReg>) {
    assert_eq!(op.sign, true);
    use crate::ccpu::instr::Reg::*;

    let (lhs, rhs, kind) = match op.kind {
        CompareKind::LessThan | CompareKind::GreaterOrEqual => (&op.lhs, &op.rhs, op.kind),
        CompareKind::LessOrEqual | CompareKind::GreaterThan => (&op.rhs, &op.lhs, op.kind.flip()),
        _ => unreachable!(),
    };

    for offset in 0..(op.width as u16) {
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
    if op.width as u16 > 1 {
        w.mov(A, Zero);
        for _ in 1..(op.width as u16) {
            w.inc(PL);
            w.st(A);
        }
    }
}
