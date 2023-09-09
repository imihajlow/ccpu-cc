use std::collections::HashSet;

use crate::{
    ccpu::{
        gen::{make_block_label, util::load_scalar},
        instr::InstructionWriter,
        reg::FrameReg,
    },
    generic_ir::{Scalar, Width},
};

pub fn gen_switch(
    w: &mut InstructionWriter,
    function_name: &str,
    cur_idx: usize,
    s: &Scalar<FrameReg>,
    width: Width,
    cases: &Vec<(u64, usize)>,
    default: usize,
) {
    if cases.is_empty() {
        if cur_idx + 1 != default {
            w.ldi_p_sym(make_block_label(function_name, default), 0);
            w.jmp();
        }
        return;
    }

    let cases = cases
        .iter()
        .map(|(val, idx)| (*val, make_block_label(function_name, *idx)))
        .collect();
    let offsets = (0..(width as usize)).collect();
    let default = make_block_label(function_name, default);
    gen_switch_inner(w, s, offsets, cases, default);
}

fn gen_switch_inner(
    w: &mut InstructionWriter,
    s: &Scalar<FrameReg>,
    offsets: Vec<usize>,
    cases: Vec<(u64, String)>,
    default: String,
) {
    assert!(!cases.is_empty());
    if offsets.len() == 1 {
        let offset = offsets[0];
        let cases_labels = cases
            .into_iter()
            .map(|(val, label)| ((val >> (offset * 8)) as u8, label))
            .collect();
        gen_switch_byte(w, s, offset as u16, &cases_labels, default, None);
    } else {
        // Count number of distinct values for each byte of input
        let mut sets = vec![HashSet::new(); offsets.len()];
        for (val, _) in &cases {
            for (i, offset) in offsets.iter().enumerate() {
                let b = (*val >> (offset * 8)) as u8;
                sets[i].insert(b);
            }
        }
        let counts = sets.iter().enumerate().map(|(i, s)| (i, s.len()));

        // Pick the offset with minimum number of distinct values
        let min_count_offset_idx = counts.min_by_key(|(_, c)| *c).unwrap().0;
        let min_count_offset = offsets[min_count_offset_idx];

        // Generate labels for each case
        let sub_cases: Vec<_> = sets[min_count_offset_idx]
            .iter()
            .map(|v| (*v, w.alloc_label()))
            .collect();

        let following_label = sub_cases.first().unwrap().1.clone();

        // Switch on selected byte
        gen_switch_byte(
            w,
            s,
            min_count_offset as u16,
            &sub_cases,
            default.clone(),
            Some(following_label),
        );
        let sub_offsets: Vec<_> = offsets
            .iter()
            .filter(|o| **o != min_count_offset)
            .cloned()
            .collect();

        for (val, label) in sub_cases.into_iter() {
            w.label(label);
            let mask = (0xff as u64) << (min_count_offset * 8);
            let val = (val as u64) << (min_count_offset * 8);
            let sub_cases = cases
                .iter()
                .filter(|(x, _)| *x & mask == val)
                .cloned()
                .collect();
            gen_switch_inner(w, s, sub_offsets.clone(), sub_cases, default.clone());
        }
    }
}

fn gen_switch_byte(
    w: &mut InstructionWriter,
    s: &Scalar<FrameReg>,
    offset: u16,
    cases: &Vec<(u8, String)>,
    default: String,
    following_label: Option<String>,
) {
    assert!(!cases.is_empty());

    /*let mut w_compare = w.clone();
    let mut w_table = w.clone();
    gen_switch_byte_compare(&mut w_compare, function_name, cur_idx, s, labels, default);
    gen_switch_byte_table(&mut w_table, function_name, cur_idx, s, labels, default);

    *w = if w_compare.get_ro_size() > w_table.get_ro_size() {
        w_table
    } else {
        w_compare
    };*/
    gen_switch_byte_compare(w, s, offset, cases, default, following_label);
}

fn gen_switch_byte_compare(
    w: &mut InstructionWriter,
    s: &Scalar<FrameReg>,
    offset: u16,
    cases: &Vec<(u8, String)>,
    default: String,
    following_label: Option<String>,
) {
    use crate::ccpu::instr::Reg::*;
    load_scalar(w, B, s, offset, true);

    let (base, last) = {
        let mut base = Vec::new();
        let mut last = None;
        for (val, label) in cases {
            match &following_label {
                Some(following) if following == label && last.is_none() => {
                    last = Some((*val, default.clone()))
                }
                _ => base.push((*val, label)),
            }
        }
        base.sort_by(|(v1, _), (v2, _)| v1.cmp(v2));
        (base, last)
    };

    for (val, label) in base {
        w.ldi_const(A, val as u8, true);
        w.sub(A, B);
        w.ldi_p_sym(label.to_string(), 0);
        w.jz();
    }

    if let Some((val, label)) = last {
        w.ldi_const(A, val as u8, true);
        w.sub(A, B);
        w.ldi_p_sym(label.to_string(), 0);
        w.jnz();
    } else {
        match following_label {
            Some(following) if default == following => (),
            _ => {
                w.ldi_p_sym(default, 0);
                w.jmp();
            }
        }
    }
}

/*fn gen_switch_byte_table(
    w: &mut InstructionWriter,
    function_name: &str,
    cur_idx: usize,
    s: &Scalar<FrameReg>,
    labels: &Vec<(u64, usize)>,
    default: usize,
) {
    use crate::ccpu::instr::Reg::*;
    let case_iter = labels.iter().map(|(x, _)| *x as u8);
    let min = case_iter.clone().min().unwrap();
    let max = case_iter.max().unwrap();
    let table_size = table_size(max - min) as usize;

    // let mut table = vec![default; table_size * 2];
    // for (val, idx) in labels {
    //     table[(*val as u8 - min) as usize] = *idx;
    // }
    // let table_label = w.alloc_label();
    // w.ro_data_vec(table_label, table, align)
    todo!()
} */

fn table_size(diff: u8) -> u8 {
    if (diff).is_power_of_two() {
        diff
    } else {
        // 5 -> 8
        // 00000101
        1 << (8 - diff.leading_zeros())
    }
}
