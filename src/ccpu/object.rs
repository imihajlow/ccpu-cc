use serde::Serialize;
use serde::Serializer;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

use crate::generic_ir::Width;

use super::instr;
use super::instr::ArithmOp;
use super::instr::Cond;
use super::instr::DataItem;
use super::instr::DataValue;
use super::instr::Imm;
use super::instr::InstructionWriter;
use super::instr::Op;
use super::instr::Reg;
use super::instr::TextItem;

#[derive(Debug, Serialize)]
pub struct Object {
    #[serde(rename = "globalSymbols")]
    global_symbols: Vec<String>,
    #[serde(rename = "exportSymbols")]
    export_symbols: Vec<String>,
    #[serde(rename = "weakSymbols")]
    weak_symbols: Vec<String>,
    consts: HashMap<String, u16>,
    sections: Vec<Section>,
}

#[derive(Debug, Serialize)]
struct Section {
    segment: String,
    name: String,
    align: u16,
    text: Vec<u8>,
    labels: HashMap<String, u16>,
    refs: Vec<(u16, Reference)>,
    lines: HashMap<u16, usize>,
}

#[derive(Debug)]
enum Reference {
    Lo(String, u16),
    Hi(String, u16),
}

impl Object {
    pub fn new(w: &InstructionWriter) -> Self {
        let exports = w.exports.iter().cloned().collect();
        let globals = w.imports.iter().cloned().collect();
        let weaks = w.weaks.iter().cloned().collect();

        let mut sections = Vec::new();
        for (id, section, text) in &w.text {
            sections.push(Section::new_text(id, section, text));
        }

        for (id, item) in &w.bss {
            sections.push(Section::new_bss(id, item));
        }

        for (id, item) in &w.data {
            sections.push(Section::new_data(id, item));
        }

        Self {
            global_symbols: globals,
            export_symbols: exports,
            weak_symbols: weaks,
            consts: HashMap::new(),
            sections,
        }
    }

    pub fn write_to_file(&self, f: &mut File) -> std::io::Result<()> {
        let json = serde_json::to_string(&self)?;
        write!(f, "{}", json)
    }
}

impl Section {
    fn new_text(fn_id: &String, section: &instr::Section, instrs: &[TextItem]) -> Self {
        let segment = format!("{}", section);
        let name = format!("{}.{}", section, fn_id);
        let mut labels = HashMap::new();
        let mut refs = Vec::new();
        let mut text = Vec::new();
        for ti in instrs {
            match ti {
                TextItem::Op(op) => {
                    encode_instruction(op, &mut text, &mut refs);
                }
                TextItem::Label(l) => {
                    labels.insert(l.to_string(), text.len().try_into().unwrap());
                }
                TextItem::Comment(_) => (),
            };
        }
        Self {
            segment,
            name,
            text,
            refs,
            labels,
            align: 0,
            lines: HashMap::new(),
        }
    }

    fn new_bss(id: &String, item: &DataItem<u16>) -> Self {
        let segment = format!("{}", item.section);
        let name = format!("{}.{}", item.section, id);
        let mut labels = HashMap::new();
        labels.insert(id.to_string(), 0);
        let text = vec![0; item.data as usize];

        Self {
            segment,
            name,
            text,
            refs: Vec::new(),
            labels,
            align: item.align as u16,
            lines: HashMap::new(),
        }
    }

    fn new_data(id: &String, item: &DataItem<DataValue>) -> Self {
        let segment = format!("{}", item.section);
        let name = format!("{}.{}", item.section, id);
        let mut labels = HashMap::new();
        labels.insert(id.to_string(), 0);
        let mut text = Vec::new();

        match &item.data {
            DataValue::Int(x, Width::Byte) => text.push(*x as u8),
            DataValue::Int(x, Width::Word) => {
                let val = *x as u16;
                text.extend_from_slice(&val.to_le_bytes());
            }
            DataValue::Int(x, Width::Dword) => {
                let val = *x as u32;
                text.extend_from_slice(&val.to_le_bytes());
            }
            DataValue::Int(x, Width::Qword) => {
                let val = *x as u64;
                text.extend_from_slice(&val.to_le_bytes());
            }
            DataValue::String(v) => {
                text.extend_from_slice(&v);
            }
        }

        Self {
            segment,
            name,
            text,
            refs: Vec::new(),
            labels,
            align: item.align as u16,
            lines: HashMap::new(),
        }
    }
}

fn encode_instruction(op: &Op, text: &mut Vec<u8>, refs: &mut Vec<(u16, Reference)>) {
    match op {
        Op::Arithm(op, reg, inv) => encode_arithm(*op, *reg, *inv, text),
        Op::Ldi(reg, imm) => encode_ldi(*reg, imm, text, refs),
        Op::Ld(reg) => encode_ld(*reg, text),
        Op::St(reg) => encode_st(*reg, text),
        Op::Jmp => text.push(0xe8),
        Op::Nop => text.push(0xff),
        Op::Jc(cond) => encode_jc(*cond, text),
    }
}

fn encode_arithm(op: ArithmOp, reg: Reg, invert: bool, text: &mut Vec<u8>) {
    use ArithmOp::*;
    let opcode = match op {
        ADD => 9,
        SUB => 13,
        ADC => 10,
        SBB => 14,
        INC => 11,
        DEC => 15,
        SHL => 3,
        NEG => 12,
        MOV => 8,
        NOT => 4,
        EXP => 0,
        AND => 1,
        OR => 2,
        XOR => 5,
        SHR => 6,
        SAR => 7,
    };
    let src = get_reg_index(reg);
    let invert = if invert { 4 } else { 0 };
    text.push((opcode << 3) | invert | src);
}

fn encode_ldi(reg: Reg, imm: &Imm, text: &mut Vec<u8>, refs: &mut Vec<(u16, Reference)>) {
    text.push(0xc0 | get_reg_index(reg));

    let offset = text.len() as u16;
    match imm {
        Imm::Const(x) => text.push(*x),
        Imm::Lo(s, o) => {
            refs.push((offset, Reference::Lo(s.to_string(), *o)));
            text.push(0);
        }
        Imm::Hi(s, o) => {
            refs.push((offset, Reference::Hi(s.to_string(), *o)));
            text.push(0);
        }
    }
}

fn encode_ld(reg: Reg, text: &mut Vec<u8>) {
    text.push(0x80 | get_reg_index(reg));
}

fn encode_st(reg: Reg, text: &mut Vec<u8>) {
    assert!(reg == Reg::A || reg == Reg::B);
    text.push(0xa0 | get_reg_index(reg));
}

fn encode_jc(c: Cond, text: &mut Vec<u8>) {
    use Cond::*;
    let cond = match c {
        C => 1,
        NC => 5,
        Z => 0,
        NZ => 4,
        O => 3,
        NO => 7,
        S => 2,
        NS => 6,
    };
    text.push(0xe0 | cond);
}

fn get_reg_index(reg: Reg) -> u8 {
    match reg {
        Reg::A => 0,
        Reg::B => 1,
        Reg::PL => 2,
        Reg::PH => 3,
        Reg::Zero => 0,
    }
}

impl Serialize for Reference {
    fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Reference::Lo(sym, 0) => ser.serialize_str(&format!("lo({})", sym)),
            Reference::Lo(sym, x) => ser.serialize_str(&format!("lo({}+{})", sym, x)),
            Reference::Hi(sym, 0) => ser.serialize_str(&format!("hi({})", sym)),
            Reference::Hi(sym, x) => ser.serialize_str(&format!("hi({}+{})", sym, x)),
        }
    }
}
