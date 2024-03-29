use std::collections::{HashMap, HashSet};
use std::fmt::Formatter;

use crate::generic_ir::{GlobalVarId, VarLocation, Width};

use super::global::{self, get_global_var_label, TRANSLATION_UNIT_UID};
use super::reg::FrameReg;

#[derive(Clone)]
pub struct InstructionWriter {
    pub(super) exports: HashSet<String>,
    pub(super) weaks: HashSet<String>,
    pub(super) imports: HashSet<String>,
    pub(super) text: Vec<(String, Section, Vec<TextItem>)>,
    pub(super) bss: HashMap<String, DataItem<u16>>,
    pub(super) data: HashMap<String, DataItem<DataValue>>,
    last: [Option<u8>; 5],
    next_label: usize,
    text_bytes: usize,
}

#[derive(Debug, Clone)]
pub(super) enum TextItem {
    Op(Op),
    Label(String),
    Comment(String),
}

#[derive(Debug, Clone)]
pub(super) struct DataItem<Inner> {
    pub(super) data: Inner,
    pub(super) align: usize,
    pub(super) section: Section,
}

#[derive(Debug, Clone)]
pub(super) enum DataValue {
    Int(u64, Width),
    String(Vec<u8>),
}

#[derive(Debug, Clone)]
pub(super) enum Section {
    Text,
    Data,
    Rodata,
    Bss,
    Custom(String),
}

#[derive(Debug, Clone)]
pub(super) enum Op {
    Arithm(ArithmOp, Reg, bool),
    Ldi(Reg, Imm),
    Ld(Reg),
    St(Reg),
    Jmp,
    Nop,
    Jc(Cond),
}

#[derive(Debug, Clone)]
pub(super) enum Imm {
    Const(u8),
    Lo(String, u16),
    Hi(String, u16),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Reg {
    A = 0,
    B = 1,
    PL = 2,
    PH = 3,
    Zero = 4,
}

#[derive(Debug, Clone, Copy)]
pub(super) enum Cond {
    C,
    NC,
    Z,
    NZ,
    O,
    NO,
    S,
    NS,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum ArithmOp {
    MOV,
    ADD,
    ADC,
    SUB,
    SBB,
    INC,
    DEC,
    SHL,
    SHR,
    SAR,
    AND,
    OR,
    XOR,
    NOT,
    NEG,
    EXP,
}

impl InstructionWriter {
    pub fn new() -> Self {
        InstructionWriter {
            text: Vec::new(),
            exports: HashSet::new(),
            weaks: HashSet::new(),
            imports: HashSet::new(),
            bss: HashMap::new(),
            data: HashMap::new(),
            last: [None, None, None, None, Some(0)],
            next_label: 0,
            text_bytes: 0,
        }
    }

    pub fn get_text_bytes(&self) -> usize {
        self.text_bytes
    }

    pub fn begin_function(&mut self, name: &GlobalVarId, custom_secion: Option<&str>) {
        let name = get_global_var_label(&name);
        self.reset_last();
        let segment_name = format!("{}_{:X}", name, *TRANSLATION_UNIT_UID);
        let section = if let Some(custom_section) = custom_secion {
            Section::Custom(custom_section.to_string())
        } else {
            Section::Text
        };
        self.text.push((segment_name, section, Vec::new()));
        self.label(name);
    }

    pub fn alloc_label(&mut self) -> String {
        let n = self.next_label;
        self.next_label += 1;
        format!("__cc_int_{}", n)
    }

    pub fn comment(&mut self, comment: String) {
        self.push(TextItem::Comment(comment))
    }

    pub fn export(&mut self, symbol: String) {
        self.exports.insert(symbol);
    }

    pub fn export_weak(&mut self, symbol: String) {
        self.weaks.insert(symbol);
    }

    pub fn import(&mut self, symbol: String) {
        self.imports.insert(symbol);
    }

    pub fn bss(&mut self, label: String, size: u16, align: usize) {
        if let Some(_) = self.bss.insert(
            label,
            DataItem {
                data: size,
                align,
                section: Section::Bss,
            },
        ) {
            panic!("duplicate bss label");
        }
    }

    pub fn data_int(
        &mut self,
        label: String,
        val: u64,
        width: Width,
        align: usize,
        ro: bool,
        custom_secion: Option<&str>,
    ) {
        let section = if let Some(custom_section) = custom_secion {
            Section::Custom(custom_section.to_string())
        } else if val == 0 {
            Section::Bss
        } else if ro {
            Section::Rodata
        } else {
            Section::Data
        };
        if let Some(_) = self.data.insert(
            label,
            DataItem {
                data: DataValue::Int(val, width),
                align,
                section,
            },
        ) {
            panic!("duplicate data label");
        }
    }

    pub fn data_vec(
        &mut self,
        label: String,
        val: Vec<u8>,
        align: usize,
        ro: bool,
        custom_secion: Option<&str>,
    ) {
        let section = if let Some(custom_section) = custom_secion {
            Section::Custom(custom_section.to_string())
        } else if val.iter().all(|x| *x == 0) {
            Section::Bss
        } else if ro {
            Section::Rodata
        } else {
            Section::Data
        };
        if let Some(_) = self.data.insert(
            label,
            DataItem {
                data: DataValue::String(val),
                align,
                section,
            },
        ) {
            panic!("duplicate data label");
        }
    }

    pub fn label(&mut self, label: String) {
        self.reset_last();
        self.push(TextItem::Label(label));
    }

    pub fn jmp(&mut self) {
        self.reset_last();
        self.push(TextItem::Op(Op::Jmp));
    }

    pub fn jz(&mut self) {
        self.reset_last();
        self.push(TextItem::Op(Op::Jc(Cond::Z)));
    }
    pub fn jc(&mut self) {
        self.reset_last();
        self.push(TextItem::Op(Op::Jc(Cond::C)));
    }
    pub fn jo(&mut self) {
        self.reset_last();
        self.push(TextItem::Op(Op::Jc(Cond::O)));
    }
    pub fn js(&mut self) {
        self.reset_last();
        self.push(TextItem::Op(Op::Jc(Cond::S)));
    }
    pub fn jnz(&mut self) {
        self.reset_last();
        self.push(TextItem::Op(Op::Jc(Cond::NZ)));
    }
    pub fn jnc(&mut self) {
        self.reset_last();
        self.push(TextItem::Op(Op::Jc(Cond::NC)));
    }
    pub fn jno(&mut self) {
        self.reset_last();
        self.push(TextItem::Op(Op::Jc(Cond::NO)));
    }
    pub fn jns(&mut self) {
        self.reset_last();
        self.push(TextItem::Op(Op::Jc(Cond::NS)));
    }

    pub fn nop(&mut self) {
        self.push(TextItem::Op(Op::Nop));
    }

    pub fn ldi_const(&mut self, reg: Reg, val: u8, allow_incdec: bool) {
        if reg == Reg::Zero {
            panic!("Wrong use of zero register");
        }
        let last_val = self.last[reg as usize];
        if let Some(v) = last_val {
            if v == val {
                return;
            }
            if allow_incdec {
                if v.wrapping_add(1) == val {
                    self.inc(reg);
                    self.last[reg as usize] = Some(val);
                    return;
                } else if v.wrapping_sub(1) == val {
                    self.dec(reg);
                    self.last[reg as usize] = Some(val);
                    return;
                }
            }
        }
        if reg == Reg::A && val == 0 {
            self.mov(Reg::A, Reg::Zero);
        } else {
            self.push(TextItem::Op(Op::Ldi(reg, Imm::Const(val))));
        }
        self.last[reg as usize] = Some(val);
    }

    pub fn ldi_lo(&mut self, reg: Reg, sym: String, offset: u16) {
        if reg == Reg::Zero {
            panic!("Wrong use of zero register");
        }
        self.push(TextItem::Op(Op::Ldi(reg, Imm::Lo(sym, offset))));
        self.last[reg as usize] = None;
    }

    pub fn ldi_hi(&mut self, reg: Reg, sym: String, offset: u16) {
        if reg == Reg::Zero {
            panic!("Wrong use of zero register");
        }
        self.push(TextItem::Op(Op::Ldi(reg, Imm::Hi(sym, offset))));
        self.last[reg as usize] = None;
    }

    pub fn ldi_p_const(&mut self, val: u16, allow_incdec: bool) {
        self.ldi_const(Reg::PL, (val & 0xff) as u8, allow_incdec);
        self.ldi_const(Reg::PH, (val >> 8) as u8, allow_incdec);
    }

    pub fn ldi_p_sym(&mut self, sym: String, offset: u16) {
        self.ldi_lo(Reg::PL, sym.clone(), offset);
        self.ldi_hi(Reg::PH, sym, offset);
    }

    pub fn ldi_p_var_location(
        &mut self,
        v: &VarLocation<FrameReg>,
        offset: u16,
        allow_incdec: bool,
    ) {
        match v {
            VarLocation::Local(reg) => {
                let addr = reg.get_address();
                self.ldi_p_const(addr + offset, allow_incdec);
            }
            VarLocation::Global(g) => {
                self.ldi_p_sym(global::get_global_var_label(g), offset);
            }
            VarLocation::Return => {
                self.ldi_p_sym(global::RET_VALUE_REG_SYMBOL.to_string(), offset);
            }
        }
    }

    pub fn ldi_pl_var_location_lo(
        &mut self,
        v: &VarLocation<FrameReg>,
        offset: u16,
        allow_incdec: bool,
    ) {
        match v {
            VarLocation::Local(reg) => {
                let addr = reg.get_address();
                self.ldi_const(Reg::PL, (addr + offset) as u8, allow_incdec);
            }
            VarLocation::Global(g) => {
                self.ldi_lo(Reg::PL, global::get_global_var_label(g), offset);
            }
            VarLocation::Return => {
                self.ldi_lo(Reg::PL, global::RET_VALUE_REG_SYMBOL.to_string(), offset);
            }
        }
    }

    pub fn mov(&mut self, dst: Reg, src: Reg) {
        self.last[dst as usize] = self.last[src as usize];
        self.arithm(ArithmOp::MOV, dst, src);
    }
    pub fn add(&mut self, dst: Reg, src: Reg) {
        self.last[dst as usize] = self.last[dst as usize]
            .and_then(|dst| self.last[src as usize].map(|src| dst.wrapping_add(src)));
        self.arithm(ArithmOp::ADD, dst, src);
    }
    pub fn adc(&mut self, dst: Reg, src: Reg) {
        self.last[dst as usize] = None;
        self.arithm(ArithmOp::ADC, dst, src);
    }
    pub fn sub(&mut self, dst: Reg, src: Reg) {
        self.last[dst as usize] = self.last[dst as usize]
            .and_then(|dst| self.last[src as usize].map(|src| dst.wrapping_sub(src)));
        self.arithm(ArithmOp::SUB, dst, src);
    }
    pub fn sbb(&mut self, dst: Reg, src: Reg) {
        self.last[dst as usize] = None;
        self.arithm(ArithmOp::SBB, dst, src);
    }
    pub fn inc(&mut self, dst: Reg) {
        self.last[dst as usize] = self.last[dst as usize].map(|dst| dst.wrapping_add(1));
        self.arithm_unary(ArithmOp::INC, dst);
    }
    pub fn dec(&mut self, dst: Reg) {
        self.last[dst as usize] = self.last[dst as usize].map(|dst| dst.wrapping_sub(1));
        self.arithm_unary(ArithmOp::DEC, dst);
    }
    pub fn shl(&mut self, dst: Reg) {
        self.last[dst as usize] = self.last[dst as usize].map(|dst| dst.wrapping_shl(1));
        self.arithm_unary(ArithmOp::SHL, dst);
    }
    pub fn shr(&mut self, dst: Reg) {
        self.last[dst as usize] = self.last[dst as usize].map(|dst| dst.wrapping_shr(1));
        self.arithm_unary(ArithmOp::SHR, dst);
    }
    pub fn sar(&mut self, dst: Reg) {
        self.last[dst as usize] = self.last[dst as usize].map(wrapping_sar);
        self.arithm_unary(ArithmOp::SAR, dst);
    }
    pub fn and(&mut self, dst: Reg, src: Reg) {
        self.last[dst as usize] =
            self.last[dst as usize].and_then(|dst| self.last[src as usize].map(|src| dst & src));
        self.arithm(ArithmOp::AND, dst, src);
    }
    pub fn or(&mut self, dst: Reg, src: Reg) {
        self.last[dst as usize] =
            self.last[dst as usize].and_then(|dst| self.last[src as usize].map(|src| dst | src));
        self.arithm(ArithmOp::OR, dst, src);
    }
    pub fn xor(&mut self, dst: Reg, src: Reg) {
        self.last[dst as usize] =
            self.last[dst as usize].and_then(|dst| self.last[src as usize].map(|src| dst ^ src));
        self.arithm(ArithmOp::XOR, dst, src);
    }
    pub fn not(&mut self, dst: Reg) {
        self.last[dst as usize] = self.last[dst as usize].map(|x| !x);
        self.arithm_unary(ArithmOp::NOT, dst);
    }
    pub fn neg(&mut self, dst: Reg) {
        self.last[dst as usize] = self.last[dst as usize].map(|x| (!x).wrapping_add(1));
        self.arithm_unary(ArithmOp::NEG, dst);
    }
    pub fn exp(&mut self, dst: Reg) {
        self.last[dst as usize] = None;
        self.arithm_unary(ArithmOp::EXP, dst);
    }

    pub fn ld(&mut self, dst: Reg) {
        if let Reg::Zero = dst {
            panic!("Zero can't be a destination register");
        }
        self.push(TextItem::Op(Op::Ld(dst)));
        self.last[dst as usize] = None;
    }

    pub fn st(&mut self, dst: Reg) {
        match dst {
            Reg::Zero | Reg::PL | Reg::PH => panic!("wrong register to store"),
            _ => (),
        };
        self.push(TextItem::Op(Op::St(dst)));
    }

    fn arithm(&mut self, op: ArithmOp, dst: Reg, src: Reg) {
        let (inv, src) = if let Reg::A = dst {
            if let Reg::A = src {
                panic!("both operands cannot be A");
            }
            (false, src)
        } else if let Reg::Zero = dst {
            panic!("Zero can't be a destination register");
        } else {
            if let Reg::A = src {
                (true, dst)
            } else {
                panic!("one of opernads must be A");
            }
        };
        self.push(TextItem::Op(Op::Arithm(op, src, inv)));
    }

    fn arithm_unary(&mut self, op: ArithmOp, dst: Reg) {
        if let Reg::Zero = dst {
            panic!("Zero can't be a destination register");
        }
        let inv = if let Reg::A = dst { false } else { true };
        self.push(TextItem::Op(Op::Arithm(op, dst, inv)));
    }

    fn push(&mut self, item: TextItem) {
        if let TextItem::Op(op) = &item {
            self.text_bytes += op.get_size();
        }
        self.text.last_mut().unwrap().2.push(item);
    }

    fn reset_last(&mut self) {
        self.last = [None, None, None, None, Some(0)];
    }
}

impl Op {
    pub fn get_size(&self) -> usize {
        match self {
            Op::Ldi(_, _) => 2,
            _ => 1,
        }
    }
}

impl TextItem {
    pub fn get_size(&self) -> usize {
        match self {
            TextItem::Op(op) => op.get_size(),
            TextItem::Label(_) => 0,
            TextItem::Comment(_) => 0,
        }
    }
}

impl DataItem<DataValue> {
    pub fn get_size(&self) -> usize {
        self.data.get_size()
    }
}

impl DataValue {
    pub fn get_size(&self) -> usize {
        match self {
            DataValue::Int(_, w) => *w as usize,
            DataValue::String(v) => v.len(),
        }
    }
}

fn wrapping_sar(x: u8) -> u8 {
    let msb = x & 0x80;
    x.wrapping_shr(1) | msb
}

impl std::fmt::Display for InstructionWriter {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        for symbol in &self.exports {
            writeln!(f, "\t.export {}", symbol)?;
        }
        for symbol in &self.weaks {
            writeln!(f, "\t.weak {}", symbol)?;
        }
        writeln!(f, "")?;
        for symbol in &self.imports {
            writeln!(f, "\t.global {}", symbol)?;
        }
        writeln!(f, "")?;
        for (id, section, text) in &self.text {
            writeln!(f, "\t.section {}.{}", section, id)?;
            for item in text {
                write!(f, "{}", item)?;
            }
        }
        writeln!(f, "")?;
        for (symbol, item) in &self.data {
            writeln!(f, "\t.section {}.{}", item.section, symbol)?;
            writeln!(f, "\t.align {}", item.align)?;
            writeln!(f, "{}: {}", symbol, item.data)?
        }
        writeln!(f, "")?;
        for (symbol, item) in &self.bss {
            writeln!(f, "\t.section bss.{}", symbol)?;
            writeln!(f, "\t.align {}", item.align)?;
            writeln!(f, "{}: res {}", symbol, item.data)?
        }
        Ok(())
    }
}

impl std::fmt::Display for DataValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            DataValue::Int(n, Width::Byte) => write!(f, "db {}", n),
            DataValue::Int(n, Width::Word) => write!(f, "dw {}", n),
            DataValue::Int(n, Width::Dword) => write!(f, "dd {}", n),
            DataValue::Int(n, Width::Qword) => write!(f, "dq {}", n),
            DataValue::String(v) => {
                for (i, b) in v.iter().enumerate() {
                    if i % 16 == 0 {
                        write!(f, "\tdb ")?;
                    }
                    write!(f, "0x{:02x}", b)?;
                    if i != v.len() - 1 {
                        if i % 16 == 15 {
                            write!(f, "\n")?;
                        } else {
                            write!(f, ", ")?;
                        }
                    }
                }
                Ok(())
            }
        }
    }
}

impl std::fmt::Display for TextItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            TextItem::Label(l) => writeln!(f, "{}:", l),
            TextItem::Op(op) => writeln!(f, "\t{}", op),
            TextItem::Comment(c) => writeln!(f, "\t; {}", c),
        }
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Reg::A => f.write_str("a"),
            Reg::B => f.write_str("b"),
            Reg::PL => f.write_str("pl"),
            Reg::PH => f.write_str("ph"),
            Reg::Zero => f.write_str("0"),
        }
    }
}

impl std::fmt::Display for Imm {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Imm::Const(c) => write!(f, "0x{:02X}", c),
            Imm::Lo(sym, 0) => write!(f, "lo({})", sym),
            Imm::Lo(sym, n) => write!(f, "lo({} + {})", sym, n),
            Imm::Hi(sym, 0) => write!(f, "hi({})", sym),
            Imm::Hi(sym, n) => write!(f, "hi({} + {})", sym, n),
        }
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Op::Arithm(ArithmOp::MOV, reg, false) => write!(f, "mov a, {}", reg),
            Op::Arithm(ArithmOp::MOV, reg, true) => write!(f, "mov {}, a", reg),
            Op::Arithm(ArithmOp::ADD, reg, false) => write!(f, "add a, {}", reg),
            Op::Arithm(ArithmOp::ADD, reg, true) => write!(f, "add {}, a", reg),
            Op::Arithm(ArithmOp::ADC, reg, false) => write!(f, "adc a, {}", reg),
            Op::Arithm(ArithmOp::ADC, reg, true) => write!(f, "adc {}, a", reg),
            Op::Arithm(ArithmOp::SUB, reg, false) => write!(f, "sub a, {}", reg),
            Op::Arithm(ArithmOp::SUB, reg, true) => write!(f, "sub {}, a", reg),
            Op::Arithm(ArithmOp::SBB, reg, false) => write!(f, "sbb a, {}", reg),
            Op::Arithm(ArithmOp::SBB, reg, true) => write!(f, "sbb {}, a", reg),
            Op::Arithm(ArithmOp::AND, reg, false) => write!(f, "and a, {}", reg),
            Op::Arithm(ArithmOp::AND, reg, true) => write!(f, "and {}, a", reg),
            Op::Arithm(ArithmOp::OR, reg, false) => write!(f, "or  a, {}", reg),
            Op::Arithm(ArithmOp::OR, reg, true) => write!(f, "or  {}, a", reg),
            Op::Arithm(ArithmOp::XOR, reg, false) => write!(f, "xor a, {}", reg),
            Op::Arithm(ArithmOp::XOR, reg, true) => write!(f, "xor {}, a", reg),

            Op::Arithm(ArithmOp::NOT, Reg::A, false) => write!(f, "not a"),
            Op::Arithm(ArithmOp::NOT, reg, true) => write!(f, "not {}", reg),
            Op::Arithm(ArithmOp::NEG, Reg::A, false) => write!(f, "neg a"),
            Op::Arithm(ArithmOp::NEG, reg, true) => write!(f, "neg {}", reg),
            Op::Arithm(ArithmOp::EXP, Reg::A, false) => write!(f, "exp a"),
            Op::Arithm(ArithmOp::EXP, reg, true) => write!(f, "exp {}", reg),
            Op::Arithm(ArithmOp::INC, Reg::A, false) => write!(f, "inc a"),
            Op::Arithm(ArithmOp::INC, reg, true) => write!(f, "inc {}", reg),
            Op::Arithm(ArithmOp::DEC, Reg::A, false) => write!(f, "dec a"),
            Op::Arithm(ArithmOp::DEC, reg, true) => write!(f, "dec {}", reg),
            Op::Arithm(ArithmOp::SHL, Reg::A, false) => write!(f, "shl a"),
            Op::Arithm(ArithmOp::SHL, reg, true) => write!(f, "shl {}", reg),
            Op::Arithm(ArithmOp::SHR, Reg::A, false) => write!(f, "shr a"),
            Op::Arithm(ArithmOp::SHR, reg, true) => write!(f, "shr {}", reg),
            Op::Arithm(ArithmOp::SAR, Reg::A, false) => write!(f, "sar a"),
            Op::Arithm(ArithmOp::SAR, reg, true) => write!(f, "sar {}", reg),

            Op::Arithm(_, _, _) => unreachable!(),

            Op::Ldi(reg, imm) => write!(f, "ldi {}, {}", reg, imm),
            Op::Ld(reg) => write!(f, "ld  {}", reg),
            Op::St(reg) => write!(f, "st  {}", reg),
            Op::Jmp => f.write_str("jmp"),
            Op::Nop => f.write_str("nop"),

            Op::Jc(Cond::C) => f.write_str("jc"),
            Op::Jc(Cond::NC) => f.write_str("jnc"),
            Op::Jc(Cond::Z) => f.write_str("jz"),
            Op::Jc(Cond::NZ) => f.write_str("jnz"),
            Op::Jc(Cond::O) => f.write_str("jo"),
            Op::Jc(Cond::NO) => f.write_str("jno"),
            Op::Jc(Cond::S) => f.write_str("js"),
            Op::Jc(Cond::NS) => f.write_str("jns"),
        }
    }
}

impl std::fmt::Display for Section {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Section::Bss => f.write_str("bss"),
            Section::Data => f.write_str("data"),
            Section::Text => f.write_str("text"),
            Section::Rodata => f.write_str("rodata"),
            Section::Custom(s) => f.write_str(s),
        }
    }
}
