use std::collections::{HashMap, HashSet};
use std::fmt::Formatter;

use crate::generic_ir::Width;

pub struct InstructionWriter {
    exports: HashSet<String>,
    imports: HashSet<String>,
    text: Vec<(String, Vec<TextItem>)>,
    bss: HashMap<String, DataItem<u16>>,
    data: HashMap<String, DataItem<DataValue>>,
}

#[derive(Debug, Clone)]
enum TextItem {
    Op(Op),
    Label(String),
}

#[derive(Debug, Clone)]
struct DataItem<Inner> {
    data: Inner,
    align: usize,
}

#[derive(Debug, Clone)]
enum DataValue {
    Int(u64, Width),
    String(Vec<u8>),
}

#[derive(Debug, Clone)]
enum Op {
    Arithm(ArithmOp, Reg, bool),
    Ldi(Reg, Imm),
    Ld(Reg),
    St(Reg),
    Jmp,
    Nop,
    Jc(Cond),
}

#[derive(Debug, Clone)]
enum Imm {
    Const(u8),
    Lo(String, u16),
    Hi(String, u16),
}

#[derive(Debug, Clone, Copy)]
pub enum Reg {
    A,
    B,
    PL,
    PH,
    Zero,
}

#[derive(Debug, Clone, Copy)]
enum Cond {
    C,
    NC,
    Z,
    NZ,
    O,
    NO,
    S,
    NS,
}

#[derive(Debug, Clone, Copy)]
enum ArithmOp {
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
            imports: HashSet::new(),
            bss: HashMap::new(),
            data: HashMap::new(),
        }
    }

    pub fn begin_function(&mut self, name: String) {
        self.text.push((name, Vec::new()));
    }

    pub fn export(&mut self, symbol: String) {
        self.exports.insert(symbol);
    }

    pub fn import(&mut self, symbol: String) {
        self.imports.insert(symbol);
    }

    pub fn bss(&mut self, label: String, size: u16, align: usize) {
        if let Some(_) = self.bss.insert(label, DataItem { data: size, align }) {
            panic!("duplicate bss label");
        }
    }

    pub fn data_int(&mut self, label: String, val: u64, width: Width, align: usize) {
        if let Some(_) = self.data.insert(
            label,
            DataItem {
                data: DataValue::Int(val, width),
                align,
            },
        ) {
            panic!("duplicate data label");
        }
    }

    pub fn data_vec(&mut self, label: String, val: Vec<u8>, align: usize) {
        if let Some(_) = self.data.insert(
            label,
            DataItem {
                data: DataValue::String(val),
                align,
            },
        ) {
            panic!("duplicate data label");
        }
    }

    pub fn label(&mut self, label: String) {
        self.push(TextItem::Label(label));
    }

    pub fn jmp(&mut self) {
        self.push(TextItem::Op(Op::Jmp));
    }

    pub fn jz(&mut self) {
        self.push(TextItem::Op(Op::Jc(Cond::Z)));
    }
    pub fn jc(&mut self) {
        self.push(TextItem::Op(Op::Jc(Cond::C)));
    }
    pub fn jo(&mut self) {
        self.push(TextItem::Op(Op::Jc(Cond::O)));
    }
    pub fn js(&mut self) {
        self.push(TextItem::Op(Op::Jc(Cond::S)));
    }
    pub fn jnz(&mut self) {
        self.push(TextItem::Op(Op::Jc(Cond::NZ)));
    }
    pub fn jnc(&mut self) {
        self.push(TextItem::Op(Op::Jc(Cond::NC)));
    }
    pub fn jno(&mut self) {
        self.push(TextItem::Op(Op::Jc(Cond::NO)));
    }
    pub fn jns(&mut self) {
        self.push(TextItem::Op(Op::Jc(Cond::NS)));
    }

    pub fn nop(&mut self) {
        self.push(TextItem::Op(Op::Nop));
    }

    pub fn ldi_const(&mut self, reg: Reg, val: u8) {
        self.push(TextItem::Op(Op::Ldi(reg, Imm::Const(val))))
    }

    pub fn ldi_lo(&mut self, reg: Reg, sym: String, offset: u16) {
        self.push(TextItem::Op(Op::Ldi(reg, Imm::Lo(sym, offset))))
    }

    pub fn ldi_hi(&mut self, reg: Reg, sym: String, offset: u16) {
        self.push(TextItem::Op(Op::Ldi(reg, Imm::Hi(sym, offset))))
    }

    pub fn ldi_p_const(&mut self, val: u16) {
        self.ldi_const(Reg::PL, (val & 0xff) as u8);
        self.ldi_const(Reg::PH, (val >> 8) as u8);
    }

    pub fn ldi_p_sym(&mut self, sym: String, offset: u16) {
        self.ldi_lo(Reg::PL, sym.clone(), offset);
        self.ldi_hi(Reg::PH, sym, offset);
    }

    pub fn mov(&mut self, dst: Reg, src: Reg) {
        self.arithm(ArithmOp::MOV, dst, src);
    }
    pub fn add(&mut self, dst: Reg, src: Reg) {
        self.arithm(ArithmOp::ADD, dst, src);
    }
    pub fn adc(&mut self, dst: Reg, src: Reg) {
        self.arithm(ArithmOp::ADC, dst, src);
    }
    pub fn sub(&mut self, dst: Reg, src: Reg) {
        self.arithm(ArithmOp::SUB, dst, src);
    }
    pub fn sbb(&mut self, dst: Reg, src: Reg) {
        self.arithm(ArithmOp::SBB, dst, src);
    }
    pub fn inc(&mut self, dst: Reg) {
        self.arithm_unary(ArithmOp::INC, dst);
    }
    pub fn dec(&mut self, dst: Reg) {
        self.arithm_unary(ArithmOp::DEC, dst);
    }
    pub fn shl(&mut self, dst: Reg, src: Reg) {
        self.arithm(ArithmOp::SHL, dst, src);
    }
    pub fn shr(&mut self, dst: Reg, src: Reg) {
        self.arithm(ArithmOp::SHR, dst, src);
    }
    pub fn sar(&mut self, dst: Reg, src: Reg) {
        self.arithm(ArithmOp::SAR, dst, src);
    }
    pub fn and(&mut self, dst: Reg, src: Reg) {
        self.arithm(ArithmOp::AND, dst, src);
    }
    pub fn or(&mut self, dst: Reg, src: Reg) {
        self.arithm(ArithmOp::OR, dst, src);
    }
    pub fn xor(&mut self, dst: Reg, src: Reg) {
        self.arithm(ArithmOp::XOR, dst, src);
    }
    pub fn not(&mut self, dst: Reg) {
        self.arithm_unary(ArithmOp::NOT, dst);
    }
    pub fn neg(&mut self, dst: Reg) {
        self.arithm_unary(ArithmOp::NEG, dst);
    }
    pub fn exp(&mut self, dst: Reg) {
        self.arithm_unary(ArithmOp::EXP, dst);
    }

    pub fn ld(&mut self, dst: Reg) {
        if let Reg::Zero = dst {
            panic!("Zero can't be a destination register");
        }
        self.push(TextItem::Op(Op::Ld(dst)));
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
            if let Reg::Zero = src {
                panic!("cannot assign Zero to any other register than A");
            }
            (true, src)
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
        self.text.last_mut().unwrap().1.push(item);
    }
}

impl std::fmt::Display for InstructionWriter {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        for symbol in &self.exports {
            writeln!(f, "\t.export {}", symbol)?;
        }
        writeln!(f, "")?;
        for symbol in &self.imports {
            writeln!(f, "\t.global {}", symbol)?;
        }
        writeln!(f, "")?;
        for (symbol, text) in &self.text {
            writeln!(f, "\t.section text.{}", symbol)?;
            writeln!(f, "{}:", symbol)?;
            for item in text {
                writeln!(f, "{}", item)?;
            }
        }
        writeln!(f, "")?;
        for (symbol, item) in &self.data {
            writeln!(f, "\t.section data.{}", symbol)?;
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
            Imm::Const(c) => write!(f, "{}", c),
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
            Op::Arithm(ArithmOp::SHL, reg, false) => write!(f, "shl a, {}", reg),
            Op::Arithm(ArithmOp::SHL, reg, true) => write!(f, "shl {}, a", reg),
            Op::Arithm(ArithmOp::SHR, reg, false) => write!(f, "shr a, {}", reg),
            Op::Arithm(ArithmOp::SHR, reg, true) => write!(f, "shr {}, a", reg),
            Op::Arithm(ArithmOp::SAR, reg, false) => write!(f, "sar a, {}", reg),
            Op::Arithm(ArithmOp::SAR, reg, true) => write!(f, "sar {}, a", reg),
            Op::Arithm(ArithmOp::AND, reg, false) => write!(f, "and a, {}", reg),
            Op::Arithm(ArithmOp::AND, reg, true) => write!(f, "and {}, a", reg),
            Op::Arithm(ArithmOp::OR, reg, false) => write!(f, "or a, {}", reg),
            Op::Arithm(ArithmOp::OR, reg, true) => write!(f, "or {}, a", reg),
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

            Op::Arithm(_, _, _) => unreachable!(),

            Op::Ldi(reg, imm) => write!(f, "ldi {}, {}", reg, imm),
            Op::Ld(reg) => write!(f, "ld {}", reg),
            Op::St(reg) => write!(f, "st {}", reg),
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