use std::fmt::Formatter;

use crate::machine;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord)]
pub enum Width {
    Byte,
    Word,
    Dword,
    Qword,
}

pub type Reg = u32;
pub type Sign = bool;
pub type BlockNumber = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarLocation {
    Global(GlobalVarId),
    Local(Reg),
    Arg(usize),
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct GlobalVarId(pub String, pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scalar {
    ConstInt(u64),
    SymbolOffset(String, u16),
    Var(VarLocation),
}

#[derive(Clone, PartialEq, Eq)]
pub enum Op {
    Copy(UnaryUnsignedOp),
    Bool(UnaryUnsignedOp),
    BoolInv(UnaryUnsignedOp),
    Add(BinaryOp),
    Sub(BinaryOp),
    Mul(BinaryOp),
    Div(BinaryOp),
    Mod(BinaryOp),
    BAnd(BinaryUnsignedOp),
    BOr(BinaryUnsignedOp),
    BXor(BinaryUnsignedOp),
    LShift(ShiftOp),
    RShift(ShiftOp),
    Neg(UnaryUnsignedOp),
    Not(UnaryUnsignedOp),
    Compare(CompareOp),
    Conv(ConvOp),
    Store(StoreOp),
    Load(LoadOp),
    Call(CallOp),
    LoadAddr(LoadAddrOp),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompareKind {
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareOp {
    pub kind: CompareKind,
    pub dst_width: Width,
    pub dst: VarLocation,
    pub width: Width,
    pub sign: Sign,
    pub lhs: Scalar,
    pub rhs: Scalar,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnaryUnsignedOp {
    pub width: Width,
    pub dst: VarLocation,
    pub src: Scalar,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryOp {
    pub width: Width,
    pub sign: Sign,
    pub dst: VarLocation,
    pub lhs: Scalar,
    pub rhs: Scalar,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryUnsignedOp {
    pub width: Width,
    pub dst: VarLocation,
    pub lhs: Scalar,
    pub rhs: Scalar,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShiftOp {
    pub lhs_width: Width,
    pub lhs_sign: Sign,
    pub dst: VarLocation,
    pub lhs: Scalar,
    pub rhs: Scalar, // only one byte is used
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConvOp {
    pub dst_width: Width,
    pub dst_sign: Sign,
    pub dst: VarLocation,
    pub src_width: Width,
    pub src_sign: Sign,
    pub src: Scalar,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StoreOp {
    pub dst_addr: Scalar,
    pub width: Width,
    pub src: Scalar,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoadOp {
    pub width: Width,
    pub dst: VarLocation,
    pub src_addr: Scalar,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallOp {
    pub dst: Option<(VarLocation, Width)>,
    pub addr: Scalar,
    pub args: Vec<(Scalar, Width)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoadAddrOp {
    pub dst: VarLocation,
    pub src: VarLocation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tail {
    Jump(BlockNumber),
    Cond(Scalar, BlockNumber, BlockNumber),
    Ret,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Phi {
    dst: Reg,
    srcs: Vec<(BlockNumber, Reg)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericBlock<GTail> {
    pub phi: Vec<Phi>,
    pub ops: Vec<Op>,
    pub tail: GTail,
}

pub type Block = GenericBlock<Tail>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub stack_size: u16,
    pub blocks: Vec<Block>,
}

impl Width {
    pub const fn new(w: u8) -> Self {
        match w {
            1 => Self::Byte,
            2 => Self::Word,
            4 => Self::Dword,
            8 => Self::Qword,
            _ => panic!("invalid width"),
        }
    }

    pub const INT_WIDTH: Self = Self::new(machine::INT_SIZE);
}

impl From<Width> for u8 {
    fn from(w: Width) -> u8 {
        match w {
            Width::Byte => 1,
            Width::Word => 2,
            Width::Dword => 4,
            Width::Qword => 8,
        }
    }
}

impl PartialOrd for Width {
    fn partial_cmp(&self, other: &Width) -> Option<std::cmp::Ordering> {
        let self_u8: u8 = (*self).into();
        let other_u8: u8 = (*other).into();
        self_u8.partial_cmp(&other_u8)
    }
}

impl<GTail> std::fmt::Display for GenericBlock<GTail>
where
    GTail: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        for phi in &self.phi {
            writeln!(f, "{}", phi)?;
        }
        for op in &self.ops {
            writeln!(f, "{}", op)?;
        }
        writeln!(f, "{}", self.tail)
    }
}

impl std::fmt::Display for Phi {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "r{} = ", self.dst)?;
        for s in self
            .srcs
            .iter()
            .map(|(n, r)| format!("{} -> r{}", n, r))
            .intersperse(", ".to_string())
        {
            f.write_str(&s)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Copy(op) => write!(f, "copy{}", op),
            Self::Add(op) => write!(f, "add{}", op),
            Self::Sub(op) => write!(f, "sub{}", op),
            Self::Mul(op) => write!(f, "mul{}", op),
            Self::Div(op) => write!(f, "div{}", op),
            Self::Mod(op) => write!(f, "mod{}", op),
            Self::BAnd(op) => write!(f, "and{}", op),
            Self::BOr(op) => write!(f, "or{}", op),
            Self::BXor(op) => write!(f, "xor{}", op),
            Self::LShift(op) => write!(f, "lsh{}", op),
            Self::RShift(op) => write!(f, "rsh{}", op),
            Self::Neg(op) => write!(f, "neg{}", op),
            Self::Not(op) => write!(f, "not{}", op),
            Self::Compare(op) => write!(f, "cmp{}", op),
            Self::Conv(op) => write!(f, "conv{}", op),
            Self::Bool(op) => write!(f, "bool{}", op),
            Self::BoolInv(op) => write!(f, "binv{}", op),
            Self::Store(op) => write!(f, "st{}", op),
            Self::Load(op) => write!(f, "ld{}", op),
            Self::Call(op) => write!(f, "call{}", op),
            Self::LoadAddr(op) => write!(f, "addr{}", op),
        }
    }
}

fn get_sign_char(sign: bool) -> char {
    if sign {
        's'
    } else {
        'u'
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}{} {}, {}, {}",
            get_sign_char(self.sign),
            self.width,
            self.dst,
            self.lhs,
            self.rhs
        )
    }
}

impl std::fmt::Display for CompareKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            CompareKind::Equal => f.write_str("eq"),
            CompareKind::NotEqual => f.write_str("ne"),
            CompareKind::LessThan => f.write_str("lt"),
            CompareKind::LessOrEqual => f.write_str("le"),
            CompareKind::GreaterThan => f.write_str("gt"),
            CompareKind::GreaterOrEqual => f.write_str("ge"),
        }
    }
}
impl std::fmt::Display for CompareOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}{}{}{} {}, {}, {}",
            self.kind,
            self.dst_width,
            get_sign_char(self.sign),
            self.width,
            self.dst,
            self.lhs,
            self.rhs
        )
    }
}

impl std::fmt::Display for BinaryUnsignedOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {}, {}, {}", self.width, self.dst, self.lhs, self.rhs)
    }
}

impl std::fmt::Display for UnaryUnsignedOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {}, {}", self.width, self.dst, self.src)
    }
}

impl std::fmt::Display for ShiftOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{} {}, {}, {}",
            self.lhs_width, self.dst, self.lhs, self.rhs
        )
    }
}

impl std::fmt::Display for StoreOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {}, {}", self.width, self.dst_addr, self.src)
    }
}

impl std::fmt::Display for LoadOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {}, {}", self.width, self.dst, self.src_addr)
    }
}

impl std::fmt::Display for CallOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if let Some((dst, dst_width)) = self.dst.as_ref() {
            write!(f, "{} {}, {}(", dst_width, dst, self.addr)?;
        } else {
            write!(f, " {}(", self.addr)?;
        }
        for s in self
            .args
            .iter()
            .map(|(s, w)| format!("{} {}", w, s))
            .intersperse(", ".to_string())
        {
            f.write_str(&s)?;
        }
        f.write_str(")")?;
        Ok(())
    }
}

impl std::fmt::Display for ConvOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}{}{}{} {}, {}",
            get_sign_char(self.dst_sign),
            self.dst_width,
            get_sign_char(self.src_sign),
            self.src_width,
            self.dst,
            self.src
        )
    }
}

impl std::fmt::Display for LoadAddrOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, " {}, {}", self.dst, self.src)
    }
}

impl std::fmt::Display for Width {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Width::Byte => f.write_str("b"),
            Width::Word => f.write_str("w"),
            Width::Dword => f.write_str("d"),
            Width::Qword => f.write_str("q"),
        }
    }
}

impl std::fmt::Display for Scalar {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Scalar::ConstInt(x) => write!(f, "{}", x),
            Scalar::SymbolOffset(sym, 0) => write!(f, "[{}]", sym),
            Scalar::SymbolOffset(sym, offs) => write!(f, "[{}+0x{:x}]", sym, offs),
            Scalar::Var(v) => write!(f, "{}", v),
        }
    }
}

impl std::fmt::Display for VarLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            VarLocation::Local(r) => write!(f, "%{}", r),
            VarLocation::Global(id) => write!(f, "{}", id),
            VarLocation::Arg(p) => write!(f, "%a{}", p),
        }
    }
}

impl std::fmt::Display for GlobalVarId {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "${}.{}", self.0, self.1)
    }
}

impl std::fmt::Display for Tail {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Tail::Jump(n) => write!(f, "jmp {}", n),
            Tail::Cond(c, t, e) => write!(f, "if {} then goto {} else goto {}", c, *t, *e),
            Tail::Ret => write!(f, "ret"),
        }
    }
}

impl std::fmt::Debug for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Display::fmt(&self, f)
    }
}
