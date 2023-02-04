
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Width {
    Byte,
    Word,
    Dword,
    Qword,
}

pub type Reg = u32;
pub type Sign = bool;
pub type BlockNumber = usize;

#[derive(Debug, Clone)]
pub enum VarLocation {
    Global(GlobalVarId),
    Local(Reg),
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct GlobalVarId(pub String, pub u32);

pub enum Src {
    ConstInt(u64),
    SymbolOffset(String, u16),
    Var(VarLocation),
    StackPointer,
}

pub enum Op {
    Add(BinaryOp),
    Sub(BinaryOp),
    Mul(BinaryOp),
    Div(BinaryOp),
    Mod(BinaryOp),
    BAnd(BinaryUnsignedOp),
    BOr(BinaryUnsignedOp),
    BXor(BinaryUnsignedOp),
    BNot(BinaryUnsignedOp),
    LShift(ShiftOp),
    RShift(ShiftOp),
    Conv(ConvOp),
    Store(StoreOp),
    Load(LoadOp),
    Call(CallOp),
}

pub struct BinaryOp {
    pub width: Width,
    pub sign: Sign,
    pub dst: VarLocation,
    pub lhs: Src,
    pub rhs: Src,
}

pub struct BinaryUnsignedOp {
    pub width: Width,
    pub dst: VarLocation,
    pub lhs: Src,
    pub rhs: Src,
}

pub struct ShiftOp {
    pub lhs_width: Width,
    pub lhs_sign: Sign,
    pub dst: VarLocation,
    pub lhs: Src,
    pub rhs: Src,
}

pub struct ConvOp {
    pub dst_width: Width,
    pub dst_sign: Sign,
    pub dst: VarLocation,
    pub src_width: Width,
    pub src_sign: Sign,
    pub src: Src,
}

pub struct StoreOp {
    pub dst_addr: Src,
    pub width: Width,
    pub src: Src,
}

pub struct LoadOp {
    pub width: Width,
    pub dst: VarLocation,
    pub src_addr: Src,
}

pub struct CallOp {
    pub dst: VarLocation,
    pub dst_width: Width,
    pub name: String,
    pub args: Vec<(Src, Width)>,
}

pub enum Tail {
    Jump(BlockNumber),
    Cond(Src, BlockNumber, BlockNumber),
    Ret,
}

pub struct Phi {
    dst: Reg,
    srcs: Vec<(BlockNumber, Reg)>,
}

pub struct GenericBlock<GTail> {
    pub phi: Vec<Phi>,
    pub ops: Vec<Op>,
    pub tail: GTail,
}

pub type Block = GenericBlock<Tail>;

pub struct Function {
    pub stack_size: u16,
    pub blocks: Vec<Block>,
}

impl Width {
    pub fn new(w: u8) -> Self {
        match w {
            1 => Self::Byte,
            2 => Self::Word,
            4 => Self::Dword,
            8 => Self::Qword,
            _ => panic!("invalid width")
        }
    }
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
