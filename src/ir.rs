pub enum Width {
    Byte,
    Word,
    Dword,
    Qword,
}

pub type Reg = u32;
pub type Sign = bool;
pub type BlockNumber = usize;

pub enum Src {
    ConstInt(u64),
    SymbolOffset(String, u16),
    Reg(Reg),
    StackPointer,
}

#[rustfmt::skip]
pub enum Op {
    Add { width: Width, sign: Sign, dst: Reg, src1: Src, src2: Src },
    Sub { width: Width, sign: Sign, dst: Reg, src1: Src, src2: Src },
    Mul { width: Width, sign: Sign, dst: Reg, src1: Src, src2: Src },
    Div { width: Width, sign: Sign, dst: Reg, src1: Src, src2: Src },
    Mod { width: Width, sign: Sign, dst: Reg, src1: Src, src2: Src },
    BAnd { width: Width, dst: Reg, src1: Src, src2: Src },
    BOr { width: Width, dst: Reg, src1: Src, src2: Src },
    BXor { width: Width, dst: Reg, src1: Src, src2: Src },
    BNot { width: Width, dst: Reg, src: Src },
    Lshift { width: Width, sign: Sign, dst: Reg, src1: Src, src2: Src },
    Rshift { width: Width, sign: Sign, dst: Reg, src1: Src, src2: Src },
    Conv { dst_width: Width, dst_sign: Sign, dst: Reg, src_width: Width, src_sign: Sign, src: Src },
    Store { dst_addr: Src, width: Width, src: Src },
    Load { width: Width, dst: Reg, src_addr: Src },
    Call { dst: Reg, dst_width: Width, name: String, args: Vec<(Src, Width)> },
}

pub enum Tail {
    Jump(BlockNumber),
    Cond(Src, BlockNumber, BlockNumber),
    Ret,
}

pub(crate) struct Block {
    pub ops: Vec<Op>,
    pub tail: Tail,
}

pub(crate) struct Function {
    pub stack_size: u16,
    pub blocks: Vec<Block>,
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
