use std::{
    collections::{HashMap, HashSet},
    fmt::Formatter,
    hash::Hash,
    mem,
};

use crate::{machine, name_scope::NameScope, stats};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, Hash)]
pub enum Width {
    Byte = 1,
    Word = 2,
    Dword = 4,
    Qword = 8,
}

pub type VirtualReg = u32;
pub type Sign = bool;
pub type BlockNumber = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarLocation<Reg> {
    Global(GlobalVarId),
    Local(Reg),
    Return,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum GlobalVarId {
    CompilerInternal(String),
    Global(String),
    Static(String),
    LocalStatic {
        name: String,
        function_name: String,
        index: usize,
    },
    Literal(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scalar<Reg> {
    ConstInt(u64),
    SymbolOffset(GlobalVarId, u16),
    Var(VarLocation<Reg>),
}

#[derive(Clone, PartialEq, Eq)]
pub enum Op<Reg> {
    Undefined(Reg),
    Arg(ArgOp<Reg>),
    FramePointer(Reg),
    Copy(UnaryUnsignedOp<Reg>),
    Bool(UnaryUnsignedOp<Reg>),
    BoolInv(UnaryUnsignedOp<Reg>),
    Add(BinaryOp<Reg>),
    Sub(BinaryOp<Reg>),
    Mul(BinaryOp<Reg>),
    Div(BinaryOp<Reg>),
    Mod(BinaryOp<Reg>),
    BAnd(BinaryUnsignedOp<Reg>),
    BOr(BinaryUnsignedOp<Reg>),
    BXor(BinaryUnsignedOp<Reg>),
    LShift(ShiftOp<Reg>),
    RShift(ShiftOp<Reg>),
    Neg(UnaryUnsignedOp<Reg>),
    Not(UnaryUnsignedOp<Reg>),
    Compare(CompareOp<Reg>),
    Conv(ConvOp<Reg>),
    Store(StoreOp<Reg>),
    Load(LoadOp<Reg>),
    Call(CallOp<Reg>),
    Memcpy(MemcpyOp<Reg>),
    IntrinCall(IntrinCallOp<Reg>),
    VaStart(VaStartOp<Reg>),
    VaArg(VaArgOp<Reg>),
    VaListInc(VaListIncOp<Reg>),
    ByteSwap(UnaryUnsignedOp<Reg>),
    #[cfg(test)]
    Dummy(usize),
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
pub struct ArgOp<Reg> {
    pub width: Width,
    pub dst_reg: Reg,
    pub arg_number: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareOp<Reg> {
    pub dst_width: Width,
    pub dst: VarLocation<Reg>,
    pub desc: CompareDesc<Reg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompareDesc<Reg> {
    pub kind: CompareKind,
    pub width: Width,
    pub sign: Sign,
    pub lhs: Scalar<Reg>,
    pub rhs: Scalar<Reg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnaryUnsignedOp<Reg> {
    pub width: Width,
    pub dst: VarLocation<Reg>,
    pub src: Scalar<Reg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryOp<Reg> {
    pub width: Width,
    pub sign: Sign,
    pub dst: VarLocation<Reg>,
    pub lhs: Scalar<Reg>,
    pub rhs: Scalar<Reg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryUnsignedOp<Reg> {
    pub width: Width,
    pub dst: VarLocation<Reg>,
    pub lhs: Scalar<Reg>,
    pub rhs: Scalar<Reg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShiftOp<Reg> {
    pub lhs_width: Width,
    pub lhs_sign: Sign,
    pub dst: VarLocation<Reg>,
    pub lhs: Scalar<Reg>,
    pub rhs: Scalar<Reg>, // only one byte is used
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConvOp<Reg> {
    pub dst_width: Width,
    pub dst_sign: Sign,
    pub dst: VarLocation<Reg>,
    pub src_width: Width,
    pub src_sign: Sign,
    pub src: Scalar<Reg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StoreOp<Reg> {
    pub dst_addr: Scalar<Reg>,
    pub width: Width,
    pub src: Scalar<Reg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoadOp<Reg> {
    pub width: Width,
    pub dst: VarLocation<Reg>,
    pub src_addr: Scalar<Reg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallOp<Reg> {
    pub dst: Option<(VarLocation<Reg>, Width)>,
    pub addr: Scalar<Reg>,
    pub args: Vec<(Scalar<Reg>, Width)>,
    pub va_args: Vec<(Scalar<Reg>, Width)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemcpyOp<Reg> {
    pub dst_addr: Scalar<Reg>,
    pub src_addr: Scalar<Reg>,
    pub len: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntrinCallOp<Reg> {
    pub name: String,
    pub variant: IntrinCallVariant<Reg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntrinCallVariant<Reg> {
    Call2R(
        (Width, VarLocation<Reg>),
        (Width, Scalar<Reg>),
        (Width, Scalar<Reg>),
    ),
    Call3(
        (Width, Scalar<Reg>),
        (Width, Scalar<Reg>),
        (Width, Scalar<Reg>),
    ),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VaStartOp<Reg> {
    pub dst: VarLocation<Reg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VaArgOp<Reg> {
    pub dst: VarLocation<Reg>,
    pub src_va_list: Scalar<Reg>,
    pub width: Width,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VaListIncOp<Reg> {
    pub dst: VarLocation<Reg>,
    pub src: Scalar<Reg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tail<Reg> {
    Jump(BlockNumber),
    Cond(JumpCondition<Reg>, BlockNumber, BlockNumber),
    Ret,
    Switch(Scalar<Reg>, Width, Vec<(u64, BlockNumber)>, BlockNumber),
}

/// Condition variants for Tail::Cond
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JumpCondition<Reg> {
    /// 0 or 1
    StrictBool(Scalar<Reg>),
    /// 0 or any other value
    RelaxedBool(Scalar<Reg>, Width),
    /// Comparison
    Compare(CompareDesc<Reg>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Phi<Reg: Eq + Hash> {
    pub srcs: HashMap<Reg, (Width, Vec<(BlockNumber, Scalar<Reg>)>)>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct GenericBlock<GTail, Reg: Eq + Hash> {
    pub phi: Phi<Reg>,
    pub ops: Vec<Op<Reg>>,
    pub tail: GTail,
    pub loop_depth: usize,
    pub original_id: usize,
}

pub type Block<Reg> = GenericBlock<Tail<Reg>, Reg>;

#[derive(Clone, PartialEq, Eq)]
pub struct Function<Reg: Eq + Hash> {
    pub stack_size: u16,
    pub blocks: Vec<Block<Reg>>,
}

impl<Reg: Copy + Eq> Op<Reg> {
    pub fn is_write_to_register(&self, reg: Reg) -> bool {
        self.get_dst_reg() == Some(reg)
    }

    pub fn has_side_effects(&self) -> bool {
        match self {
            Op::Call(_) => true,
            Op::Memcpy(_) => true,
            Op::Store(_) => true,
            _ => false,
        }
    }

    pub fn get_dst_reg(&self) -> Option<Reg> {
        match self {
            Op::Undefined(target) => Some(*target),
            Op::Arg(op) => op.get_dst_reg(),
            Op::FramePointer(reg) => Some(*reg),
            Op::Copy(op) => op.get_dst_reg(),
            Op::Bool(op) => op.get_dst_reg(),
            Op::BoolInv(op) => op.get_dst_reg(),
            Op::Add(op) => op.get_dst_reg(),
            Op::Sub(op) => op.get_dst_reg(),
            Op::Mul(op) => op.get_dst_reg(),
            Op::Div(op) => op.get_dst_reg(),
            Op::Mod(op) => op.get_dst_reg(),
            Op::BAnd(op) => op.get_dst_reg(),
            Op::BOr(op) => op.get_dst_reg(),
            Op::BXor(op) => op.get_dst_reg(),
            Op::LShift(op) => op.get_dst_reg(),
            Op::RShift(op) => op.get_dst_reg(),
            Op::Neg(op) => op.get_dst_reg(),
            Op::Not(op) => op.get_dst_reg(),
            Op::Compare(op) => op.get_dst_reg(),
            Op::Conv(op) => op.get_dst_reg(),
            Op::Store(op) => op.get_dst_reg(),
            Op::Load(op) => op.get_dst_reg(),
            Op::Call(op) => op.get_dst_reg(),
            Op::Memcpy(_) => None,
            Op::IntrinCall(op) => op.get_dst_reg(),
            Op::VaStart(op) => op.get_dst_reg(),
            Op::VaArg(op) => op.get_dst_reg(),
            Op::VaListInc(op) => op.get_dst_reg(),
            Op::ByteSwap(op) => op.get_dst_reg(),
            #[cfg(test)]
            Op::Dummy(_) => None,
        }
    }

    pub fn is_read_from_register(&self, reg: Reg) -> bool {
        match self {
            Op::Undefined(_) => false,
            Op::Arg(_) => false,
            Op::FramePointer(_) => false,
            Op::Copy(op) => op.is_read_from_register(reg),
            Op::Bool(op) => op.is_read_from_register(reg),
            Op::BoolInv(op) => op.is_read_from_register(reg),
            Op::Add(op) => op.is_read_from_register(reg),
            Op::Sub(op) => op.is_read_from_register(reg),
            Op::Mul(op) => op.is_read_from_register(reg),
            Op::Div(op) => op.is_read_from_register(reg),
            Op::Mod(op) => op.is_read_from_register(reg),
            Op::BAnd(op) => op.is_read_from_register(reg),
            Op::BOr(op) => op.is_read_from_register(reg),
            Op::BXor(op) => op.is_read_from_register(reg),
            Op::LShift(op) => op.is_read_from_register(reg),
            Op::RShift(op) => op.is_read_from_register(reg),
            Op::Neg(op) => op.is_read_from_register(reg),
            Op::Not(op) => op.is_read_from_register(reg),
            Op::Compare(op) => op.is_read_from_register(reg),
            Op::Conv(op) => op.is_read_from_register(reg),
            Op::Store(op) => op.is_read_from_register(reg),
            Op::Load(op) => op.is_read_from_register(reg),
            Op::Call(op) => op.is_read_from_register(reg),
            Op::IntrinCall(op) => op.is_read_from_register(reg),
            Op::Memcpy(op) => op.is_read_from_register(reg),
            Op::VaStart(_) => false,
            Op::VaArg(op) => op.is_read_from_register(reg),
            Op::VaListInc(op) => op.is_read_from_register(reg),
            Op::ByteSwap(op) => op.is_read_from_register(reg),
            #[cfg(test)]
            Op::Dummy(_) => false,
        }
    }

    pub fn is_memory_read(&self) -> bool {
        match self {
            Op::Undefined(_) => false,
            Op::Arg(_) => false,
            Op::FramePointer(_) => false,
            Op::Copy(_) => false,
            Op::Bool(_) => false,
            Op::BoolInv(_) => false,
            Op::Add(_) => false,
            Op::Sub(_) => false,
            Op::Mul(_) => false,
            Op::Div(_) => false,
            Op::Mod(_) => false,
            Op::BAnd(_) => false,
            Op::BOr(_) => false,
            Op::BXor(_) => false,
            Op::LShift(_) => false,
            Op::RShift(_) => false,
            Op::Neg(_) => false,
            Op::Not(_) => false,
            Op::Compare(_) => false,
            Op::Conv(_) => false,
            Op::Store(_) => false,
            Op::Load(_) => true,
            Op::Call(_) => true,
            Op::IntrinCall(_) => true,
            Op::Memcpy(_) => true,
            Op::VaStart(_) => false,
            Op::VaArg(_) => false,
            Op::VaListInc(_) => false,
            Op::ByteSwap(_) => false,
            #[cfg(test)]
            Op::Dummy(_) => false,
        }
    }

    pub fn is_memory_write(&self) -> bool {
        match self {
            Op::Undefined(_) => false,
            Op::Arg(_) => false,
            Op::FramePointer(_) => false,
            Op::Copy(_) => false,
            Op::Bool(_) => false,
            Op::BoolInv(_) => false,
            Op::Add(_) => false,
            Op::Sub(_) => false,
            Op::Mul(_) => false,
            Op::Div(_) => false,
            Op::Mod(_) => false,
            Op::BAnd(_) => false,
            Op::BOr(_) => false,
            Op::BXor(_) => false,
            Op::LShift(_) => false,
            Op::RShift(_) => false,
            Op::Neg(_) => false,
            Op::Not(_) => false,
            Op::Compare(_) => false,
            Op::Conv(_) => false,
            Op::Store(_) => true,
            Op::Load(_) => false,
            Op::Call(_) => true,
            Op::IntrinCall(_) => true,
            Op::Memcpy(_) => true,
            Op::VaStart(_) => false,
            Op::VaArg(_) => false,
            Op::VaListInc(_) => false,
            Op::ByteSwap(_) => false,
            #[cfg(test)]
            Op::Dummy(_) => false,
        }
    }

    pub fn get_dst_data_width(&self) -> Option<Width> {
        match self {
            Op::Undefined(_) => Some(Width::Qword),
            Op::Arg(op) => Some(op.width),
            Op::FramePointer(_) => Some(Width::PTR_WIDTH),
            Op::Copy(op) => Some(op.width),
            Op::Bool(_) => Some(Width::Byte),
            Op::BoolInv(_) => Some(Width::Byte),
            Op::Add(op) => Some(op.width),
            Op::Sub(op) => Some(op.width),
            Op::Mul(op) => Some(op.width),
            Op::Div(op) => Some(op.width),
            Op::Mod(op) => Some(op.width),
            Op::BAnd(op) => Some(op.width),
            Op::BOr(op) => Some(op.width),
            Op::BXor(op) => Some(op.width),
            Op::LShift(op) => Some(op.lhs_width),
            Op::RShift(op) => Some(op.lhs_width),
            Op::Neg(op) => Some(op.width),
            Op::Not(op) => Some(op.width),
            Op::Compare(op) => Some(op.dst_width),
            Op::Conv(op) => Some(op.dst_width),
            Op::Store(_) => None,
            Op::Load(op) => Some(op.width),
            Op::Call(op) => op.dst.as_ref().map(|(_, w)| w).copied(),
            Op::IntrinCall(op) => op.get_dst_data_width(),
            Op::Memcpy(_) => None,
            Op::VaStart(_) => Some(Width::VA_LIST_WIDTH),
            Op::VaArg(op) => Some(op.width),
            Op::VaListInc(_) => Some(Width::VA_LIST_WIDTH),
            Op::ByteSwap(op) => Some(op.width),
            #[cfg(test)]
            Op::Dummy(_) => None,
        }
    }

    pub fn get_class(&self) -> stats::InstructionClass {
        use crate::stats::InstructionClass;
        match self {
            Op::Undefined(_) => InstructionClass::Other,
            Op::Arg(_) => InstructionClass::Other,
            Op::FramePointer(_) => InstructionClass::Other,
            Op::Copy(op) => InstructionClass::Copy(op.width),
            Op::Bool(op) => InstructionClass::Bool(op.width),
            Op::BoolInv(op) => InstructionClass::BoolInv(op.width),
            Op::Add(op) => InstructionClass::Add {
                w: op.width,
                has_const: op.lhs.is_const() || op.rhs.is_const(),
            },
            Op::Sub(op) => InstructionClass::Sub {
                w: op.width,
                lhs_const: op.lhs.is_const(),
                rhs_const: op.rhs.is_const(),
            },
            Op::Mul(op) => InstructionClass::Mul {
                w: op.width,
                has_const: op.lhs.is_const() || op.rhs.is_const(),
            },
            Op::Div(op) => InstructionClass::Div {
                w: op.width,
                lhs_const: op.lhs.is_const(),
                rhs_const: op.rhs.is_const(),
            },
            Op::Mod(op) => InstructionClass::Mod {
                w: op.width,
                lhs_const: op.lhs.is_const(),
                rhs_const: op.rhs.is_const(),
            },
            Op::BAnd(op) => InstructionClass::BAnd {
                w: op.width,
                has_const: op.lhs.is_const() || op.rhs.is_const(),
            },
            Op::BOr(op) => InstructionClass::BOr {
                w: op.width,
                has_const: op.lhs.is_const() || op.rhs.is_const(),
            },
            Op::BXor(op) => InstructionClass::BXor {
                w: op.width,
                has_const: op.lhs.is_const() || op.rhs.is_const(),
            },
            Op::LShift(op) => InstructionClass::LShift {
                w: op.lhs_width,
                lhs_const: op.lhs.is_const(),
                rhs_const: op.rhs.is_const(),
            },
            Op::RShift(op) => InstructionClass::RShift {
                w: op.lhs_width,
                lhs_const: op.lhs.is_const(),
                rhs_const: op.rhs.is_const(),
            },
            Op::Neg(op) => InstructionClass::Neg(op.width),
            Op::Not(op) => InstructionClass::Not(op.width),
            Op::Compare(op) => InstructionClass::Compare {
                w: op.desc.width,
                lhs_const: op.desc.lhs.is_const(),
                rhs_const: op.desc.rhs.is_const(),
            },
            Op::Conv(op) => InstructionClass::Conv {
                from: op.src_width,
                to: op.dst_width,
                sign: op.dst_sign,
            },
            Op::Store(op) => InstructionClass::Store {
                addr_const: op.dst_addr.is_const(),
                val_const: op.src.is_const(),
                w: op.width,
            },
            Op::Load(op) => InstructionClass::Load {
                addr_const: op.src_addr.is_const(),
                w: op.width,
            },
            Op::Call(_) => InstructionClass::Call,
            Op::IntrinCall(_) => InstructionClass::IntrinCall,
            Op::Memcpy(_) => InstructionClass::Memcpy,
            Op::VaStart(_) => InstructionClass::Other,
            Op::VaArg(_) => InstructionClass::Other,
            Op::VaListInc(_) => InstructionClass::Other,
            Op::ByteSwap(op) => InstructionClass::ByteSwap(op.width),
            #[cfg(test)]
            Op::Dummy(_) => InstructionClass::Other,
        }
    }
}

impl<Reg: Copy + Eq + Hash> Op<Reg> {
    pub fn collect_read_regs(&self, set: &mut HashSet<Reg>) {
        match self {
            Op::Undefined(_) => (),
            Op::Arg(_) => (),
            Op::FramePointer(_) => (),
            Op::Copy(op) => op.collect_read_regs(set),
            Op::Bool(op) => op.collect_read_regs(set),
            Op::BoolInv(op) => op.collect_read_regs(set),
            Op::Add(op) => op.collect_read_regs(set),
            Op::Sub(op) => op.collect_read_regs(set),
            Op::Mul(op) => op.collect_read_regs(set),
            Op::Div(op) => op.collect_read_regs(set),
            Op::Mod(op) => op.collect_read_regs(set),
            Op::BAnd(op) => op.collect_read_regs(set),
            Op::BOr(op) => op.collect_read_regs(set),
            Op::BXor(op) => op.collect_read_regs(set),
            Op::LShift(op) => op.collect_read_regs(set),
            Op::RShift(op) => op.collect_read_regs(set),
            Op::Neg(op) => op.collect_read_regs(set),
            Op::Not(op) => op.collect_read_regs(set),
            Op::Compare(op) => op.collect_read_regs(set),
            Op::Conv(op) => op.collect_read_regs(set),
            Op::Store(op) => op.collect_read_regs(set),
            Op::Load(op) => op.collect_read_regs(set),
            Op::Call(op) => op.collect_read_regs(set),
            Op::IntrinCall(op) => op.collect_read_regs(set),
            Op::Memcpy(op) => op.collect_read_regs(set),
            Op::VaStart(_) => (),
            Op::VaArg(op) => op.collect_read_regs(set),
            Op::VaListInc(op) => op.collect_read_regs(set),
            Op::ByteSwap(op) => op.collect_read_regs(set),
            #[cfg(test)]
            Op::Dummy(_) => (),
        }
    }

    pub fn collect_set_regs(&self, set: &mut HashSet<Reg>) {
        match self {
            Op::Undefined(reg) | Op::FramePointer(reg) => {
                set.insert(*reg);
            }
            Op::Arg(op) => op.collect_set_regs(set),
            Op::Copy(op) => op.collect_set_regs(set),
            Op::Bool(op) => op.collect_set_regs(set),
            Op::BoolInv(op) => op.collect_set_regs(set),
            Op::Add(op) => op.collect_set_regs(set),
            Op::Sub(op) => op.collect_set_regs(set),
            Op::Mul(op) => op.collect_set_regs(set),
            Op::Div(op) => op.collect_set_regs(set),
            Op::Mod(op) => op.collect_set_regs(set),
            Op::BAnd(op) => op.collect_set_regs(set),
            Op::BOr(op) => op.collect_set_regs(set),
            Op::BXor(op) => op.collect_set_regs(set),
            Op::LShift(op) => op.collect_set_regs(set),
            Op::RShift(op) => op.collect_set_regs(set),
            Op::Neg(op) => op.collect_set_regs(set),
            Op::Not(op) => op.collect_set_regs(set),
            Op::Compare(op) => op.collect_set_regs(set),
            Op::Conv(op) => op.collect_set_regs(set),
            Op::Store(op) => op.collect_set_regs(set),
            Op::Load(op) => op.collect_set_regs(set),
            Op::Call(op) => op.collect_set_regs(set),
            Op::IntrinCall(op) => op.collect_set_regs(set),
            Op::Memcpy(op) => op.collect_set_regs(set),
            Op::VaStart(op) => op.collect_set_regs(set),
            Op::VaArg(op) => op.collect_set_regs(set),
            Op::VaListInc(op) => op.collect_set_regs(set),
            Op::ByteSwap(op) => op.collect_set_regs(set),
            #[cfg(test)]
            Op::Dummy(_) => (),
        }
    }

    /**
     * Remap registers from one type to another using the given HashMap.
     * All registers must be present in the map.
     */
    pub fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> Op<TargetReg> {
        match self {
            Op::Undefined(reg) => {
                let new_reg = map.get(&reg).copied().unwrap();
                Op::Undefined(new_reg)
            }
            Op::Arg(op) => Op::Arg(op.remap_regs(map)),
            Op::FramePointer(reg) => {
                let new_reg = map.get(&reg).copied().unwrap();
                Op::FramePointer(new_reg)
            }
            Op::Copy(op) => Op::Copy(op.remap_regs(map)),
            Op::Bool(op) => Op::Bool(op.remap_regs(map)),
            Op::BoolInv(op) => Op::BoolInv(op.remap_regs(map)),
            Op::Add(op) => Op::Add(op.remap_regs(map)),
            Op::Sub(op) => Op::Sub(op.remap_regs(map)),
            Op::Mul(op) => Op::Mul(op.remap_regs(map)),
            Op::Div(op) => Op::Div(op.remap_regs(map)),
            Op::Mod(op) => Op::Mod(op.remap_regs(map)),
            Op::BAnd(op) => Op::BAnd(op.remap_regs(map)),
            Op::BOr(op) => Op::BOr(op.remap_regs(map)),
            Op::BXor(op) => Op::BXor(op.remap_regs(map)),
            Op::LShift(op) => Op::LShift(op.remap_regs(map)),
            Op::RShift(op) => Op::RShift(op.remap_regs(map)),
            Op::Neg(op) => Op::Neg(op.remap_regs(map)),
            Op::Not(op) => Op::Not(op.remap_regs(map)),
            Op::Compare(op) => Op::Compare(op.remap_regs(map)),
            Op::Conv(op) => Op::Conv(op.remap_regs(map)),
            Op::Store(op) => Op::Store(op.remap_regs(map)),
            Op::Load(op) => Op::Load(op.remap_regs(map)),
            Op::Call(op) => Op::Call(op.remap_regs(map)),
            Op::IntrinCall(op) => Op::IntrinCall(op.remap_regs(map)),
            Op::Memcpy(op) => Op::Memcpy(op.remap_regs(map)),
            Op::VaStart(op) => Op::VaStart(op.remap_regs(map)),
            Op::VaArg(op) => Op::VaArg(op.remap_regs(map)),
            Op::VaListInc(op) => Op::VaListInc(op.remap_regs(map)),
            Op::ByteSwap(op) => Op::ByteSwap(op.remap_regs(map)),
            #[cfg(test)]
            Op::Dummy(x) => Op::Dummy(x),
        }
    }

    /**
     * Remap registers in operation sources according to the map given.
     * Keep old values unchanged if not found in the map.
     *
     * Returns true if any substution has been performed.
     */
    pub fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        match self {
            Op::Undefined(_) => false,
            Op::Arg(_) => false,
            Op::FramePointer(_) => false,
            Op::Copy(op) => op.subs_src_regs(map),
            Op::Bool(op) => op.subs_src_regs(map),
            Op::BoolInv(op) => op.subs_src_regs(map),
            Op::Add(op) => op.subs_src_regs(map),
            Op::Sub(op) => op.subs_src_regs(map),
            Op::Mul(op) => op.subs_src_regs(map),
            Op::Div(op) => op.subs_src_regs(map),
            Op::Mod(op) => op.subs_src_regs(map),
            Op::BAnd(op) => op.subs_src_regs(map),
            Op::BOr(op) => op.subs_src_regs(map),
            Op::BXor(op) => op.subs_src_regs(map),
            Op::LShift(op) => op.subs_src_regs(map),
            Op::RShift(op) => op.subs_src_regs(map),
            Op::Neg(op) => op.subs_src_regs(map),
            Op::Not(op) => op.subs_src_regs(map),
            Op::Compare(op) => op.subs_src_regs(map),
            Op::Conv(op) => op.subs_src_regs(map),
            Op::Store(op) => op.subs_src_regs(map),
            Op::Load(op) => op.subs_src_regs(map),
            Op::Call(op) => op.subs_src_regs(map),
            Op::IntrinCall(op) => op.subs_src_regs(map),
            Op::Memcpy(op) => op.subs_src_regs(map),
            Op::VaStart(_) => false,
            Op::VaArg(op) => op.subs_src_regs(map),
            Op::VaListInc(op) => op.subs_src_regs(map),
            Op::ByteSwap(op) => op.subs_src_regs(map),
            #[cfg(test)]
            Op::Dummy(_) => false,
        }
    }
}

impl Op<VirtualReg> {
    /**
     * Remap registers according to the map.
     * If scope is given, for each assignment to a register create its new version and update the map.
     */
    pub fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        match self {
            Op::Undefined(reg) => {
                let new_reg = scope.alloc_reg();
                map.insert(reg, new_reg);
                Op::Undefined(new_reg)
            }
            Op::FramePointer(reg) => {
                let new_reg = scope.alloc_reg();
                map.insert(reg, new_reg);
                Op::FramePointer(new_reg)
            }
            Op::Arg(op) => Op::Arg(op.remap_regs_to_new_version(map, scope)),
            Op::Copy(op) => Op::Copy(op.remap_regs_to_new_version(map, scope)),
            Op::Bool(op) => Op::Bool(op.remap_regs_to_new_version(map, scope)),
            Op::BoolInv(op) => Op::BoolInv(op.remap_regs_to_new_version(map, scope)),
            Op::Add(op) => Op::Add(op.remap_regs_to_new_version(map, scope)),
            Op::Sub(op) => Op::Sub(op.remap_regs_to_new_version(map, scope)),
            Op::Mul(op) => Op::Mul(op.remap_regs_to_new_version(map, scope)),
            Op::Div(op) => Op::Div(op.remap_regs_to_new_version(map, scope)),
            Op::Mod(op) => Op::Mod(op.remap_regs_to_new_version(map, scope)),
            Op::BAnd(op) => Op::BAnd(op.remap_regs_to_new_version(map, scope)),
            Op::BOr(op) => Op::BOr(op.remap_regs_to_new_version(map, scope)),
            Op::BXor(op) => Op::BXor(op.remap_regs_to_new_version(map, scope)),
            Op::LShift(op) => Op::LShift(op.remap_regs_to_new_version(map, scope)),
            Op::RShift(op) => Op::RShift(op.remap_regs_to_new_version(map, scope)),
            Op::Neg(op) => Op::Neg(op.remap_regs_to_new_version(map, scope)),
            Op::Not(op) => Op::Not(op.remap_regs_to_new_version(map, scope)),
            Op::Compare(op) => Op::Compare(op.remap_regs_to_new_version(map, scope)),
            Op::Conv(op) => Op::Conv(op.remap_regs_to_new_version(map, scope)),
            Op::Store(op) => Op::Store(op.remap_regs(map)),
            Op::Load(op) => Op::Load(op.remap_regs_to_new_version(map, scope)),
            Op::Call(op) => Op::Call(op.remap_regs_to_new_version(map, scope)),
            Op::IntrinCall(op) => Op::IntrinCall(op.remap_regs_to_new_version(map, scope)),
            Op::Memcpy(op) => Op::Memcpy(op.remap_regs(map)),
            Op::VaStart(op) => Op::VaStart(op.remap_regs_to_new_version(map, scope)),
            Op::VaArg(op) => Op::VaArg(op.remap_regs_to_new_version(map, scope)),
            Op::VaListInc(op) => Op::VaListInc(op.remap_regs_to_new_version(map, scope)),
            Op::ByteSwap(op) => Op::ByteSwap(op.remap_regs_to_new_version(map, scope)),
            #[cfg(test)]
            Op::Dummy(x) => Op::Dummy(x),
        }
    }
}

impl<Reg: Copy + Eq> Tail<Reg> {
    pub fn is_read_from_register(&self, reg: Reg) -> bool {
        match self {
            Tail::Jump(_) | Tail::Ret => false,
            Tail::Cond(c, _, _) => c.is_read_from_register(reg),
            Tail::Switch(c, _, _, _) => c.is_reg(reg),
        }
    }

    pub fn get_connections(&self) -> Vec<usize> {
        let mut to_visit = Vec::new();
        match self {
            Tail::Ret => (),
            Tail::Jump(n) => to_visit.push(*n),
            Tail::Cond(_, n, m) => {
                to_visit.push(*n);
                to_visit.push(*m);
            }
            Tail::Switch(_, _, cases, default) => {
                for (_, n) in cases.iter() {
                    to_visit.push(*n);
                }
                to_visit.push(*default);
            }
        };
        to_visit
    }

    pub fn replace_block_id(&mut self, old_id: usize, new_id: usize) {
        match self {
            Tail::Ret => (),
            Tail::Jump(n) => {
                if *n == old_id {
                    *n = new_id;
                }
            }
            Tail::Cond(_, a, b) => {
                if *a == old_id {
                    *a = new_id;
                }
                if *b == old_id {
                    *b = new_id;
                }
            }
            Tail::Switch(_, _, cases, default) => {
                for (_, id) in cases.iter_mut() {
                    if *id == old_id {
                        *id = new_id;
                    }
                }
                if *default == old_id {
                    *default = new_id;
                }
            }
        }
    }

    pub fn is_return(&self) -> bool {
        match self {
            Tail::Ret => true,
            _ => false,
        }
    }
}

impl<Reg: Copy + Eq + Hash> Tail<Reg> {
    pub fn collect_read_regs(&self, set: &mut HashSet<Reg>) {
        match self {
            Tail::Jump(_) | Tail::Ret => (),
            Tail::Cond(c, _, _) => c.collect_read_regs(set),
            Tail::Switch(c, _, _, _) => c.collect_regs(set),
        }
    }

    pub fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> Tail<TargetReg> {
        match self {
            Tail::Jump(n) => Tail::Jump(n),
            Tail::Ret => Tail::Ret,
            Tail::Cond(c, i, e) => Tail::Cond(c.remap_regs(map), i, e),
            Tail::Switch(c, w, s, d) => Tail::Switch(c.remap_reg(map), w, s, d),
        }
    }

    pub fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        match self {
            Tail::Cond(c, _, _) => c.subs_src_regs(map),
            Tail::Switch(c, _, _, _) => c.subs_reg(map),
            Tail::Ret => false,
            Tail::Jump(_) => false,
        }
    }
}

impl<Reg: Copy + Eq> JumpCondition<Reg> {
    pub fn is_read_from_register(&self, reg: Reg) -> bool {
        match self {
            JumpCondition::StrictBool(s) | JumpCondition::RelaxedBool(s, _) => s.is_reg(reg),
            JumpCondition::Compare(op) => op.is_read_from_register(reg),
        }
    }
}

impl<Reg: Copy + Eq + Hash> JumpCondition<Reg> {
    pub fn collect_read_regs(&self, set: &mut HashSet<Reg>) {
        match self {
            JumpCondition::StrictBool(s) => s.collect_regs(set),
            JumpCondition::RelaxedBool(s, _) => s.collect_regs(set),
            JumpCondition::Compare(op) => op.collect_read_regs(set),
        }
    }

    pub fn remap_regs<TargetReg: Copy>(
        self,
        map: &HashMap<Reg, TargetReg>,
    ) -> JumpCondition<TargetReg> {
        match self {
            JumpCondition::StrictBool(s) => JumpCondition::StrictBool(s.remap_reg(map)),
            JumpCondition::RelaxedBool(s, w) => JumpCondition::RelaxedBool(s.remap_reg(map), w),
            JumpCondition::Compare(op) => JumpCondition::Compare(op.remap_regs(map)),
        }
    }

    pub fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        match self {
            JumpCondition::StrictBool(s) => s.subs_reg(map),
            JumpCondition::RelaxedBool(s, _) => s.subs_reg(map),
            JumpCondition::Compare(op) => op.subs_src_regs(map),
        }
    }
}

impl<Reg: Copy + Eq + Hash> Phi<Reg> {
    pub fn new() -> Self {
        Self {
            srcs: HashMap::new(),
        }
    }

    pub fn add_binding(&mut self, dst: &Reg, src: &Reg, block: usize, width: Width) {
        if let Some((old_width, l)) = self.srcs.get_mut(dst) {
            assert_eq!(*old_width, width);
            l.push((block, Scalar::Var(VarLocation::Local(*src))));
        } else {
            self.srcs.insert(
                *dst,
                (width, vec![(block, Scalar::Var(VarLocation::Local(*src)))]),
            );
        }
    }

    pub fn with_adjusted_block_ids(self, offsets: &Vec<usize>) -> Self {
        let srcs = self
            .srcs
            .into_iter()
            .map(|(k, (w, v))| {
                (
                    k,
                    (
                        w,
                        v.into_iter()
                            .map(|(block_id, var)| (block_id - offsets[block_id], var))
                            .collect(),
                    ),
                )
            })
            .collect();
        Self { srcs }
    }

    pub fn is_empty(&self) -> bool {
        self.srcs.is_empty()
    }

    pub fn delete_dsts_from_set(&mut self, set: &HashSet<Reg>) -> bool {
        let old_srcs = mem::replace(&mut self.srcs, HashMap::new());
        let old_len = old_srcs.len();
        self.srcs = old_srcs
            .into_iter()
            .filter(|(dst, _)| !set.contains(dst))
            .collect();
        old_len != self.srcs.len()
    }

    pub fn remap_regs<TargetReg: Copy + Eq + Hash>(
        self,
        map: &HashMap<Reg, TargetReg>,
    ) -> Phi<TargetReg> {
        Phi {
            srcs: self
                .srcs
                .into_iter()
                .map(|(dst, (w, srcs))| {
                    (
                        map.get(&dst).copied().unwrap(),
                        (
                            w,
                            srcs.into_iter()
                                .map(|(i, scalar)| (i, scalar.remap_reg(map)))
                                .collect(),
                        ),
                    )
                })
                .collect(),
        }
    }

    pub fn collect_set_regs(&self, set: &mut HashSet<Reg>) {
        for (dst, _) in self.srcs.iter() {
            set.insert(*dst);
        }
    }

    pub fn collect_read_regs(&self, set: &mut HashSet<Reg>) {
        for (_, (_, src)) in self.srcs.iter() {
            for (_, val) in src {
                val.collect_regs(set);
            }
        }
    }

    pub fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        let mut result = false;
        for (_, (_, src)) in &mut self.srcs {
            for (_, src) in src {
                result |= src.subs_reg(map);
            }
        }
        result
    }

    pub fn subs_block_id(&mut self, old: usize, new: usize) -> bool {
        let mut result = false;
        for (_, (_, src)) in &mut self.srcs {
            for (block_id, _) in src {
                if *block_id == old {
                    *block_id = new;
                    result = true;
                }
            }
        }
        result
    }

    pub fn delete_srcs_from_set(&mut self, set: &HashSet<usize>) {
        for (_, (_, src)) in &mut self.srcs {
            src.retain(|(block_id, _)| !set.contains(block_id));
        }
    }
}

impl<Reg: Copy + Eq> ArgOp<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        Some(self.dst_reg)
    }
}

impl<Reg: Copy + Eq + Hash> ArgOp<Reg> {
    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        regs.insert(self.dst_reg);
    }

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> ArgOp<TargetReg> {
        let new_dst_reg = *map.get(&self.dst_reg).unwrap();
        ArgOp {
            dst_reg: new_dst_reg,
            arg_number: self.arg_number,
            width: self.width,
        }
    }
}

impl ArgOp<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        let new_dst_reg = if let Some(new_reg) = map.get(&self.dst_reg) {
            *new_reg
        } else {
            let new_reg = scope.alloc_reg();
            map.insert(self.dst_reg, new_reg);
            new_reg
        };
        Self {
            dst_reg: new_dst_reg,
            ..self
        }
    }
}

impl<Reg: Copy + Eq> CompareOp<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.desc.is_read_from_register(reg)
    }
}

impl<Reg: Copy + Eq + Hash> CompareOp<Reg> {
    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.desc.collect_read_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> CompareOp<TargetReg> {
        CompareOp {
            desc: self.desc.remap_regs(map),
            dst: self.dst.remap_reg(map),
            dst_width: self.dst_width,
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        self.desc.subs_src_regs(map)
    }
}

impl CompareOp<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        Self {
            desc: self.desc.remap_regs(map),
            dst: self.dst.remap_reg_to_new_version(map, scope),
            ..self
        }
    }
}

impl<Reg: Copy + Eq> CompareDesc<Reg> {
    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.lhs.is_reg(reg) | self.rhs.is_reg(reg)
    }
}

impl<Reg: Copy + Eq + Hash> CompareDesc<Reg> {
    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.lhs.collect_regs(regs);
        self.rhs.collect_regs(regs);
    }

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> CompareDesc<TargetReg> {
        CompareDesc {
            lhs: self.lhs.remap_reg(map),
            rhs: self.rhs.remap_reg(map),
            kind: self.kind,
            width: self.width,
            sign: self.sign,
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        self.lhs.subs_reg(map) | self.rhs.subs_reg(map)
    }
}

impl<Reg: Copy + Eq> UnaryUnsignedOp<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.src.is_reg(reg)
    }
}

impl<Reg: Copy + Eq + Hash> UnaryUnsignedOp<Reg> {
    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.src.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs<TargetReg: Copy>(
        self,
        map: &HashMap<Reg, TargetReg>,
    ) -> UnaryUnsignedOp<TargetReg> {
        UnaryUnsignedOp {
            src: self.src.remap_reg(map),
            dst: self.dst.remap_reg(map),
            width: self.width,
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        self.src.subs_reg(map)
    }
}

impl UnaryUnsignedOp<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        Self {
            src: self.src.remap_reg(map),
            dst: self.dst.remap_reg_to_new_version(map, scope),
            ..self
        }
    }
}

impl<Reg: Copy + Eq> BinaryOp<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.rhs.is_reg(reg) || self.lhs.is_reg(reg)
    }
}

impl<Reg: Copy + Eq + Hash> BinaryOp<Reg> {
    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.lhs.collect_regs(regs);
        self.rhs.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> BinaryOp<TargetReg> {
        BinaryOp {
            lhs: self.lhs.remap_reg(map),
            rhs: self.rhs.remap_reg(map),
            dst: self.dst.remap_reg(map),
            width: self.width,
            sign: self.sign,
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        self.lhs.subs_reg(map) | self.rhs.subs_reg(map)
    }
}

impl BinaryOp<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        Self {
            lhs: self.lhs.remap_reg(map),
            rhs: self.rhs.remap_reg(map),
            dst: self.dst.remap_reg_to_new_version(map, scope),
            ..self
        }
    }
}

impl<Reg: Copy + Eq> BinaryUnsignedOp<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.rhs.is_reg(reg) || self.lhs.is_reg(reg)
    }
}

impl<Reg: Copy + Eq + Hash> BinaryUnsignedOp<Reg> {
    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.lhs.collect_regs(regs);
        self.rhs.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs<TargetReg: Copy>(
        self,
        map: &HashMap<Reg, TargetReg>,
    ) -> BinaryUnsignedOp<TargetReg> {
        BinaryUnsignedOp {
            lhs: self.lhs.remap_reg(map),
            rhs: self.rhs.remap_reg(map),
            dst: self.dst.remap_reg(map),
            width: self.width,
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        self.lhs.subs_reg(map) | self.rhs.subs_reg(map)
    }
}

impl BinaryUnsignedOp<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        Self {
            lhs: self.lhs.remap_reg(map),
            rhs: self.rhs.remap_reg(map),
            dst: self.dst.remap_reg_to_new_version(map, scope),
            ..self
        }
    }
}

impl<Reg: Copy + Eq> ShiftOp<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.rhs.is_reg(reg) || self.lhs.is_reg(reg)
    }
}

impl<Reg: Copy + Eq + Hash> ShiftOp<Reg> {
    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.lhs.collect_regs(regs);
        self.rhs.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> ShiftOp<TargetReg> {
        ShiftOp {
            lhs: self.lhs.remap_reg(map),
            rhs: self.rhs.remap_reg(map),
            dst: self.dst.remap_reg(map),
            lhs_width: self.lhs_width,
            lhs_sign: self.lhs_sign,
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        self.lhs.subs_reg(map) | self.rhs.subs_reg(map)
    }
}

impl ShiftOp<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        Self {
            lhs: self.lhs.remap_reg(map),
            rhs: self.rhs.remap_reg(map),
            dst: self.dst.remap_reg_to_new_version(map, scope),
            ..self
        }
    }
}

impl<Reg: Copy + Eq> ConvOp<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.src.is_reg(reg)
    }
}

impl<Reg: Copy + Eq + Hash> ConvOp<Reg> {
    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.src.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> ConvOp<TargetReg> {
        ConvOp {
            src: self.src.remap_reg(map),
            dst: self.dst.remap_reg(map),
            src_width: self.src_width,
            src_sign: self.src_sign,
            dst_width: self.dst_width,
            dst_sign: self.dst_sign,
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        self.src.subs_reg(map)
    }
}

impl ConvOp<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        Self {
            src: self.src.remap_reg(map),
            dst: self.dst.remap_reg_to_new_version(map, scope),
            ..self
        }
    }
}

impl<Reg: Copy + Eq> StoreOp<Reg> {
    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.src.is_reg(reg) || self.dst_addr.is_reg(reg)
    }

    fn get_dst_reg(&self) -> Option<Reg> {
        None
    }
}

impl<Reg: Copy + Eq + Hash> StoreOp<Reg> {
    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.src.collect_regs(regs);
        self.dst_addr.collect_regs(regs);
    }

    fn collect_set_regs(&self, _regs: &mut HashSet<Reg>) {}

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> StoreOp<TargetReg> {
        StoreOp {
            src: self.src.remap_reg(map),
            dst_addr: self.dst_addr.remap_reg(map),
            width: self.width,
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        self.src.subs_reg(map) | self.dst_addr.subs_reg(map)
    }
}

impl<Reg: Copy + Eq> LoadOp<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.src_addr.is_reg(reg)
    }
}

impl<Reg: Copy + Eq + Hash> LoadOp<Reg> {
    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.src_addr.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> LoadOp<TargetReg> {
        LoadOp {
            src_addr: self.src_addr.remap_reg(map),
            dst: self.dst.remap_reg(map),
            width: self.width,
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        self.src_addr.subs_reg(map)
    }
}

impl LoadOp<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        Self {
            src_addr: self.src_addr.remap_reg(map),
            dst: self.dst.remap_reg_to_new_version(map, scope),
            ..self
        }
    }
}

impl<Reg: Copy + Eq> CallOp<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.as_ref().map(|(dst, _)| dst.get_reg()).flatten()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        if self.addr.is_reg(reg) {
            return true;
        }
        for (a, _) in &self.args {
            if a.is_reg(reg) {
                return true;
            }
        }
        for (a, _) in &self.va_args {
            if a.is_reg(reg) {
                return true;
            }
        }
        false
    }
}

impl<Reg: Copy + Eq + Hash> CallOp<Reg> {
    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        for (a, _) in &self.args {
            a.collect_regs(regs);
        }

        for (a, _) in &self.va_args {
            a.collect_regs(regs);
        }

        self.addr.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.as_ref().map(|(dst, _)| dst.collect_regs(regs));
    }

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> CallOp<TargetReg> {
        CallOp {
            args: self
                .args
                .into_iter()
                .map(|(a, w)| (a.remap_reg(map), w))
                .collect(),
            va_args: self
                .va_args
                .into_iter()
                .map(|(a, w)| (a.remap_reg(map), w))
                .collect(),

            dst: self.dst.map(|(dst, w)| (dst.remap_reg(map), w)),
            addr: self.addr.remap_reg(map),
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        let mut result = self.addr.subs_reg(map);
        for (arg, _) in &mut self.args {
            result |= arg.subs_reg(map);
        }
        for (arg, _) in &mut self.va_args {
            result |= arg.subs_reg(map);
        }
        result
    }
}

impl CallOp<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        Self {
            args: self
                .args
                .into_iter()
                .map(|(a, w)| (a.remap_reg(map), w))
                .collect(),

            va_args: self
                .va_args
                .into_iter()
                .map(|(a, w)| (a.remap_reg(map), w))
                .collect(),

            dst: self
                .dst
                .map(|(dst, w)| (dst.remap_reg_to_new_version(map, scope), w)),

            addr: self.addr.remap_reg(map),
        }
    }
}

impl<Reg: Copy + Eq> IntrinCallOp<Reg> {
    pub fn foreach_arg_mut<F: FnMut(&mut Width, &mut Scalar<Reg>)>(&mut self, mut f: F) {
        match &mut self.variant {
            IntrinCallVariant::Call2R(_, (a1w, a1s), (a2w, a2s)) => {
                f(a1w, a1s);
                f(a2w, a2s);
            }
            IntrinCallVariant::Call3((a1w, a1s), (a2w, a2s), (a3w, a3s)) => {
                f(a1w, a1s);
                f(a2w, a2s);
                f(a3w, a3s);
            }
        }
    }

    pub fn get_result_location(&self) -> Option<(&Width, &VarLocation<Reg>)> {
        match &self.variant {
            IntrinCallVariant::Call2R((rw, rl), _, _) => Some((rw, rl)),
            IntrinCallVariant::Call3(_, _, _) => None,
        }
    }
}

impl<Reg: Copy + Eq> IntrinCallOp<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.variant.get_dst_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.variant.is_read_from_register(reg)
    }

    fn get_dst_data_width(&self) -> Option<Width> {
        self.variant.get_dst_data_width()
    }
}

impl<Reg: Copy + Eq + Hash> IntrinCallOp<Reg> {
    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.variant.collect_read_regs(regs)
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.variant.collect_set_regs(regs)
    }

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> IntrinCallOp<TargetReg> {
        IntrinCallOp {
            name: self.name,
            variant: self.variant.remap_regs(map),
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        self.variant.subs_src_regs(map)
    }
}

impl IntrinCallOp<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        Self {
            name: self.name,
            variant: self.variant.remap_regs_to_new_version(map, scope),
        }
    }
}

impl<Reg: Copy + Eq> IntrinCallVariant<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        match self {
            IntrinCallVariant::Call2R((_, dst), _, _) => dst.get_reg(),
            IntrinCallVariant::Call3(_, _, _) => None,
        }
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        match self {
            IntrinCallVariant::Call2R(_, (_, a1), (_, a2)) => a1.is_reg(reg) || a2.is_reg(reg),
            IntrinCallVariant::Call3((_, a1), (_, a2), (_, a3)) => {
                a1.is_reg(reg) || a2.is_reg(reg) || a3.is_reg(reg)
            }
        }
    }

    fn get_dst_data_width(&self) -> Option<Width> {
        match self {
            IntrinCallVariant::Call2R((rw, _), _, _) => Some(*rw),
            IntrinCallVariant::Call3(_, _, _) => None,
        }
    }
}

impl<Reg: Copy + Eq + Hash> IntrinCallVariant<Reg> {
    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        match self {
            IntrinCallVariant::Call2R(_, (_, a1), (_, a2)) => {
                a1.collect_regs(regs);
                a2.collect_regs(regs);
            }
            IntrinCallVariant::Call3((_, a1), (_, a2), (_, a3)) => {
                a1.collect_regs(regs);
                a2.collect_regs(regs);
                a3.collect_regs(regs);
            }
        }
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        match self {
            IntrinCallVariant::Call2R((_, r), _, _) => r.collect_regs(regs),
            IntrinCallVariant::Call3(_, _, _) => (),
        }
    }

    fn remap_regs<TargetReg: Copy>(
        self,
        map: &HashMap<Reg, TargetReg>,
    ) -> IntrinCallVariant<TargetReg> {
        match self {
            IntrinCallVariant::Call2R((rw, rr), (a1w, a1r), (a2w, a2r)) => {
                IntrinCallVariant::Call2R(
                    (rw, rr.remap_reg(map)),
                    (a1w, a1r.remap_reg(map)),
                    (a2w, a2r.remap_reg(map)),
                )
            }
            IntrinCallVariant::Call3((a1w, a1r), (a2w, a2r), (a3w, a3r)) => {
                IntrinCallVariant::Call3(
                    (a1w, a1r.remap_reg(map)),
                    (a2w, a2r.remap_reg(map)),
                    (a3w, a3r.remap_reg(map)),
                )
            }
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        match self {
            IntrinCallVariant::Call2R(_, (_, a1), (_, a2)) => a1.subs_reg(map) | a2.subs_reg(map),
            IntrinCallVariant::Call3((_, a1), (_, a2), (_, a3)) => {
                a1.subs_reg(map) | a2.subs_reg(map) | a3.subs_reg(map)
            }
        }
    }
}

impl IntrinCallVariant<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        match self {
            IntrinCallVariant::Call2R((rw, rr), (a1w, a1r), (a2w, a2r)) => {
                IntrinCallVariant::Call2R(
                    (rw, rr.remap_reg_to_new_version(map, scope)),
                    (a1w, a1r.remap_reg(map)),
                    (a2w, a2r.remap_reg(map)),
                )
            }
            IntrinCallVariant::Call3((a1w, a1r), (a2w, a2r), (a3w, a3r)) => {
                IntrinCallVariant::Call3(
                    (a1w, a1r.remap_reg(map)),
                    (a2w, a2r.remap_reg(map)),
                    (a3w, a3r.remap_reg(map)),
                )
            }
        }
    }
}

impl<Reg: Copy + Eq> MemcpyOp<Reg> {
    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.src_addr.is_reg(reg) || self.dst_addr.is_reg(reg)
    }
}

impl<Reg: Copy + Eq + Hash> MemcpyOp<Reg> {
    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.src_addr.collect_regs(regs);
        self.dst_addr.collect_regs(regs);
    }

    fn collect_set_regs(&self, _regs: &mut HashSet<Reg>) {}

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> MemcpyOp<TargetReg> {
        MemcpyOp {
            src_addr: self.src_addr.remap_reg(map),
            dst_addr: self.dst_addr.remap_reg(map),
            len: self.len,
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        self.dst_addr.subs_reg(map) | self.src_addr.subs_reg(map)
    }
}

impl<Reg: Copy + Eq> VaStartOp<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }
}

impl<Reg: Copy + Eq + Hash> VaStartOp<Reg> {
    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> VaStartOp<TargetReg> {
        VaStartOp {
            dst: self.dst.remap_reg(map),
        }
    }
}

impl VaStartOp<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        Self {
            dst: self.dst.remap_reg_to_new_version(map, scope),
        }
    }
}

impl<Reg: Copy + Eq> VaArgOp<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    pub fn is_read_from_register(&self, reg: Reg) -> bool {
        self.src_va_list.is_reg(reg)
    }
}

impl<Reg: Copy + Eq + Hash> VaArgOp<Reg> {
    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.src_va_list.collect_regs(regs);
    }

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> VaArgOp<TargetReg> {
        VaArgOp {
            dst: self.dst.remap_reg(map),
            src_va_list: self.src_va_list.remap_reg(map),
            width: self.width,
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        self.src_va_list.subs_reg(map)
    }
}

impl VaArgOp<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        Self {
            dst: self.dst.remap_reg_to_new_version(map, scope),
            src_va_list: self.src_va_list.remap_reg(map),
            width: self.width,
        }
    }
}

impl<Reg: Copy + Eq> VaListIncOp<Reg> {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    pub fn is_read_from_register(&self, reg: Reg) -> bool {
        self.src.is_reg(reg)
    }
}

impl<Reg: Copy + Eq + Hash> VaListIncOp<Reg> {
    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.src.collect_regs(regs);
    }

    fn remap_regs<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> VaListIncOp<TargetReg> {
        VaListIncOp {
            dst: self.dst.remap_reg(map),
            src: self.src.remap_reg(map),
        }
    }

    fn subs_src_regs(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        self.src.subs_reg(map)
    }
}

impl VaListIncOp<VirtualReg> {
    fn remap_regs_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        Self {
            dst: self.dst.remap_reg_to_new_version(map, scope),
            src: self.src.remap_reg(map),
        }
    }
}

impl<Reg: Copy + Eq> VarLocation<Reg> {
    pub fn unwrap_reg(self) -> Reg {
        if let VarLocation::Local(x) = self {
            x
        } else {
            panic!("not a register");
        }
    }

    pub fn get_reg(&self) -> Option<Reg> {
        if let VarLocation::Local(x) = self {
            Some(*x)
        } else {
            None
        }
    }
}

impl<Reg: Copy + Eq> VarLocation<Reg> {
    fn is_reg(&self, reg: Reg) -> bool {
        if let VarLocation::Local(x) = self {
            *x == reg
        } else {
            false
        }
    }
}

impl<Reg: Copy + Eq + Hash> VarLocation<Reg> {
    fn collect_regs(&self, regs: &mut HashSet<Reg>) {
        if let VarLocation::Local(n) = self {
            regs.insert(*n);
        }
    }

    fn remap_reg<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> VarLocation<TargetReg> {
        match self {
            VarLocation::Local(n) => VarLocation::Local(map.get(&n).copied().unwrap()),
            VarLocation::Global(x) => VarLocation::Global(x),
            VarLocation::Return => VarLocation::Return,
        }
    }
}

impl VarLocation<VirtualReg> {
    /**
     * Allocate a new register, remap this location to its value and update the map.
     */
    fn remap_reg_to_new_version(
        self,
        map: &mut HashMap<VirtualReg, VirtualReg>,
        scope: &mut NameScope,
    ) -> Self {
        if let VarLocation::Local(n) = self {
            let reg = scope.alloc_reg();
            map.insert(n, reg);
            VarLocation::Local(reg)
        } else {
            self
        }
    }
}

impl<Reg: Copy + Eq> Scalar<Reg> {
    pub fn unwrap_var(self) -> VarLocation<Reg> {
        if let Scalar::Var(v) = self {
            v
        } else {
            panic!("not a var");
        }
    }

    pub fn get_reg(&self) -> Option<Reg> {
        if let Scalar::Var(VarLocation::Local(r)) = self {
            Some(*r)
        } else {
            None
        }
    }

    fn is_reg(&self, reg: Reg) -> bool {
        if let Scalar::Var(x) = self {
            x.is_reg(reg)
        } else {
            false
        }
    }

    fn is_const(&self) -> bool {
        if let Self::Var(_) = self {
            false
        } else {
            true
        }
    }
}

impl<Reg: Copy + Eq + Hash> Scalar<Reg> {
    fn collect_regs(&self, regs: &mut HashSet<Reg>) {
        if let Scalar::Var(x) = self {
            x.collect_regs(regs);
        }
    }

    fn remap_reg<TargetReg: Copy>(self, map: &HashMap<Reg, TargetReg>) -> Scalar<TargetReg> {
        match self {
            Scalar::Var(v) => Scalar::Var(v.remap_reg(map)),
            Scalar::ConstInt(n) => Scalar::ConstInt(n),
            Scalar::SymbolOffset(s, o) => Scalar::SymbolOffset(s, o),
        }
    }

    pub fn subs_reg(&mut self, map: &HashMap<Reg, Scalar<Reg>>) -> bool {
        if let Scalar::Var(v) = self {
            if let Some(reg) = v.get_reg() {
                if let Some(s) = map.get(&reg) {
                    *self = s.clone();
                    return true;
                }
            }
        }
        false
    }
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
    pub const PTR_WIDTH: Self = Self::new(machine::PTR_SIZE);
    pub const BOOL_WIDTH: Self = Self::new(machine::BOOL_SIZE);
    pub const VA_LIST_WIDTH: Self = Self::new(machine::VA_LIST_SIZE);
}

impl PartialOrd for Width {
    fn partial_cmp(&self, other: &Width) -> Option<std::cmp::Ordering> {
        let self_u8 = (*self) as u8;
        let other_u8 = (*other) as u8;
        self_u8.partial_cmp(&other_u8)
    }
}

impl<GTail, Reg> std::fmt::Display for GenericBlock<GTail, Reg>
where
    GTail: std::fmt::Display,
    Reg: std::fmt::Display + Eq + Hash,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.phi)?;
        for op in &self.ops {
            writeln!(f, "{}", op)?;
        }
        writeln!(f, "{}", self.tail)
    }
}

impl<Reg> std::fmt::Display for Phi<Reg>
where
    Reg: std::fmt::Display + Eq + Hash,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        for (dst, srcs) in self.srcs.iter() {
            write!(f, "phi{} %{} = ", srcs.0, dst)?;
            for s in srcs
                .1
                .iter()
                .map(|(n, r)| format!("{} -> {}", n, r))
                .intersperse(", ".to_string())
            {
                f.write_str(&s)?;
            }
            f.write_str("\n")?;
        }
        Ok(())
    }
}

impl<Reg> std::fmt::Display for Op<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Undefined(reg) => write!(f, "undef %{}", reg),
            Self::Arg(op) => write!(f, "arg{}", op),
            Self::FramePointer(reg) => write!(f, "fp %{}", reg),
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
            Self::IntrinCall(op) => write!(f, "intrin{}", op),
            Self::Memcpy(op) => write!(f, "memcpy{}", op),
            Self::VaStart(op) => write!(f, "va_start{}", op),
            Self::VaArg(op) => write!(f, "va_arg{}", op),
            Self::VaListInc(op) => write!(f, "va_inc{}", op),
            Self::ByteSwap(op) => write!(f, "bswap{}", op),
            #[cfg(test)]
            Self::Dummy(n) => write!(f, "dummy {}", n),
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

impl CompareKind {
    pub fn flip(self) -> Self {
        use CompareKind::*;
        match self {
            Equal => Equal,
            NotEqual => NotEqual,
            LessThan => GreaterThan,
            LessOrEqual => GreaterOrEqual,
            GreaterThan => LessThan,
            GreaterOrEqual => LessOrEqual,
        }
    }

    pub fn inverse(self) -> Self {
        use CompareKind::*;
        match self {
            Equal => NotEqual,
            NotEqual => Equal,
            LessThan => GreaterOrEqual,
            LessOrEqual => GreaterThan,
            GreaterThan => LessOrEqual,
            GreaterOrEqual => LessThan,
        }
    }
}

impl<Reg> std::fmt::Display for ArgOp<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} %{}, {}", self.width, self.dst_reg, self.arg_number,)
    }
}

impl<Reg> std::fmt::Display for BinaryOp<Reg>
where
    Reg: std::fmt::Display,
{
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

impl<Reg> std::fmt::Display for CompareOp<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}{}{}{} {}, {}, {}",
            self.desc.kind,
            self.dst_width,
            get_sign_char(self.desc.sign),
            self.desc.width,
            self.dst,
            self.desc.lhs,
            self.desc.rhs
        )
    }
}

impl<Reg> std::fmt::Display for BinaryUnsignedOp<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {}, {}, {}", self.width, self.dst, self.lhs, self.rhs)
    }
}

impl<Reg> std::fmt::Display for UnaryUnsignedOp<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {}, {}", self.width, self.dst, self.src)
    }
}

impl<Reg> std::fmt::Display for ShiftOp<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}{} {}, {}, {}",
            get_sign_char(self.lhs_sign),
            self.lhs_width,
            self.dst,
            self.lhs,
            self.rhs
        )
    }
}

impl<Reg> std::fmt::Display for StoreOp<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} [{}], {}", self.width, self.dst_addr, self.src)
    }
}

impl<Reg> std::fmt::Display for LoadOp<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {}, [{}]", self.width, self.dst, self.src_addr)
    }
}

impl<Reg> std::fmt::Display for CallOp<Reg>
where
    Reg: std::fmt::Display,
{
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
        if !self.va_args.is_empty() {
            f.write_str(" ... ")?;
            for s in self
                .va_args
                .iter()
                .map(|(s, w)| format!("{} {}", w, s))
                .intersperse(", ".to_string())
            {
                f.write_str(&s)?;
            }
        }
        f.write_str(")")?;
        Ok(())
    }
}

impl<Reg> std::fmt::Display for ConvOp<Reg>
where
    Reg: std::fmt::Display,
{
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

impl<Reg> std::fmt::Display for MemcpyOp<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, " [{}], [{}], {}", self.dst_addr, self.src_addr, self.len)
    }
}

impl<Reg> std::fmt::Display for IntrinCallOp<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, " {}{}", self.name, self.variant)
    }
}

impl<Reg> std::fmt::Display for IntrinCallVariant<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            IntrinCallVariant::Call2R((rw, rr), (a1w, a1r), (a2w, a2r)) => {
                write!(f, "{} {}, {} {}, {} {}", rw, rr, a1w, a1r, a2w, a2r)
            }
            IntrinCallVariant::Call3((a1w, a1r), (a2w, a2r), (a3w, a3r)) => {
                write!(f, " {} {}, {} {}, {} {}", a1w, a1r, a2w, a2r, a3w, a3r)
            }
        }
    }
}

impl<Reg> std::fmt::Display for VaStartOp<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, " {}", self.dst)
    }
}

impl<Reg> std::fmt::Display for VaArgOp<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {}, {}", self.width, self.dst, self.src_va_list)
    }
}

impl<Reg> std::fmt::Display for VaListIncOp<Reg>
where
    Reg: std::fmt::Display,
{
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

impl<Reg> std::fmt::Display for Scalar<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Scalar::ConstInt(x) => write!(f, "{}", x),
            Scalar::SymbolOffset(sym, 0) => write!(f, "{}", sym),
            Scalar::SymbolOffset(sym, offs) => write!(f, "{}+0x{:x}", sym, offs),
            Scalar::Var(v) => write!(f, "{}", v),
        }
    }
}

impl<Reg> std::fmt::Display for VarLocation<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            VarLocation::Local(r) => write!(f, "%{}", r),
            VarLocation::Global(id) => write!(f, "*{}", id),
            VarLocation::Return => write!(f, "%ret"),
        }
    }
}

impl std::fmt::Display for GlobalVarId {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            GlobalVarId::Global(s) => write!(f, "${}", s),
            GlobalVarId::Static(s) => write!(f, "$static({})", s),
            GlobalVarId::CompilerInternal(s) => write!(f, "$internal({})", s),
            GlobalVarId::LocalStatic {
                name,
                function_name,
                index,
            } => write!(f, "$static({}, {}, {})", function_name, name, index),
            GlobalVarId::Literal(id) => write!(f, "$literal({})", id),
        }
    }
}

impl<Reg> std::fmt::Display for Tail<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Tail::Jump(n) => write!(f, "jmp {}", n),
            Tail::Cond(c, t, e) => write!(f, "if {} then goto {} else goto {}", c, *t, *e),
            Tail::Ret => write!(f, "ret"),
            Tail::Switch(val, width, cases, default) => {
                write!(f, "switch{} {} ", width, val)?;
                for (val, block) in cases {
                    write!(f, "{}: {}, ", val, block)?;
                }
                write!(f, "default: {}", default)
            }
        }
    }
}

impl<Reg> std::fmt::Display for JumpCondition<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            JumpCondition::StrictBool(r) => write!(f, "{}", r),
            JumpCondition::RelaxedBool(r, w) => write!(f, "bool{}({})", w, r),
            JumpCondition::Compare(op) => write!(
                f,
                "(cmp{}{}{} {}, {})",
                op.kind,
                get_sign_char(op.sign),
                op.width,
                op.lhs,
                op.rhs
            ),
        }
    }
}

impl<Reg> std::fmt::Debug for Op<Reg>
where
    Reg: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Display::fmt(&self, f)
    }
}

impl<GTail, Reg> std::fmt::Debug for GenericBlock<GTail, Reg>
where
    Reg: std::fmt::Display + Eq + Hash,
    GTail: std::fmt::Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Display::fmt(&self, f)
    }
}
