use std::{
    collections::{HashMap, HashSet},
    fmt::Formatter,
    mem,
};

use replace_with::replace_with_or_abort;

use crate::{machine, name_scope::NameScope};

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
    Frame(u32),
    Return,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct GlobalVarId(pub String, pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scalar {
    ConstInt(u64),
    SymbolOffset(GlobalVarId, u16),
    FramePointer,
    Var(VarLocation),
}

#[derive(Clone, PartialEq, Eq)]
pub enum Op {
    Undefined(Reg),
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
    Memcpy(MemcpyOp),
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
pub struct MemcpyOp {
    pub dst_addr: Scalar,
    pub src_addr: Scalar,
    pub len: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tail {
    Jump(BlockNumber),
    Cond(Scalar, BlockNumber, BlockNumber),
    Ret,
    Switch(Scalar, Width, Vec<(u64, BlockNumber)>, BlockNumber),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Phi {
    srcs: HashMap<Reg, Vec<(BlockNumber, Scalar)>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericBlock<GTail> {
    pub phi: Phi,
    pub ops: Vec<Op>,
    pub tail: GTail,
    pub loop_depth: usize,
}

pub type Block = GenericBlock<Tail>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub stack_size: u16,
    pub blocks: Vec<Block>,
}

impl Op {
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
            #[cfg(test)]
            Op::Dummy(_) => None,
        }
    }

    pub fn is_read_from_register(&self, reg: Reg) -> bool {
        match self {
            Op::Undefined(_) => false,
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
            Op::Memcpy(op) => op.is_read_from_register(reg),
            #[cfg(test)]
            Op::Dummy(_) => false,
        }
    }

    pub fn is_memory_read(&self) -> bool {
        match self {
            Op::Undefined(_) => false,
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
            Op::Memcpy(_) => true,
            #[cfg(test)]
            Op::Dummy(_) => false,
        }
    }

    pub fn is_memory_write(&self) -> bool {
        match self {
            Op::Undefined(_) => false,
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
            Op::Memcpy(_) => true,
            #[cfg(test)]
            Op::Dummy(_) => false,
        }
    }

    pub fn collect_read_regs(&self, set: &mut HashSet<Reg>) {
        match self {
            Op::Undefined(_) => (),
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
            Op::Memcpy(op) => op.collect_read_regs(set),
            #[cfg(test)]
            Op::Dummy(_) => (),
        }
    }

    pub fn collect_set_regs(&self, set: &mut HashSet<Reg>) {
        match self {
            Op::Undefined(reg) => {
                set.insert(*reg);
            }
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
            Op::Memcpy(op) => op.collect_set_regs(set),
            #[cfg(test)]
            Op::Dummy(_) => (),
        }
    }

    /**
     * Remap registers according to the map.
     * If scope is given, for each assignment to a register create its new version and update the map.
     */
    pub fn remap_regs(&mut self, map: &mut HashMap<Reg, Reg>, scope: Option<&mut NameScope>) {
        match self {
            Op::Undefined(reg) => {
                if let Some(scope) = scope {
                    let new_reg = scope.alloc_reg();
                    map.insert(*reg, new_reg);
                    *reg = new_reg;
                } else {
                    *reg = map.get(reg).copied().unwrap_or(*reg);
                }
            }
            Op::Copy(op) => op.remap_regs(map, scope),
            Op::Bool(op) => op.remap_regs(map, scope),
            Op::BoolInv(op) => op.remap_regs(map, scope),
            Op::Add(op) => op.remap_regs(map, scope),
            Op::Sub(op) => op.remap_regs(map, scope),
            Op::Mul(op) => op.remap_regs(map, scope),
            Op::Div(op) => op.remap_regs(map, scope),
            Op::Mod(op) => op.remap_regs(map, scope),
            Op::BAnd(op) => op.remap_regs(map, scope),
            Op::BOr(op) => op.remap_regs(map, scope),
            Op::BXor(op) => op.remap_regs(map, scope),
            Op::LShift(op) => op.remap_regs(map, scope),
            Op::RShift(op) => op.remap_regs(map, scope),
            Op::Neg(op) => op.remap_regs(map, scope),
            Op::Not(op) => op.remap_regs(map, scope),
            Op::Compare(op) => op.remap_regs(map, scope),
            Op::Conv(op) => op.remap_regs(map, scope),
            Op::Store(op) => op.remap_regs(map, scope),
            Op::Load(op) => op.remap_regs(map, scope),
            Op::Call(op) => op.remap_regs(map, scope),
            Op::Memcpy(op) => op.remap_regs(map, scope),
            #[cfg(test)]
            Op::Dummy(_) => (),
        }
    }
}

impl Tail {
    pub fn collect_read_regs(&self, set: &mut HashSet<Reg>) {
        match self {
            Tail::Jump(_) | Tail::Ret => (),
            Tail::Cond(c, _, _) => c.collect_regs(set),
            Tail::Switch(c, _, _, _) => c.collect_regs(set),
        }
    }

    pub fn is_read_from_register(&self, reg: Reg) -> bool {
        match self {
            Tail::Jump(_) | Tail::Ret => false,
            Tail::Cond(c, _, _) => c.is_reg(reg),
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

    pub fn remap_regs(&mut self, map: &HashMap<Reg, Reg>) {
        match self {
            Tail::Jump(_) | Tail::Ret => (),
            Tail::Cond(c, _, _) => c.remap_reg(map),
            Tail::Switch(c, _, _, _) => c.remap_reg(map),
        }
    }

    pub fn is_return(&self) -> bool {
        match self {
            Tail::Ret => true,
            _ => false,
        }
    }
}

impl Phi {
    pub fn new() -> Self {
        Self {
            srcs: HashMap::new(),
        }
    }

    pub fn add_binding(&mut self, dst: &Reg, src: &Reg, block: usize) {
        if let Some(l) = self.srcs.get_mut(dst) {
            l.push((block, Scalar::Var(VarLocation::Local(*src))));
        } else {
            self.srcs
                .insert(*dst, vec![(block, Scalar::Var(VarLocation::Local(*src)))]);
        }
    }

    pub fn with_adjusted_block_ids(self, offsets: &Vec<usize>) -> Self {
        let srcs = self
            .srcs
            .into_iter()
            .map(|(k, v)| {
                (
                    k,
                    v.into_iter()
                        .map(|(block_id, var)| (block_id - offsets[block_id], var))
                        .collect(),
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

    pub fn remap_regs(&mut self, map: &HashMap<Reg, Reg>) {
        replace_with_or_abort(&mut self.srcs, |srcs| {
            srcs.into_iter()
                .map(|(dst, mut srcs)| {
                    for (_, value) in srcs.iter_mut() {
                        value.remap_reg(map);
                    }
                    (map.get(&dst).copied().unwrap_or(dst), srcs)
                })
                .collect()
        });
    }

    pub fn collect_set_regs(&self, set: &mut HashSet<Reg>) {
        for (dst, _) in self.srcs.iter() {
            set.insert(*dst);
        }
    }

    pub fn collect_read_regs(&self, set: &mut HashSet<Reg>) {
        for (_, src) in self.srcs.iter() {
            for (_, val) in src {
                val.collect_regs(set);
            }
        }
    }
}

impl CompareOp {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.rhs.is_reg(reg) || self.lhs.is_reg(reg)
    }

    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.lhs.collect_regs(regs);
        self.rhs.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs(&mut self, map: &mut HashMap<Reg, Reg>, scope: Option<&mut NameScope>) {
        self.lhs.remap_reg(map);
        self.rhs.remap_reg(map);
        self.dst.remap_reg_to_new_version(map, scope);
    }
}

impl UnaryUnsignedOp {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.src.is_reg(reg)
    }

    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.src.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs(&mut self, map: &mut HashMap<Reg, Reg>, scope: Option<&mut NameScope>) {
        self.src.remap_reg(map);
        self.dst.remap_reg_to_new_version(map, scope);
    }
}

impl BinaryOp {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.rhs.is_reg(reg) || self.lhs.is_reg(reg)
    }

    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.lhs.collect_regs(regs);
        self.rhs.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs(&mut self, map: &mut HashMap<Reg, Reg>, scope: Option<&mut NameScope>) {
        self.lhs.remap_reg(map);
        self.rhs.remap_reg(map);
        self.dst.remap_reg_to_new_version(map, scope);
    }
}

impl BinaryUnsignedOp {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.rhs.is_reg(reg) || self.lhs.is_reg(reg)
    }

    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.lhs.collect_regs(regs);
        self.rhs.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs(&mut self, map: &mut HashMap<Reg, Reg>, scope: Option<&mut NameScope>) {
        self.lhs.remap_reg(map);
        self.rhs.remap_reg(map);
        self.dst.remap_reg_to_new_version(map, scope);
    }
}

impl ShiftOp {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.rhs.is_reg(reg) || self.lhs.is_reg(reg)
    }

    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.lhs.collect_regs(regs);
        self.rhs.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs(&mut self, map: &mut HashMap<Reg, Reg>, scope: Option<&mut NameScope>) {
        self.lhs.remap_reg(map);
        self.rhs.remap_reg(map);
        self.dst.remap_reg_to_new_version(map, scope);
    }
}

impl ConvOp {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.src.is_reg(reg)
    }

    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.src.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs(&mut self, map: &mut HashMap<Reg, Reg>, scope: Option<&mut NameScope>) {
        self.src.remap_reg(map);
        self.dst.remap_reg_to_new_version(map, scope);
    }
}

impl StoreOp {
    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.src.is_reg(reg) || self.dst_addr.is_reg(reg)
    }

    fn get_dst_reg(&self) -> Option<Reg> {
        None
    }

    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.src.collect_regs(regs);
        self.dst_addr.collect_regs(regs);
    }

    fn collect_set_regs(&self, _regs: &mut HashSet<Reg>) {}

    fn remap_regs(&mut self, map: &mut HashMap<Reg, Reg>, _scope: Option<&mut NameScope>) {
        self.src.remap_reg(map);
        self.dst_addr.remap_reg(map);
    }
}

impl LoadOp {
    fn get_dst_reg(&self) -> Option<Reg> {
        self.dst.get_reg()
    }

    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.src_addr.is_reg(reg)
    }

    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.src_addr.collect_regs(regs);
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.collect_regs(regs);
    }

    fn remap_regs(&mut self, map: &mut HashMap<Reg, Reg>, scope: Option<&mut NameScope>) {
        self.src_addr.remap_reg(map);
        self.dst.remap_reg_to_new_version(map, scope);
    }
}

impl CallOp {
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
        false
    }

    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        for (a, _) in &self.args {
            a.collect_regs(regs);
        }
    }

    fn collect_set_regs(&self, regs: &mut HashSet<Reg>) {
        self.dst.as_ref().map(|(dst, _)| dst.collect_regs(regs));
    }

    fn remap_regs(&mut self, map: &mut HashMap<Reg, Reg>, scope: Option<&mut NameScope>) {
        for (a, _) in &mut self.args {
            a.remap_reg(map);
        }
        self.dst
            .as_mut()
            .map(|(dst, _)| dst.remap_reg_to_new_version(map, scope));
    }
}

impl MemcpyOp {
    fn is_read_from_register(&self, reg: Reg) -> bool {
        self.src_addr.is_reg(reg) || self.dst_addr.is_reg(reg)
    }

    fn collect_read_regs(&self, regs: &mut HashSet<Reg>) {
        self.src_addr.collect_regs(regs);
        self.dst_addr.collect_regs(regs);
    }

    fn collect_set_regs(&self, _regs: &mut HashSet<Reg>) {}

    fn remap_regs(&mut self, map: &mut HashMap<Reg, Reg>, _scope: Option<&mut NameScope>) {
        self.src_addr.remap_reg(map);
        self.dst_addr.remap_reg(map);
    }
}

impl VarLocation {
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

    fn is_reg(&self, reg: Reg) -> bool {
        if let VarLocation::Local(x) = self {
            *x == reg
        } else {
            false
        }
    }

    fn collect_regs(&self, regs: &mut HashSet<Reg>) {
        if let VarLocation::Local(n) = self {
            regs.insert(*n);
        }
    }

    fn remap_reg(&mut self, map: &HashMap<Reg, Reg>) {
        if let VarLocation::Local(n) = self {
            *n = map.get(n).copied().unwrap_or(*n);
        }
    }

    /**
     * If scope is given, allocate a new register, remap this location to its value and update the map.
     * If scope is none, remap the register according to the map if it exists there.
     */
    fn remap_reg_to_new_version(
        &mut self,
        map: &mut HashMap<Reg, Reg>,
        scope: Option<&mut NameScope>,
    ) {
        if let VarLocation::Local(n) = self {
            let reg = if let Some(scope) = scope {
                let reg = scope.alloc_reg();
                map.insert(*n, reg);
                reg
            } else {
                map.get(n).copied().unwrap_or(*n)
            };
            *n = reg;
        }
    }
}

impl Scalar {
    pub fn unwrap_var(self) -> VarLocation {
        if let Scalar::Var(v) = self {
            v
        } else {
            panic!("not a var");
        }
    }

    fn is_reg(&self, reg: Reg) -> bool {
        if let Scalar::Var(x) = self {
            x.is_reg(reg)
        } else {
            false
        }
    }

    fn collect_regs(&self, regs: &mut HashSet<Reg>) {
        if let Scalar::Var(x) = self {
            x.collect_regs(regs);
        }
    }

    fn remap_reg(&mut self, map: &HashMap<Reg, Reg>) {
        if let Scalar::Var(v) = self {
            v.remap_reg(map);
        }
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
        writeln!(f, "{}", self.phi)?;
        for op in &self.ops {
            writeln!(f, "{}", op)?;
        }
        writeln!(f, "{}", self.tail)
    }
}

impl std::fmt::Display for Phi {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        for (dst, srcs) in self.srcs.iter() {
            write!(f, "%{} = ", dst)?;
            for s in srcs
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

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Undefined(reg) => write!(f, "undef {}", reg),
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
            Self::Memcpy(op) => write!(f, "memcpy{}", op),
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
        write!(f, "{} [{}], {}", self.width, self.dst_addr, self.src)
    }
}

impl std::fmt::Display for LoadOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {}, [{}]", self.width, self.dst, self.src_addr)
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

impl std::fmt::Display for MemcpyOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, " [{}], [{}], {}", self.dst_addr, self.src_addr, self.len)
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
            Scalar::SymbolOffset(sym, 0) => write!(f, "{}", sym),
            Scalar::SymbolOffset(sym, offs) => write!(f, "{}+0x{:x}", sym, offs),
            Scalar::Var(v) => write!(f, "{}", v),
            Scalar::FramePointer => write!(f, "F"),
        }
    }
}

impl std::fmt::Display for VarLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            VarLocation::Local(r) => write!(f, "%{}", r),
            VarLocation::Global(id) => write!(f, "{}", id),
            VarLocation::Arg(p) => write!(f, "%a{}", p),
            VarLocation::Frame(p) => write!(f, "[F+0x{:x}]", p),
            VarLocation::Return => write!(f, "%ret"),
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

impl std::fmt::Debug for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Display::fmt(&self, f)
    }
}
