pub use crate::generic_ir::BlockNumber;
pub use crate::generic_ir::CompareKind;
pub use crate::generic_ir::GlobalVarId;
pub use crate::generic_ir::Sign;
pub use crate::generic_ir::Width;

use crate::generic_ir;

pub type VirtualReg = generic_ir::VirtualReg;
pub type VarLocation = generic_ir::VarLocation<VirtualReg>;
pub type Scalar = generic_ir::Scalar<VirtualReg>;
pub type Op = generic_ir::Op<VirtualReg>;

pub type ArgOp = generic_ir::ArgOp<VirtualReg>;
pub type CompareOp = generic_ir::CompareOp<VirtualReg>;
pub type CompareDesc = generic_ir::CompareDesc<VirtualReg>;
pub type UnaryUnsignedOp = generic_ir::UnaryUnsignedOp<VirtualReg>;
pub type BinaryOp = generic_ir::BinaryOp<VirtualReg>;
pub type BinaryUnsignedOp = generic_ir::BinaryUnsignedOp<VirtualReg>;
pub type ShiftOp = generic_ir::ShiftOp<VirtualReg>;
pub type ConvOp = generic_ir::ConvOp<VirtualReg>;
pub type StoreOp = generic_ir::StoreOp<VirtualReg>;
pub type LoadOp = generic_ir::LoadOp<VirtualReg>;
pub type CallOp = generic_ir::CallOp<VirtualReg>;
pub type MemcpyOp = generic_ir::MemcpyOp<VirtualReg>;
pub type VaArgOp = generic_ir::VaArgOp<VirtualReg>;
pub type Tail = generic_ir::Tail<VirtualReg>;
pub type Phi = generic_ir::Phi<VirtualReg>;
pub type GenericBlock<GTail> = generic_ir::GenericBlock<GTail, VirtualReg>;
pub type Block = GenericBlock<Tail>;
pub type Function = generic_ir::Function<VirtualReg>;
pub type JumpCondition = generic_ir::JumpCondition<VirtualReg>;
