pub const STACK_FRAME_0_BASE: u16 = 0xc000;
pub const STACK_FRAME_1_BASE: u16 = 0xc800;
pub const STACK_FRAME_SIZE: u16 = 0x800;

pub const SP_INCDEC_ADDR: u16 = 0xFC02;
pub const SP_CR_ADDR: u16 = 0xFC03;
pub const SP0_ADDR: u16 = 0xFC00;
pub const SP1_ADDR: u16 = 0xFC01;

pub const SP_INCDEC_INC0: u8 = 0xFE;
pub const SP_INCDEC_INC1: u8 = 0xFD;
pub const SP_INCDEC_DEC0: u8 = 0xFB;
pub const SP_INCDEC_DEC1: u8 = 0xF7;

pub const SP_CR_ENA: u8 = 0x01;
