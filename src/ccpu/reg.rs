use std::fmt::Formatter;

use crate::register::Register;

use super::stack::{STACK_FRAME_0_BASE, STACK_FRAME_1_BASE, STACK_FRAME_SIZE};

pub const FRAME_SIZE: usize = STACK_FRAME_SIZE as usize / 8;

pub const VA_ARGS_START: usize = FRAME_SIZE / 2;
pub const INTRIN_START: usize = FRAME_SIZE - 6;

// TODO prevent overlapping of variadic args and registers - unlikely

/**
 * Memory-mapped register on the hardware "stack" frame.
 */
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum FrameReg {
    FrameA(u16),
    FrameB(u16),
    Tmp,
    RetAddr,
    IntrinsicArg1,
    IntrinsicArg2,
    IntrinsicArg3,
    IntrinsicRet,
}

impl FrameReg {
    /**
     * Get virtual register address in the normal case.
     * Normaly, the frame from 0xC000 to 0xC800 belongs to the current function,
     * frame from 0xC800 to 0xD000 to the callee.
     */
    pub fn get_address(&self) -> u16 {
        match self {
            FrameReg::FrameA(n) => STACK_FRAME_0_BASE + n * 8,
            FrameReg::FrameB(n) => STACK_FRAME_1_BASE + n * 8,
            FrameReg::IntrinsicArg3 => STACK_FRAME_0_BASE + STACK_FRAME_SIZE - 8 * 6,
            FrameReg::IntrinsicArg2 => STACK_FRAME_0_BASE + STACK_FRAME_SIZE - 8 * 5,
            FrameReg::IntrinsicArg1 => STACK_FRAME_0_BASE + STACK_FRAME_SIZE - 8 * 4,
            FrameReg::IntrinsicRet => STACK_FRAME_0_BASE + STACK_FRAME_SIZE - 8 * 3,
            FrameReg::Tmp => STACK_FRAME_0_BASE + STACK_FRAME_SIZE - 8 * 2,
            FrameReg::RetAddr => STACK_FRAME_0_BASE + STACK_FRAME_SIZE - 8 * 1,
        }
    }

    /**
     * Get callee's virtual register address.
     */
    pub fn get_address_callee(&self) -> Option<u16> {
        match self {
            FrameReg::FrameA(n) => Some(STACK_FRAME_1_BASE + n * 8),
            FrameReg::Tmp => Some(STACK_FRAME_1_BASE + STACK_FRAME_SIZE - 8 * 2),
            FrameReg::RetAddr => Some(STACK_FRAME_1_BASE + STACK_FRAME_SIZE - 8 * 1),
            FrameReg::IntrinsicArg3 => Some(STACK_FRAME_1_BASE + STACK_FRAME_SIZE - 8 * 6),
            FrameReg::IntrinsicArg2 => Some(STACK_FRAME_1_BASE + STACK_FRAME_SIZE - 8 * 5),
            FrameReg::IntrinsicArg1 => Some(STACK_FRAME_1_BASE + STACK_FRAME_SIZE - 8 * 4),
            FrameReg::IntrinsicRet => Some(STACK_FRAME_1_BASE + STACK_FRAME_SIZE - 8 * 3),
            FrameReg::FrameB(_) => None,
        }
    }
}

impl Register for FrameReg {
    fn get_temp_register() -> Self {
        FrameReg::Tmp
    }

    fn get_register_count() -> usize {
        2 * FRAME_SIZE
    }

    fn get_index(&self) -> usize {
        match self {
            FrameReg::FrameA(index) => *index as usize,
            FrameReg::FrameB(index) => *index as usize + FRAME_SIZE,
            FrameReg::RetAddr => FRAME_SIZE - 1,
            FrameReg::Tmp => FRAME_SIZE - 2,
            FrameReg::IntrinsicRet => FRAME_SIZE - 3,
            FrameReg::IntrinsicArg1 => FRAME_SIZE - 4,
            FrameReg::IntrinsicArg2 => FRAME_SIZE - 5,
            FrameReg::IntrinsicArg3 => FRAME_SIZE - 6,
        }
    }

    fn from_index(index: usize) -> Option<Self> {
        if index < FRAME_SIZE - 6 {
            Some(FrameReg::FrameA(index as u16))
        } else if index == FRAME_SIZE - 6 {
            Some(FrameReg::IntrinsicArg3)
        } else if index == FRAME_SIZE - 5 {
            Some(FrameReg::IntrinsicArg2)
        } else if index == FRAME_SIZE - 4 {
            Some(FrameReg::IntrinsicArg1)
        } else if index == FRAME_SIZE - 3 {
            Some(FrameReg::IntrinsicRet)
        } else if index == FRAME_SIZE - 2 {
            Some(FrameReg::Tmp)
        } else if index == FRAME_SIZE - 1 {
            Some(FrameReg::RetAddr)
        } else if index < 2 * FRAME_SIZE {
            Some(FrameReg::FrameB((index - FRAME_SIZE) as u16))
        } else {
            None
        }
    }

    fn get_callee_arg(arg_idx: usize) -> Option<Self> {
        if arg_idx < VA_ARGS_START {
            Some(FrameReg::FrameB(arg_idx as u16))
        } else {
            None
        }
    }

    fn get_callee_va_arg(va_arg_idx: usize) -> Option<Self> {
        let idx = va_arg_idx + VA_ARGS_START;
        if idx < FRAME_SIZE - 6 {
            Some(FrameReg::FrameB(idx as u16))
        } else {
            None
        }
    }

    fn get_current_fn_arg(arg_idx: usize) -> Option<Self> {
        if arg_idx < FRAME_SIZE - 6 {
            Some(FrameReg::FrameA(arg_idx as u16))
        } else {
            None
        }
    }
}

impl std::fmt::Display for FrameReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::FrameA(n) => write!(f, "A{}", n),
            Self::FrameB(n) => write!(f, "B{}", n),
            Self::Tmp => f.write_str("tmp"),
            Self::RetAddr => f.write_str("ret_addr"),
            Self::IntrinsicArg1 => write!(f, "IA1"),
            Self::IntrinsicArg2 => write!(f, "IA2"),
            Self::IntrinsicArg3 => write!(f, "IA3"),
            Self::IntrinsicRet => write!(f, "IRet"),
        }
    }
}

impl std::fmt::Debug for FrameReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Display::fmt(&self, f)
    }
}
