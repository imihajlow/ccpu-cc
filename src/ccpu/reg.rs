use std::fmt::Formatter;

use crate::register::Register;

use super::stack::{STACK_FRAME_0_BASE, STACK_FRAME_1_BASE, STACK_FRAME_SIZE};

const FRAME_SIZE: usize = STACK_FRAME_SIZE as usize / 8;

/**
 * Memory-mapped register on the hardware "stack" frame.
 */
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum FrameReg {
    FrameA(u16),
    FrameB(u16),
    Tmp,
    RetAddr,
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
            FrameReg::Tmp => STACK_FRAME_0_BASE + STACK_FRAME_SIZE - 16,
            FrameReg::RetAddr => STACK_FRAME_0_BASE + STACK_FRAME_SIZE - 8,
        }
    }

    /**
     * Get callee's virtual register address.
     */
    pub fn get_address_callee(&self) -> Option<u16> {
        match self {
            FrameReg::FrameA(n) => Some(STACK_FRAME_1_BASE + n * 8),
            FrameReg::Tmp => Some(STACK_FRAME_1_BASE + STACK_FRAME_SIZE - 16),
            FrameReg::RetAddr => Some(STACK_FRAME_1_BASE + STACK_FRAME_SIZE - 8),
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
        }
    }

    fn from_index(index: usize) -> Option<Self> {
        if index < FRAME_SIZE - 2 {
            Some(FrameReg::FrameA(index as u16))
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
        if arg_idx < FRAME_SIZE - 2 {
            Some(FrameReg::FrameB(arg_idx as u16))
        } else {
            None
        }
    }

    fn get_current_fn_arg(arg_idx: usize) -> Option<Self> {
        if arg_idx < FRAME_SIZE - 2 {
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
        }
    }
}

impl std::fmt::Debug for FrameReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Display::fmt(&self, f)
    }
}
