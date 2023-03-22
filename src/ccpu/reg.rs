use std::fmt::Formatter;

use crate::register::Register;

const FRAME_SIZE: usize = 2048;

/**
 * Memory-mapped register on the hardware "stack" frame.
 */
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum FrameReg {
    FrameA(u16),
    FrameB(u16),
    Tmp,
}

impl Register for FrameReg {
    fn get_temp_register() -> Self {
        FrameReg::Tmp
    }

    fn get_register_count() -> usize {
        2 * FRAME_SIZE + 1
    }

    fn get_index(&self) -> usize {
        match self {
            FrameReg::FrameA(index) => *index as usize,
            FrameReg::FrameB(index) => *index as usize + FRAME_SIZE,
            FrameReg::Tmp => 2 * FRAME_SIZE,
        }
    }

    fn from_index(index: usize) -> Option<Self> {
        if index < FRAME_SIZE {
            Some(FrameReg::FrameA(index as u16))
        } else if index < 2 * FRAME_SIZE {
            Some(FrameReg::FrameB((index - FRAME_SIZE) as u16))
        } else if index == FRAME_SIZE * 2 {
            Some(FrameReg::Tmp)
        } else {
            None
        }
    }

    fn get_callee_arg(arg_idx: usize) -> Option<Self> {
        if arg_idx < FRAME_SIZE {
            Some(FrameReg::FrameB(arg_idx as u16))
        } else {
            None
        }
    }

    fn get_current_fn_arg(arg_idx: usize) -> Option<Self> {
        if arg_idx < FRAME_SIZE {
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
        }
    }
}

impl std::fmt::Debug for FrameReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Display::fmt(&self, f)
    }
}
