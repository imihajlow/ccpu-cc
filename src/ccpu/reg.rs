use std::fmt::Formatter;

use crate::temp_reg::TempReg;

/**
 * Memory-mapped register.
 */
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum FrameReg {
    FrameA(u16),
    FrameB(u16),
    Tmp,
}

impl FrameReg {
    pub fn unwrap_frame_a(&self) -> u16 {
        if let FrameReg::FrameA(n) = self {
            *n
        } else {
            panic!("Not a frame-A register")
        }
    }
}

impl TempReg for FrameReg {
    fn get_temp_register() -> Self {
        FrameReg::Tmp
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
