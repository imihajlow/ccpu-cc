use std::fmt::Formatter;

#[derive(Debug, Clone)]
pub enum ObjectLocation {
    Frame(u32),
}

impl std::fmt::Display for ObjectLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ObjectLocation::Frame(offset) => write!(f, "[F+0x{:x}]", offset),
        }
    }
}
