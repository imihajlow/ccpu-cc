use std::fmt::Formatter;

use crate::ir;

#[derive(Debug, Clone)]
pub enum ObjectLocation {
    PointedBy(ir::Scalar),
}

impl ObjectLocation {
    pub fn get_address(self) -> ir::Scalar {
        match self {
            ObjectLocation::PointedBy(p) => p,
        }
    }
}

impl std::fmt::Display for ObjectLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ObjectLocation::PointedBy(p) => write!(f, "*{}", p),
        }
    }
}
