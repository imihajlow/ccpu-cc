use crate::ctype::QualifiedType;
use crate::initializer::TypedConstant;
use crate::object_location::ObjectLocation;
use std::fmt::Formatter;

use crate::ir;

#[derive(Debug, Clone)]
pub enum RValue {
    Void,
    Scalar(ir::Scalar),
    Object(ObjectLocation),
}

#[derive(Clone)]
pub struct TypedRValue {
    pub src: RValue,
    pub t: QualifiedType,
}

impl RValue {
    pub fn new_void() -> Self {
        RValue::Void
    }

    pub fn new_const(x: u64) -> Self {
        RValue::Scalar(ir::Scalar::ConstInt(x))
    }

    pub fn new_var(v: ir::VarLocation) -> Self {
        RValue::Scalar(ir::Scalar::Var(v))
    }

    pub fn new_object(l: ObjectLocation) -> Self {
        RValue::Object(l)
    }

    pub fn unwrap_scalar(self) -> ir::Scalar {
        if let RValue::Scalar(s) = self {
            s
        } else {
            panic!("not a scalar rvalue")
        }
    }

    pub fn unwrap_object_location(self) -> ObjectLocation {
        if let RValue::Object(l) = self {
            l
        } else {
            panic!("not an object")
        }
    }
}

impl TypedRValue {
    pub fn new_from_typed_constant(tv: TypedConstant) -> Self {
        use crate::initializer::Constant;
        match tv.val {
            Constant::Int(x) => Self {
                src: RValue::new_const(x as u64),
                t: tv.t,
            },
            Constant::Void => Self {
                src: RValue::Void,
                t: tv.t,
            },
            _ => todo!(),
        }
    }

    pub fn unwrap_scalar(self) -> ir::Scalar {
        self.src.unwrap_scalar()
    }

    pub fn unwrap_scalar_and_type(self) -> (ir::Scalar, QualifiedType) {
        (self.src.unwrap_scalar(), self.t)
    }
}

impl std::fmt::Display for RValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            RValue::Void => f.write_str("(void)"),
            RValue::Scalar(s) => s.fmt(f),
            RValue::Object(l) => write!(f, "obj@{}", l),
        }
    }
}
