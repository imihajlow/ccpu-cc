use std::collections::HashMap;

use crate::{ctype::QualifiedType, machine};

#[derive(Debug, Clone)]
pub enum Value {
    Void,
    Int(i128),
    Struct(HashMap<String, Value>),
    Array(Vec<Value>),
}

/**
 * Represents a constant value with given type.
 *
 * Invariant: for int types, contents of the val fits into the type.
 */
#[derive(Clone)]
pub struct TypedValue {
    pub t: QualifiedType,
    pub val: Value,
}

impl TypedValue {
    /**
     * Do the integer type promotion.
     */
    pub fn promote(self) -> Self {
        Self {
            t: self.t.promote(),
            ..self
        }
    }

    /**
     * Drop extra bits which don't fit into the target type (ensure the TypedValue invariant).
     */
    pub fn clamp_to_type(self) -> Self {
        use crate::ctype::CType;
        match self.val {
            Value::Int(x) => {
                let clamped = match self.t.t {
                    CType::Int(1, true) => (x as i8) as i128,
                    CType::Int(1, false) => (x as u8) as i128,
                    CType::Int(2, true) => (x as i16) as i128,
                    CType::Int(2, false) => (x as u16) as i128,
                    CType::Int(4, true) => (x as i32) as i128,
                    CType::Int(4, false) => (x as u32) as i128,
                    CType::Int(8, true) => (x as i64) as i128,
                    CType::Int(8, false) => (x as u64) as i128,
                    CType::Int(_, _) => unreachable!(),
                    CType::Bool => {
                        if x == 0 {
                            0
                        } else {
                            1
                        }
                    }
                    CType::Pointer(_) => x & !(!0 << (machine::PTR_SIZE * 8)),
                    _ => x,
                };
                Self {
                    val: Value::Int(clamped),
                    ..self
                }
            }
            _ => self,
        }
    }

    pub fn negate(self) -> Self {
        let negated = match self.val {
            Value::Int(x) => Self { val: Value::Int(-x), ..self },
            _ => panic!(),
        };
        negated.clamp_to_type()
    }

    pub fn complement(self) -> Self {
        let comp = match self.val {
            Value::Int(x) => Self { val: Value::Int(!x), ..self },
            _ => panic!(),
        };
        comp.clamp_to_type()
    }

    pub fn boolean_not(self) -> Self {
        match self.val {
            Value::Int(0) => Self { val: Value::Int(1), ..self },
            Value::Int(_) => Self { val: Value::Int(0), ..self },
            _ => panic!(),
        }
    }
}
