use std::collections::HashMap;

use lang_c::ast::IntegerSuffix;

use crate::{ctype::{QualifiedType, CType, self, Qualifiers}, machine};

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
     * Construct an integer constant according to 6.4.4.1
     */
    pub fn new_from_int_literal(n: u128, suffix: IntegerSuffix, is_decimal: bool) -> Self {
        use lang_c::ast::IntegerSize;
        let len: u8 = 128 - n.leading_zeros() as u8;
        let int_len = machine::INT_SIZE * 8 - 1;
        let uint_len = machine::INT_SIZE * 8;
        let long_len = machine::LONG_SIZE * 8 - 1;
        let ulong_len = machine::LONG_SIZE * 8;
        let llong_len = machine::LLONG_SIZE * 8 - 1;

        if suffix.imaginary {
            unimplemented!();
        }

        let t = match (suffix.unsigned, suffix.size) {
            (false, IntegerSize::Int) => {
                if len <= int_len {
                    ctype::INT_TYPE
                } else if len <= uint_len {
                    if is_decimal {
                        ctype::LONG_TYPE
                    } else {
                        ctype::UINT_TYPE
                    }
                } else if len <= long_len {
                    ctype::LONG_TYPE
                } else if len <= ulong_len {
                    if is_decimal {
                        ctype::LLONG_TYPE
                    } else {
                        ctype::ULONG_TYPE
                    }
                } else if len <= llong_len {
                    ctype::LLONG_TYPE
                } else {
                    if is_decimal {
                        ctype::LLONG_TYPE
                    } else {
                        ctype::ULLONG_TYPE
                    }
                }
            }
            (true, IntegerSize::Int) => {
                if len <= uint_len {
                    ctype::UINT_TYPE
                } else if len <= ulong_len {
                    ctype::ULONG_TYPE
                } else {
                    ctype::ULLONG_TYPE
                }
            }
            (false, IntegerSize::Long) => {
                if len <= long_len {
                    ctype::LONG_TYPE
                } else if len <= ulong_len {
                    if is_decimal {
                        ctype::LLONG_TYPE
                    } else {
                        ctype::ULONG_TYPE
                    }
                } else if len <= llong_len {
                    ctype::LLONG_TYPE
                } else {
                    if is_decimal {
                        ctype::LLONG_TYPE
                    } else {
                        ctype::ULLONG_TYPE
                    }
                }
            }
            (true, IntegerSize::Long) => {
                if len <= ulong_len {
                    ctype::ULONG_TYPE
                } else {
                    ctype::ULLONG_TYPE
                }
            }
            (false, IntegerSize::LongLong) => {
                if len <= llong_len {
                    ctype::LLONG_TYPE
                } else {
                    if is_decimal {
                        ctype::LLONG_TYPE
                    } else {
                        ctype::ULLONG_TYPE
                    }
                }
            }
            (true, IntegerSize::LongLong) => {
                ctype::ULLONG_TYPE
            }
        };
        Self {
            t: QualifiedType { t: t, qualifiers: Qualifiers::CONST },
            val: Value::Int(n as i128)
        }.clamp_to_type()
    }

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
