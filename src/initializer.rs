use std::collections::HashMap;

use lang_c::ast::IntegerSuffix;

use crate::{
    ctype::{self, CType, QualifiedType, Qualifiers},
    machine,
};

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
    pub fn new_integer(value: i128, t: CType) -> Self {
        Self {
            t: QualifiedType {
                t,
                qualifiers: Qualifiers::CONST,
            },
            val: Value::Int(value),
        }
        .clamp_to_type()
    }

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
            (true, IntegerSize::LongLong) => ctype::ULLONG_TYPE,
        };
        Self {
            t: QualifiedType {
                t: t,
                qualifiers: Qualifiers::CONST,
            },
            val: Value::Int(n as i128),
        }
        .clamp_to_type()
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
     * Perform usual arithmetic conversions according to 6.3.1.8
     */
    pub fn usual_arithmetic_convert(lhs: Self, rhs: Self) -> (Self, Self) {
        if lhs.t.t.is_long_double() || rhs.t.t.is_long_double() {
            todo!()
        } else if lhs.t.t.is_double() || rhs.t.t.is_double() {
            todo!()
        } else if lhs.t.t.is_float() || rhs.t.t.is_float() {
            todo!()
        } else if lhs.t.t.is_integer() && rhs.t.t.is_integer() {
            // Otherwise, the integer promotions are performed on both operands.
            let lhs = lhs.promote();
            let rhs = rhs.promote();
            // Then the following rules are applied to the promoted operands
            if lhs.t.is_same_as(&rhs.t) {
                // If both operands have the same type, then no further conversion is needed.
                (lhs, rhs)
            } else {
                let (lhs_size, lhs_sign) = match lhs.t.t {
                    CType::Int(size, sign) => (size, sign),
                    _ => unreachable!(),
                };
                let (rhs_size, rhs_sign) = match rhs.t.t {
                    CType::Int(size, sign) => (size, sign),
                    _ => unreachable!(),
                };
                let common_type = if lhs_sign == rhs_sign {
                    // Otherwise, if both operands have signed integer types or both have unsigned integer types,
                    // the operand with the type of lesser integer conversion rank is converted to the type
                    // of the operand with greater rank.
                    let max_size = std::cmp::max(lhs_size, rhs_size);
                    CType::Int(max_size, lhs_sign)
                } else {
                    let (signed_size, unsigned_size) = if lhs_sign {
                        (lhs_size, rhs_size)
                    } else {
                        (rhs_size, lhs_size)
                    };
                    if unsigned_size >= signed_size {
                        // Otherwise, if the operand that has unsigned integer type has rank greater or equal
                        // to the rank of the type of the other operand, then the operand with signed integer type
                        // is converted to the type of the operand with unsigned integer type.
                        CType::Int(unsigned_size, false)
                    } else if signed_size > unsigned_size {
                        // Otherwise, if the type of the operand with signed integer type can represent
                        // all of the values of the type of the operand with unsigned integer type,
                        // then the operand with unsigned integer type is converted to the type
                        // of the operand with signed integer type.
                        CType::Int(signed_size, true)
                    } else {
                        // Otherwise, both operands are converted to the unsigned integer type
                        // corresponding to the type of the operand with signed integer type.
                        // -- this will never happen
                        CType::Int(signed_size, false)
                    }
                };
                (
                    Self {
                        t: QualifiedType {
                            t: common_type.clone(),
                            ..lhs.t
                        },
                        ..lhs
                    }
                    .clamp_to_type(),
                    Self {
                        t: QualifiedType {
                            t: common_type,
                            ..rhs.t
                        },
                        ..rhs
                    }
                    .clamp_to_type(),
                )
            }
        } else {
            unreachable!()
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

    pub fn unwrap_integer(&self) -> i128 {
        if let Value::Int(x) = self.val {
            x
        } else {
            panic!("not an integer")
        }
    }

    pub fn negate(self) -> Self {
        let negated = match self.val {
            Value::Int(x) => Self {
                val: Value::Int(-x),
                ..self
            },
            _ => panic!(),
        };
        negated.clamp_to_type()
    }

    pub fn complement(self) -> Self {
        let comp = match self.val {
            Value::Int(x) => Self {
                val: Value::Int(!x),
                ..self
            },
            _ => panic!(),
        };
        comp.clamp_to_type()
    }

    pub fn boolean_not(self) -> Self {
        match self.val {
            Value::Int(0) => Self {
                val: Value::Int(1),
                ..self
            },
            Value::Int(_) => Self {
                val: Value::Int(0),
                ..self
            },
            _ => panic!(),
        }
    }
}
