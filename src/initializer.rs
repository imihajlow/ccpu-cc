use std::collections::HashMap;

use lang_c::{ast::IntegerSuffix, span::Span};

use crate::{
    ctype::{self, CType, QualifiedType, Qualifiers},
    error::{CompileError, ErrorCollector},
    machine,
};

#[derive(Debug, Clone)]
pub enum Constant {
    Void,
    Int(i128),
    Struct(HashMap<String, Constant>),
    Array(Vec<Constant>),
}

/**
 * Represents a constant value with given type.
 *
 * Invariant: for int types, contents of the val fits into the type.
 */
#[derive(Clone)]
pub struct TypedConstant {
    pub t: QualifiedType,
    pub val: Constant,
}

impl TypedConstant {
    pub fn new(val: Constant, t: QualifiedType) -> Self {
        Self { t, val }.clamp_to_type()
    }

    pub fn new_integer(value: i128, t: CType) -> Self {
        Self {
            t: QualifiedType {
                t,
                qualifiers: Qualifiers::CONST,
            },
            val: Constant::Int(value),
        }
        .clamp_to_type()
    }

    pub fn new_default(t: QualifiedType) -> Self {
        if t.t.is_scalar() {
            Self {
                t,
                val: Constant::Int(0),
            }
        } else {
            todo!()
        }
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
            val: Constant::Int(n as i128),
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
            if lhs.t.is_compatible_to(&rhs.t, false) {
                // If both operands have the same type, then no further conversion is needed.
                (lhs, rhs)
            } else {
                let common_type = lhs.t.t.least_common_int_type(&rhs.t.t);
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
     * Drop extra bits which don't fit into the target type (ensure the TypedConstant invariant).
     *
     * TODO emit warning when data is lost
     */
    pub fn clamp_to_type(self) -> Self {
        match self.val {
            Constant::Int(x) => {
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
                    val: Constant::Int(clamped),
                    ..self
                }
            }
            _ => self,
        }
    }

    pub fn implicit_cast(
        self,
        t: &QualifiedType,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<Self, ()> {
        // explicit and implicit casts are the same?
        if self.t.is_explicit_castable_to(t) {
            Ok(Self {
                t: t.clone(),
                val: self.val,
            }
            .clamp_to_type())
        } else {
            ec.record_error(
                CompileError::BadCast(format!("{}", self.t), format!("{}", t)),
                span,
            )?;
            unreachable!();
        }
    }

    pub fn unwrap_integer(&self) -> i128 {
        if let Constant::Int(x) = self.val {
            x
        } else {
            panic!("not an integer")
        }
    }

    pub fn is_zero(&self) -> bool {
        match self.val {
            Constant::Int(x) => x == 0,
            _ => false,
        }
    }

    pub fn negate(self) -> Self {
        let negated = match self.val {
            Constant::Int(x) => Self {
                val: Constant::Int(-x),
                ..self
            },
            _ => panic!(),
        };
        negated.clamp_to_type()
    }

    pub fn complement(self) -> Self {
        let comp = match self.val {
            Constant::Int(x) => Self {
                val: Constant::Int(!x),
                ..self
            },
            _ => panic!(),
        };
        comp.clamp_to_type()
    }

    pub fn boolean_not(self) -> Self {
        match self.val {
            Constant::Int(0) => Self {
                val: Constant::Int(1),
                ..self
            },
            Constant::Int(_) => Self {
                val: Constant::Int(0),
                ..self
            },
            _ => panic!(),
        }
    }
}
