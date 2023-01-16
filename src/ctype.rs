use crate::machine::{self, *};
use bitflags::bitflags;
use replace_with::replace_with_or_abort;
use std::fmt::Formatter;

#[rustfmt::skip]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CType {
    Void,
    Bool,
    Int(u8, bool),
    Float(u8),
    Pointer(Box<QualifiedType>),
    Array(Box<QualifiedType>, Option<usize>),
    Struct(TypeIdentifier),
    Union(TypeIdentifier),
    Enum(TypeIdentifier),
    Function { result: Box<QualifiedType>, args: Vec<QualifiedType>, vararg: bool },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QualifiedType {
    pub t: CType,
    pub qualifiers: Qualifiers,
}

bitflags! {
    pub struct Qualifiers: u32 {
        const CONST = 1 << 0;
        const VOLATILE = 1 << 1;
        const RESTRICT = 1 << 2;
        const ATOMIC = 1 << 3;
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum TypeIdentifier {
    Anon(usize),
    Named(String),
}

pub const CHAR_TYPE: CType = CType::Int(1, CHAR_SIGNED);
pub const SCHAR_TYPE: CType = CType::Int(1, true);
pub const UCHAR_TYPE: CType = CType::Int(1, false);

pub const SHORT_TYPE: CType = SSHORT_TYPE;
pub const SSHORT_TYPE: CType = CType::Int(SHORT_SIZE, true);
pub const USHORT_TYPE: CType = CType::Int(SHORT_SIZE, false);

pub const INT_TYPE: CType = SINT_TYPE;
pub const SINT_TYPE: CType = CType::Int(INT_SIZE, true);
pub const UINT_TYPE: CType = CType::Int(INT_SIZE, false);

pub const LONG_TYPE: CType = SLONG_TYPE;
pub const SLONG_TYPE: CType = CType::Int(LONG_SIZE, true);
pub const ULONG_TYPE: CType = CType::Int(LONG_SIZE, false);

pub const LLONG_TYPE: CType = SLLONG_TYPE;
pub const SLLONG_TYPE: CType = CType::Int(LLONG_SIZE, true);
pub const ULLONG_TYPE: CType = CType::Int(LLONG_SIZE, false);

pub const FLOAT_TYPE: CType = CType::Float(4);
pub const DOUBLE_TYPE: CType = CType::Float(8);
pub const LDOUBLE_TYPE: CType = CType::Float(16);

impl QualifiedType {
    pub fn is_same_as(&self, other: &Self) -> bool {
        if self.qualifiers != other.qualifiers {
            return false;
        }
        match &self.t {
            CType::Pointer(t1) => {
                if let CType::Pointer(t2) = &other.t {
                    t1.is_same_as(&t2)
                } else {
                    false
                }
            }
            CType::Array(t1, n1) => {
                if let CType::Array(t2, n2) = &other.t {
                    if !t1.is_same_as(t2) {
                        false
                    } else {
                        n1 == n2 || n1.is_none() || n2.is_none()
                    }
                } else {
                    false
                }
            }
            _ => self == other,
        }
    }

    pub fn wrap_pointer(&mut self, qualifiers: Qualifiers) {
        replace_with_or_abort(self, |self_| QualifiedType {
            t: CType::Pointer(Box::new(self_)),
            qualifiers,
        });
    }
}

impl std::fmt::Display for TypeIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            TypeIdentifier::Anon(_) => f.write_str("<anonymous>"),
            TypeIdentifier::Named(name) => f.write_str(name),
        }
    }
}

impl std::fmt::Display for CType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use CType::*;
        #[allow(unreachable_patterns)]
        match self {
            Void => f.write_str("void"),
            Bool => f.write_str("_Bool"),
            Int(1, true) => f.write_str("signed char"),
            Int(1, false) => f.write_str("unsigned char"),
            Int(machine::INT_SIZE, true) => f.write_str("int"),
            Int(machine::INT_SIZE, false) => f.write_str("unsigned int"),
            Int(machine::SHORT_SIZE, true) => f.write_str("short"),
            Int(machine::SHORT_SIZE, false) => f.write_str("unsigned short"),
            Int(machine::LONG_SIZE, true) => f.write_str("long"),
            Int(machine::LONG_SIZE, false) => f.write_str("unsigned long"),
            Int(machine::LLONG_SIZE, true) => f.write_str("long long"),
            Int(machine::LLONG_SIZE, false) => f.write_str("unsigned long long"),
            Int(_, _) => unimplemented!(),
            Float(4) => f.write_str("float"),
            Float(8) => f.write_str("double"),
            Float(_) => unimplemented!(),
            Pointer(inner) => write!(f, "{} *", inner),
            Array(inner, None) => write!(f, "{} []", inner),
            Array(inner, Some(n)) => write!(f, "{} [{}]", inner, n),
            Struct(id) => write!(f, "struct {}", id),
            Union(id) => write!(f, "union {}", id),
            Enum(id) => write!(f, "enum {}", id),
            Function { .. } => todo!(),
        }
    }
}

impl std::fmt::Display for QualifiedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {}", self.t, self.qualifiers)
    }
}

impl std::fmt::Display for Qualifiers {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if self.contains(Qualifiers::CONST) {
            f.write_str("const ")?;
        }
        if self.contains(Qualifiers::VOLATILE) {
            f.write_str("volatile ")?;
        }
        if self.contains(Qualifiers::ATOMIC) {
            f.write_str("_Atomic ")?;
        }
        if self.contains(Qualifiers::RESTRICT) {
            f.write_str("restrict ")?;
        }
        Ok(())
    }
}
