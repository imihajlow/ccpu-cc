use crate::machine::*;

#[derive(Debug, Clone)]
pub enum CType {
    Void,
    Bool,
    Int(u8, bool),
    Float(u8),
    Pointer(Box<QualifiedType>),
    Array(Box<QualifiedType>, usize),
    Struct(TypeIdentifier, Vec<Field>),
    Alias(String, Box<QualifiedType>),
}

#[derive(Debug, Clone)]
pub struct QualifiedType {
    pub t: CType,
    pub qualifiers: Vec<Qualifier>,
}

#[derive(Debug, Clone)]
pub enum Qualifier {
    Const,
    Volatile,
    Restrict,
    Atomic,
}

#[derive(Debug, Clone)]
pub enum TypeIdentifier {
    Anon(usize),
    Named(String),
}

#[derive(Debug, Clone)]
pub struct Field {
    pub id: String,
    pub t: QualifiedType,
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
