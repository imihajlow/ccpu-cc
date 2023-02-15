use crate::{
    error::{CompileWarning, ErrorCollector},
    ir,
    machine::{self, *},
};
use bitflags::bitflags;
use lang_c::span::Span;
use replace_with::replace_with_or_abort;
use std::fmt::Formatter;

#[rustfmt::skip]
#[derive(Debug, Clone, PartialEq)]
pub enum CType {
    Void,
    Bool,
    Int(u8, bool),
    Float(u8),
    Pointer(Box<QualifiedType>),
    Array(Box<QualifiedType>, Option<u32>),
    Struct(TypeIdentifier),
    Union(TypeIdentifier),
    Enum(TypeIdentifier),
    Function { result: Box<QualifiedType>, args: FunctionArgs, vararg: bool },
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionArgs {
    Empty,
    Void,
    List(Vec<(QualifiedType, Option<String>, Span)>),
}

#[derive(Debug, Clone, PartialEq)]
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

pub const WCHAR_TYPE: CType = CHAR16_TYPE;
pub const CHAR16_TYPE: CType = CType::Int(2, false);
pub const CHAR32_TYPE: CType = CType::Int(4, false);

pub const SIZE_TYPE: CType = CType::Int(PTR_SIZE, false);
pub const SSIZE_TYPE: CType = CType::Int(PTR_SIZE, true);

impl QualifiedType {
    /**
     * Checks if two types are equal and can be used interchangebly in definition.
     *
     * Example:
     * ```
     * extern int x[];
     * int x[25];
     * ```
     */
    pub fn is_compatible_to(&self, other: &Self, check_qualifiers: bool) -> bool {
        if check_qualifiers && self.qualifiers != other.qualifiers {
            return false;
        }
        match &self.t {
            CType::Pointer(t1) => {
                if let CType::Pointer(t2) = &other.t {
                    t1.is_compatible_to(&t2, true)
                } else {
                    false
                }
            }
            CType::Array(t1, n1) => {
                if let CType::Array(t2, n2) = &other.t {
                    if !t1.is_compatible_to(t2, true) {
                        false
                    } else {
                        n1 == n2 || n1.is_none() || n2.is_none()
                    }
                } else {
                    false
                }
            }
            CType::Function {
                result: r1,
                args: a1,
                vararg: va1,
            } => {
                if let CType::Function {
                    result: r2,
                    args: a2,
                    vararg: va2,
                } = &other.t
                {
                    va1 == va2 && r1.is_compatible_to(r2, true) && a1.is_compatible_to(a2)
                } else {
                    false
                }
            }
            _ => self == other,
        }
    }

    pub fn is_explicit_castable_to(&self, other: &Self) -> bool {
        self.t.is_explicit_castable_to(&other.t)
    }

    pub fn wrap_pointer(&mut self, qualifiers: Qualifiers) {
        replace_with_or_abort(self, |self_| QualifiedType {
            t: CType::Pointer(Box::new(self_)),
            qualifiers,
        });
    }

    /**
     * Original types becomes the return type of the function.
     */
    pub fn wrap_function(&mut self, args: FunctionArgs, vararg: bool) {
        replace_with_or_abort(self, |self_| QualifiedType {
            t: CType::Function {
                result: Box::new(self_),
                args,
                vararg,
            },
            qualifiers: Qualifiers::empty(),
        });
    }

    pub fn wrap_array(&mut self, size: Option<u32>) {
        replace_with_or_abort(self, |self_| QualifiedType {
            t: CType::Array(Box::new(self_), size),
            qualifiers: Qualifiers::empty(),
        });
    }

    pub fn is_const(&self) -> bool {
        self.qualifiers.contains(Qualifiers::CONST)
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

    pub fn dereference(self) -> Result<Self, Self> {
        match self.t.dereference() {
            Ok(t) => Ok(t),
            Err(t) => Err(Self { t, ..self }),
        }
    }
}

impl FunctionArgs {
    pub fn is_compatible_to(&self, other: &Self) -> bool {
        use FunctionArgs::*;
        match (self, other) {
            (Empty, _) => true,
            (_, Empty) => true,
            (Void, Void) => true,
            (List(v1), List(v2)) => {
                if v1.len() != v2.len() {
                    false
                } else {
                    v1.iter()
                        .zip(v2)
                        .all(|((t1, _, _), (t2, _, _))| t1.is_compatible_to(t2, true))
                }
            }
            _ => false,
        }
    }
}

impl CType {
    pub fn is_explicit_castable_to(&self, other: &Self) -> bool {
        if let CType::Void = other {
            return true;
        }
        if (self.is_arithmetic() || self.is_pointer())
            && (other.is_arithmetic() || other.is_pointer() || other.is_array())
        {
            return true;
        }
        match (self, other) {
            (CType::Struct(x), CType::Struct(y)) if x == y => true,
            (CType::Union(x), CType::Union(y)) if x == y => true,
            _ => false,
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        use CType::*;
        match self {
            Int(_, _) => true,
            Bool => true,
            Float(_) => true,
            Enum(_) => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        use CType::*;
        match self {
            Int(_, _) => true,
            Bool => true,
            Enum(_) => true,
            _ => false,
        }
    }

    pub fn is_long_double(&self) -> bool {
        false
    }

    pub fn is_double(&self) -> bool {
        if let CType::Float(8) = self {
            true
        } else {
            false
        }
    }

    pub fn is_float(&self) -> bool {
        if let CType::Float(4) = self {
            true
        } else {
            false
        }
    }

    pub fn is_any_float(&self) -> bool {
        if let CType::Float(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_scalar(&self) -> bool {
        use CType::*;
        match self {
            Int(_, _) => true,
            Bool => true,
            Float(_) => true,
            Enum(_) => true,
            Pointer(_) => true,
            Array(_, _) => true,
            _ => false,
        }
    }

    pub fn is_aggregate(&self) -> bool {
        use CType::*;
        match self {
            Array(_, _) => true,
            Struct(_) => true,
            Union(_) => true,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        if let CType::Pointer(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_array(&self) -> bool {
        if let CType::Array(_, _) = self {
            true
        } else {
            false
        }
    }

    pub fn is_dereferencable(&self) -> bool {
        self.is_pointer() || self.is_array()
    }

    pub fn is_void(&self) -> bool {
        if let CType::Void = self {
            true
        } else {
            false
        }
    }

    pub fn is_same_struct_union(&self, other: &Self) -> bool {
        use CType::*;
        match (self, other) {
            (Struct(x), Struct(y)) if x == y => true,
            (Union(x), Union(y)) if x == y => true,
            _ => false,
        }
    }

    /**
     * Do the integer type promotion.
     */
    pub fn promote(self) -> Self {
        match self {
            CType::Int(size, _) if size < machine::INT_SIZE => CType::Int(machine::INT_SIZE, true),
            CType::Bool => CType::Int(machine::INT_SIZE, true),
            x => x,
        }
    }

    pub fn sizeof(&self, span: Span, ec: &mut ErrorCollector) -> Result<u32, ()> {
        match self {
            CType::Void | CType::Function { .. } => {
                ec.record_warning(CompileWarning::InvalidSizeof, span)?;
                Ok(1)
            }
            CType::Bool => Ok(machine::BOOL_SIZE as u32),
            CType::Int(s, _) => Ok(*s as u32),
            CType::Float(s) => Ok(*s as u32),
            CType::Pointer(_) => Ok(machine::PTR_SIZE as u32),
            CType::Array(t, Some(n)) => Ok(t.t.sizeof(span, ec)? * *n),
            CType::Array(_, None) => Ok(machine::PTR_SIZE as u32),
            CType::Struct(_) => todo!(),
            CType::Union(_) => todo!(),
            CType::Enum(_) => todo!(),
        }
    }

    pub fn get_width_sign(&self) -> Option<(ir::Width, bool)> {
        match self {
            CType::Int(size, sign) => Some((ir::Width::new(*size), *sign)),
            CType::Bool => Some((ir::Width::new(machine::BOOL_SIZE), false)),
            CType::Array(_, _) | CType::Pointer(_) => {
                Some((ir::Width::new(machine::PTR_SIZE), false))
            }
            CType::Union(_) => todo!(),
            _ => None,
        }
    }

    pub fn get_scalar_width(&self) -> Option<ir::Width> {
        match self {
            CType::Int(size, _) | CType::Float(size) => Some(ir::Width::new(*size)),
            CType::Bool => Some(ir::Width::new(machine::BOOL_SIZE)),
            CType::Array(_, _) | CType::Pointer(_) => Some(ir::Width::new(machine::PTR_SIZE)),
            CType::Union(_) => todo!(),
            _ => None,
        }
    }

    pub fn is_complete(&self) -> bool {
        match self {
            CType::Void => false,
            CType::Struct(_) => todo!(),
            CType::Union(_) => todo!(),
            _ => true,
        }
    }

    pub fn dereferences_to_complete(&self) -> bool {
        match self {
            CType::Pointer(t) | CType::Array(t, _) => t.t.is_complete(),
            _ => false,
        }
    }

    /**
     * Find a common type for two integer types. Use to perform usual arithmetic conversion.
     */
    pub fn least_common_int_type(&self, other: &Self) -> Self {
        let lhs = self;
        let rhs = other;
        let (lhs_size, lhs_sign) = match lhs {
            CType::Int(size, sign) => (*size, *sign),
            _ => panic!("this function must be used on integer types"),
        };
        let (rhs_size, rhs_sign) = match rhs {
            CType::Int(size, sign) => (*size, *sign),
            _ => panic!("this function must be used on integer types"),
        };
        if lhs_sign == rhs_sign {
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
        }
    }

    pub fn dereference(self) -> Result<QualifiedType, Self> {
        match self {
            CType::Pointer(t) => Ok(*t),
            CType::Array(t, _) => Ok(*t),
            _ => Err(self),
        }
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
            Function {
                result,
                args,
                vararg,
            } => write!(
                f,
                "(fn ({}{}) -> {})",
                args,
                if *vararg { ", ..." } else { "" },
                result
            ),
        }
    }
}

impl std::fmt::Display for FunctionArgs {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            FunctionArgs::Empty => Ok(()),
            FunctionArgs::Void => f.write_str("void"),
            FunctionArgs::List(l) => {
                for s in l
                    .iter()
                    .map(|(t, _, _)| format!("{}", t))
                    .intersperse(", ".to_string())
                {
                    f.write_str(&s)?;
                }
                Ok(())
            }
        }
    }
}
impl std::fmt::Display for QualifiedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if !self.qualifiers.is_empty() {
            write!(f, "{} {}", self.t, self.qualifiers)
        } else {
            write!(f, "{}", self.t)
        }
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
