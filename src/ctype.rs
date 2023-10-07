use crate::{
    error::{CompileError, CompileWarning, ErrorCollector},
    ir,
    machine::{self, *},
    name_scope::NameScope,
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
    StructUnion(StructUnionIdentifier),
    Enum(EnumIdentifier),
    Function { result: Box<QualifiedType>, args: FunctionArgs, vararg: bool },
    VaList,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, Hash)]
pub struct StructUnionIdentifier {
    pub id: usize,
    pub name: Option<String>,
    pub kind: StructUnionKind,
}

#[derive(Debug, Clone, Hash)]
pub struct EnumIdentifier {
    pub id: usize,
    pub name: Option<String>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub enum StructUnionKind {
    Struct,
    Union,
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

    pub fn get_field(
        &self,
        name: &str,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<(u32, QualifiedType), ()> {
        match &self.t {
            CType::StructUnion(id) => {
                let su = scope.get_struct_union(id);
                if !su.is_complete() {
                    ec.record_error(CompileError::IncompleteStruct(self.clone()), span)?;
                    unreachable!()
                }
                if let Some(r) = su.get_field(name, scope, span, ec)? {
                    Ok(r)
                } else {
                    ec.record_error(
                        CompileError::NoSuchMember(id.clone(), name.to_string()),
                        span,
                    )?;
                    unreachable!()
                }
            }
            _ => {
                ec.record_error(CompileError::NotAStruct(self.clone()), span)?;
                unreachable!();
            }
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
        if self == other {
            return true;
        }
        if let CType::Void = other {
            return true;
        }
        if (other.is_arithmetic() || other.is_pointer())
            && (self.is_arithmetic() || self.is_pointer() || self.is_array())
        {
            return true;
        }
        match (self, other) {
            (CType::StructUnion(x), CType::StructUnion(y)) if x == y => true,
            (CType::Enum(x), CType::Enum(y)) if x == y => true,
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

    // Arithmetic types and pointer types are collectively called scalar types.
    pub fn is_scalar(&self) -> bool {
        use CType::*;
        match self {
            Int(_, _) => true,
            Bool => true,
            Float(_) => true,
            Enum(_) => true,
            Pointer(_) => true,
            _ => false,
        }
    }

    pub fn is_scalar_or_array(&self) -> bool {
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

    // Array and structure types are collectively called aggregate types.
    pub fn is_aggregate(&self) -> bool {
        use CType::*;
        match self {
            Array(_, _) => true,
            StructUnion(_) => true,
            _ => false,
        }
    }

    pub fn is_object(&self) -> bool {
        if let CType::StructUnion(_) = self {
            true
        } else {
            false
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

    pub fn is_pointer_to_void(&self) -> bool {
        match self {
            CType::Pointer(t) if t.t.is_void() => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        if let CType::Function { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn is_valist(&self) -> bool {
        if let CType::VaList = self {
            true
        } else {
            false
        }
    }

    pub fn is_same_struct_union(&self, other: &Self) -> bool {
        use CType::*;
        match (self, other) {
            (StructUnion(x), StructUnion(y)) => x == y,
            _ => false,
        }
    }

    pub fn is_complete(&self, scope: &NameScope) -> bool {
        match self {
            CType::Void => false,
            CType::Enum(id) => scope.get_enum(id).is_complete(),
            CType::StructUnion(id) => scope.get_struct_union(id).is_complete(),
            _ => true,
        }
    }

    pub fn is_packed_object(&self, scope: &NameScope) -> bool {
        if let CType::StructUnion(id) = self {
            scope.get_struct_union(id).is_packed()
        } else {
            false
        }
    }

    pub fn dereferences_to_complete(&self, scope: &NameScope) -> bool {
        match self {
            CType::Pointer(t) | CType::Array(t, _) => t.t.is_complete(scope),
            _ => false,
        }
    }

    pub fn get_anon_struct_or_union_id(&self) -> Option<&StructUnionIdentifier> {
        if let CType::StructUnion(su) = self {
            if su.name.is_none() {
                Some(su)
            } else {
                None
            }
        } else {
            None
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

    pub fn sizeof(
        &self,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<u32, ()> {
        match self {
            CType::Void | CType::Function { .. } => {
                ec.record_warning(CompileWarning::InvalidSizeof, span)?;
                Ok(1)
            }
            CType::Bool => Ok(machine::BOOL_SIZE as u32),
            CType::Int(s, _) => Ok(*s as u32),
            CType::Float(s) => Ok(*s as u32),
            CType::Pointer(_) => Ok(machine::PTR_SIZE as u32),
            CType::Array(t, Some(n)) => Ok(t.t.sizeof(scope, span, ec)? * *n),
            CType::Array(_, None) => Ok(machine::PTR_SIZE as u32),
            CType::StructUnion(su) => {
                let su = scope.get_struct_union(su);
                if !su.is_complete() {
                    ec.record_error(CompileError::SizeOfIncomplete(self.clone()), span)?;
                    unreachable!();
                }
                su.sizeof(scope, span, ec)
            }
            CType::Enum(_) => Ok(machine::INT_SIZE as u32),
            CType::VaList => {
                ec.record_error(
                    CompileError::Unimplemented("sizeof va_list".to_string()),
                    span,
                )?;
                unreachable!()
            }
        }
    }

    pub fn alignof(
        &self,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<u32, ()> {
        #[allow(unreachable_patterns)]
        match self {
            CType::Void | CType::Function { .. } => Ok(1),
            CType::Bool => Ok(BOOL_ALIGN),
            CType::Int(1, _) => Ok(1),
            CType::Int(SHORT_SIZE, _) => Ok(SHORT_ALIGN),
            CType::Int(INT_SIZE, _) => Ok(INT_ALIGN),
            CType::Int(LONG_SIZE, _) => Ok(LONG_ALIGN),
            CType::Int(LLONG_SIZE, _) => Ok(LLONG_ALIGN),
            CType::Int(_, _) => unreachable!(),
            CType::Float(s) => Ok(*s as u32),
            CType::Pointer(_) => Ok(PTR_ALIGN),
            CType::Array(t, _) => t.t.alignof(scope, span, ec),
            CType::StructUnion(su) => {
                let su = scope.get_struct_union(su);
                if !su.is_complete() {
                    ec.record_error(CompileError::SizeOfIncomplete(self.clone()), span)?;
                    unreachable!();
                }
                su.alignof(scope, span, ec)
            }
            CType::Enum(_) => Ok(machine::INT_ALIGN),
            CType::VaList => {
                ec.record_error(
                    CompileError::Unimplemented("alignof va_list".to_string()),
                    span,
                )?;
                unreachable!()
            }
        }
    }

    pub fn get_width_sign(&self) -> Option<(ir::Width, bool)> {
        match self {
            CType::Int(size, sign) => Some((ir::Width::new(*size), *sign)),
            CType::Float(size) => Some((ir::Width::new(*size), false)),
            CType::Bool => Some((ir::Width::BOOL_WIDTH, false)),
            CType::Array(_, _) | CType::Pointer(_) => Some((ir::Width::PTR_WIDTH, false)),
            CType::Enum(_) => Some((ir::Width::INT_WIDTH, true)),
            CType::StructUnion(_) => None,
            CType::VaList => Some((ir::Width::VA_LIST_WIDTH, false)),
            CType::Function { .. } => Some((ir::Width::PTR_WIDTH, false)),
            CType::Void => None,
        }
    }

    pub fn get_scalar_width(&self) -> Option<ir::Width> {
        match self {
            CType::Int(size, _) | CType::Float(size) => Some(ir::Width::new(*size)),
            CType::Bool => Some(ir::Width::BOOL_WIDTH),
            CType::Array(_, _) | CType::Pointer(_) => Some(ir::Width::PTR_WIDTH),
            CType::Enum(_) => Some(ir::Width::INT_WIDTH),
            CType::StructUnion(_) => None,
            CType::Function { .. } => None,
            CType::Void => None,
            CType::VaList => Some(ir::Width::VA_LIST_WIDTH),
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

    pub fn get_element_count(&self) -> Option<u32> {
        if let CType::Array(_, count) = self {
            *count
        } else {
            None
        }
    }
}

impl PartialEq for StructUnionIdentifier {
    fn eq(&self, other: &StructUnionIdentifier) -> bool {
        self.id == other.id
    }
}

impl PartialEq for EnumIdentifier {
    fn eq(&self, other: &EnumIdentifier) -> bool {
        self.id == other.id
    }
}

impl PartialEq for FunctionArgs {
    fn eq(&self, other: &FunctionArgs) -> bool {
        match (self, other) {
            (FunctionArgs::Empty, FunctionArgs::Empty) => true,
            (FunctionArgs::Void, FunctionArgs::Void) => true,
            (FunctionArgs::List(v1), FunctionArgs::List(v2)) => {
                v1.len() == v2.len()
                    && v1
                        .iter()
                        .zip(v2.iter())
                        .all(|((a1, _, _), (a2, _, _))| a1 == a2)
            }
            _ => false,
        }
    }
}

impl std::fmt::Display for StructUnionIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.kind {
            StructUnionKind::Struct => f.write_str("struct ")?,
            StructUnionKind::Union => f.write_str("union ")?,
        }
        match &self.name {
            None => f.write_str("<anonymous>"),
            Some(name) => f.write_str(&name),
        }
    }
}

impl std::fmt::Display for EnumIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str("enum ")?;
        match &self.name {
            None => f.write_str("<anonymous>"),
            Some(name) => f.write_str(&name),
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
            Int(_, _) => unreachable!(),
            Float(4) => f.write_str("float"),
            Float(8) => f.write_str("double"),
            Float(_) => unreachable!(),
            Pointer(inner) => write!(f, "{} *", inner),
            Array(inner, None) => write!(f, "{} []", inner),
            Array(inner, Some(n)) => write!(f, "{} [{}]", inner, n),
            StructUnion(id) => id.fmt(f),
            Enum(id) => id.fmt(f),
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
            VaList => f.write_str("__builtin_va_list"),
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
