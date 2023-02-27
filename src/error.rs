use lang_c::span::Span;
use std::fmt::Formatter;

use crate::{
    ctype::{CType, QualifiedType, StructUnionIdentifier},
    string::StringParseError,
};

#[derive(Debug, PartialEq)]
pub enum CompileError {
    TooManyErrors,
    Unimplemented(String),
    UnknownIdentifier(String),
    StaticAssertionFailed(String),
    // Errors concerning types in declarations
    MultipleStorageClasses,
    WrongStorageClass,
    MultipleTypes,
    MultipleSignSpecifiers,
    LongShortTogether,
    TypeTooLong,
    WrongModifiers(String),
    UnknownType(String),
    IncompatibleTypes(QualifiedType, QualifiedType),
    NamedVoidParameter,
    QualifiedVoidParameter,
    VoidParameter,
    NotAType(String),
    NotAVar(String),
    ArraySizeNotInteger(QualifiedType),
    ArraySizeNegative,
    MemberRedeclaration(String),
    // Global definition errors
    TypeRedefinition(String),
    TaggedTypedRedefinition(String),
    RedefinitionWithDifferentTag(String),
    ConflictingTypes(String),
    ConflictingStorageClass(String),
    TypedefInitialized,
    VarRedefinition(String),
    // Constant expression errors
    VariablesForbidden,
    CallsForbidden,
    NonConstInConstExpr,
    BadCast(String, String),
    AssignmentToConst,
    DivisionByZero,
    CharParseError(StringParseError),
    // General expression error
    ArithmeticTypeRequired,
    IntegerTypeRequired,
    ScalarTypeRequired,
    ArithmeticOrPointerTypeRequired(QualifiedType),
    PointerTypeRequired,
    BadTypesForOperator(String),
    CannotCompare(QualifiedType, QualifiedType),
    BadIndirection(QualifiedType),
    BadSubscripted,
    BadSubsript,
    NotAssignable,
    AssignmentToConstQualified(QualifiedType),
    SizeOfIncomplete(CType),
    IncompleteStruct(QualifiedType),
    PointersToIncompatible(QualifiedType, QualifiedType),
    NotAStruct(QualifiedType),
    NoSuchMember(StructUnionIdentifier, String),
    // Function call errors
    NotAFunction(QualifiedType),
    TooManyArguments(usize, usize),
    TooFewArguments(usize, usize),
    // Control flow errors
    InvalidBreak,
    InvalidContinue,
    NoReturnValue,
    UnexpectedRetrurnValue,
    UndeclaredLabel(String),
    LabelRedefinition(String),
    UnexpectedCase,
    UnexpectedDefault,
    DuplicateCase(i128),
    DuplicateDefault,
}

pub enum CompileWarning {
    Unimplemented(String),
    ImplicitInt,
    EmptyDeclaration,
    ShiftByNegative,
    LocalVarShadow(String),
    ExternVarInitialized(String),
    InvalidSizeof,
    IndexTooWide(QualifiedType),
    IncompatibleTypes(QualifiedType, QualifiedType),
    ImplicitArgumentTypes,
}

pub struct CompileErrorWithSpan(pub CompileError, pub Span);

pub struct ErrorCollector {
    errors: Vec<(CompileError, Span)>,
    warnings: Vec<(CompileWarning, Span)>,
}

impl ErrorCollector {
    pub fn new() -> Self {
        ErrorCollector {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn record_error(&mut self, error: CompileError, span: Span) -> Result<(), ()> {
        self.errors.push((error, span));
        Err(())
    }

    pub fn record_warning(&mut self, error: CompileWarning, span: Span) -> Result<(), ()> {
        self.warnings.push((error, span));
        Ok(())
    }

    pub fn get_error_count(&self) -> usize {
        self.errors.len()
    }

    pub fn get_warning_count(&self) -> usize {
        self.warnings.len()
    }

    pub fn print_issues(&self) {
        for (warn, span) in &self.warnings {
            println!("{:?}: warning: {}", span, warn);
        }
        for (err, span) in &self.errors {
            println!("{:?}: error: {}", span, err);
        }
    }

    #[cfg(test)]
    pub fn get_first_error(&self) -> Option<&(CompileError, Span)> {
        self.errors.first()
    }

    #[cfg(test)]
    pub fn get_first_warning(&self) -> Option<&(CompileWarning, Span)> {
        self.warnings.first()
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            CompileError::TooManyErrors => write!(f, "too many errors"),
            CompileError::Unimplemented(s) => write!(f, "unimplemented: {}", &s),
            CompileError::UnknownIdentifier(s) => write!(f, "unknown identifier: {}", &s),
            CompileError::StaticAssertionFailed(s) => {
                write!(f, "static assertion failed, message: {}", s)
            }
            CompileError::WrongStorageClass => f.write_str("wrong storage class"),
            CompileError::MultipleStorageClasses => {
                write!(f, "multiple storage classes in declaration specifiers")
            }
            CompileError::UnknownType(s) => write!(f, "unknown type '{}'", &s),
            CompileError::MultipleTypes => {
                write!(f, "two or more data types in declaration specifiers")
            }
            CompileError::MultipleSignSpecifiers => {
                write!(f, "only one signed/unsigned is allowed")
            }
            CompileError::LongShortTogether => write!(f, "long and short cannot be together"),
            CompileError::TypeTooLong => write!(f, "too long"),
            CompileError::WrongModifiers(t) => write!(f, "wrong modifiers for {}", t),
            CompileError::TypeRedefinition(t) => write!(f, "type '{}' is redefined", t),
            CompileError::TaggedTypedRedefinition(t) => write!(f, "'{}' is redefined", t),
            CompileError::RedefinitionWithDifferentTag(t) => write!(
                f,
                "use of '{}' with tag type that does not match previous declaration",
                t
            ),
            CompileError::ConflictingTypes(s) => write!(f, "conflicting types for {}", s),
            CompileError::ConflictingStorageClass(s) => {
                write!(f, "conflicting storage classes for {}", s)
            }
            CompileError::VariablesForbidden => f.write_str("variables are forbidden here"),
            CompileError::CallsForbidden => f.write_str("function calls are forbidden here"),
            CompileError::NonConstInConstExpr => {
                f.write_str("non-const value in a constant expression")
            }
            CompileError::BadCast(t1, t2) => write!(f, "bad cast from '{}' to '{}'", t1, t2),
            CompileError::TypedefInitialized => f.write_str("typedef is initialized"),
            CompileError::AssignmentToConst => f.write_str("assignment to a constant expression"),
            CompileError::ArithmeticTypeRequired => {
                f.write_str("an arithmetic type is required here")
            }
            CompileError::IntegerTypeRequired => f.write_str("an integer type is required here"),
            CompileError::ScalarTypeRequired => f.write_str("a scalar type is required here"),
            CompileError::PointerTypeRequired => f.write_str("a pointer is required here"),
            CompileError::BadTypesForOperator(op) => write!(f, "bad types for operator '{}'", op),
            CompileError::DivisionByZero => f.write_str("division by zero"),
            CompileError::CannotCompare(t1, t2) => {
                write!(f, "cannot compare '{}' and '{}'", t1, t2)
            }
            CompileError::IncompatibleTypes(t1, t2) => {
                write!(f, "incompatible types '{}' and '{}'", t1, t2)
            }
            CompileError::CharParseError(e) => {
                write!(f, "error while parsing character literal: {}", e)
            }
            CompileError::NamedVoidParameter => f.write_str("argument may not have 'void' type"),
            CompileError::QualifiedVoidParameter => {
                f.write_str("'void' as parameter must not have type qualifiers")
            }
            CompileError::VoidParameter => {
                f.write_str("'void' must be the first and only parameter if specified")
            }
            CompileError::VarRedefinition(s) => write!(f, "redefinition of '{}'", s),
            CompileError::NotAType(s) => write!(f, "'{}' is not a type, but a variable", s),
            CompileError::NotAVar(s) => write!(f, "'{}' is not a variable, but a type alias", s),
            CompileError::BadIndirection(t) => {
                write!(f, "indirection requires pointer operand ('{}' invalid)", t)
            }
            CompileError::NotAssignable => write!(f, "expression is not assignable"),
            CompileError::BadSubscripted => {
                write!(f, "subscripted value is not an array or pointer")
            }
            CompileError::BadSubsript => write!(f, "array subscript is not an integer"),
            CompileError::AssignmentToConstQualified(t) => write!(
                f,
                "cannot assign to a location with const-qualified type '{}'",
                t
            ),
            CompileError::ArraySizeNotInteger(t) => {
                write!(f, "size of array has non-integer type '{}'", t)
            }
            CompileError::ArraySizeNegative => write!(f, "negative array size"),
            CompileError::MemberRedeclaration(s) => write!(f, "member '{}' is redeclared", s),
            CompileError::SizeOfIncomplete(t) => {
                write!(f, "size of an incompete type '{}' is unknown", t)
            }
            CompileError::IncompleteStruct(t) => {
                write!(f, "incomplete definition of type '{}'", t)
            }
            CompileError::NoSuchMember(t, s) => {
                write!(f, "no member named '{}' in '{}'", s, t)
            }
            CompileError::PointersToIncompatible(t1, t2) => write!(
                f,
                "'{}' and '{}' are not pointers to compatible types",
                t1, t2
            ),
            CompileError::NotAStruct(t) => write!(f, "type '{}' is not a struct or union", t),
            CompileError::ArithmeticOrPointerTypeRequired(t) => write!(
                f,
                "used type '{}' where arithmetic or pointer type is required",
                t
            ),
            CompileError::NotAFunction(t) => write!(
                f,
                "called object type '{}' is not a function or function pointer",
                t
            ),
            CompileError::TooManyArguments(got, expected) => write!(
                f,
                "too many arguments to function call, expected {}, have {}",
                expected, got
            ),
            CompileError::TooFewArguments(got, expected) => write!(
                f,
                "too few arguments to function call, expected {}, have {}",
                expected, got
            ),
            CompileError::InvalidBreak => {
                f.write_str("'break' statement not in loop or switch statement")
            }
            CompileError::InvalidContinue => {
                f.write_str("'continue' statement not in loop statement")
            }
            CompileError::NoReturnValue => f.write_str("non-void function should return a value"),
            CompileError::UnexpectedRetrurnValue => {
                f.write_str("void function should not return a value")
            }
            CompileError::UndeclaredLabel(l) => write!(f, "use of undeclared label '{}'", l),
            CompileError::LabelRedefinition(l) => write!(f, "redefinition of label '{}'", l),
            CompileError::UnexpectedCase => write!(f, "'case' statement not in switch statement"),
            CompileError::UnexpectedDefault => {
                write!(f, "'default' statement not in switch statement")
            }
            CompileError::DuplicateCase(x) => write!(f, "duplicate case value '{}'", x),
            CompileError::DuplicateDefault => write!(f, "multiple default labels in one switch"),
        }
    }
}

impl std::fmt::Display for CompileWarning {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            CompileWarning::Unimplemented(s) => write!(f, "unimplemented: {}", &s),
            CompileWarning::ImplicitInt => f.write_str("implicit int"),
            CompileWarning::EmptyDeclaration => {
                f.write_str("empty declration doesn't declare anything")
            }
            CompileWarning::ShiftByNegative => f.write_str("shift by a negative value"),
            CompileWarning::LocalVarShadow(s) => {
                write!(f, "declaration of '{}' shadows a local variable", s)
            }
            CompileWarning::ExternVarInitialized(s) => {
                write!(f, "extern variable '{}' has an initializer", s)
            }
            CompileWarning::InvalidSizeof => f.write_str("invalid application of sizeof"),
            CompileWarning::IndexTooWide(t) => {
                write!(f, "pointer arithmetics with '{}': data may be lost", t)
            }
            CompileWarning::IncompatibleTypes(t1, t2) => {
                write!(f, "incompatible types '{}' and '{}'", t1, t2)
            }
            CompileWarning::ImplicitArgumentTypes => {
                f.write_str("empty arguments list in function declaration")
            }
        }
    }
}
