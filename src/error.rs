use lang_c::span::Span;
use std::fmt::Formatter;

pub enum CompileError {
    TooManyErrors,
    Unimplemented(String),
    UnknownIdentifier(String),
    // Errors concerning types in declarations
    MultipleStorageClasses,
    WrongStorageClass,
    MultipleTypes,
    MultipleSignSpecifiers,
    LongShortTogether,
    TypeTooLong,
    WrongModifiers(String),
    UnknownType(String),
    // Global definition errors
    TypeRedefinition(String),
    ConflictingTypes(String),
    ConflictingStorageClass(String),
    TypedefInitialized,
    // Constant expression errors
    VariablesForbidden,
    NonConstInConstExpr,
    BadCast(String, String),
    AssignmentToConst,
    DivisionByZero,
    // General expression error
    ArithmeticTypeRequired,
    IntegerTypeRequired,
    ScalarTypeRequired,
    BadTypesForOperator(String),
}

pub enum CompileWarning {
    Unimplemented(String),
    ImplicitInt,
    EmptyDeclaration,
    ShiftByNegative,
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
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            CompileError::TooManyErrors => write!(f, "too many errors"),
            CompileError::Unimplemented(s) => write!(f, "unimplemented: {}", &s),
            CompileError::UnknownIdentifier(s) => write!(f, "unknown identifier: {}", &s),
            CompileError::WrongStorageClass => f.write_str("wrong storage class"),
            CompileError::MultipleStorageClasses => {
                write!(f, "multiple storage classes in declaration specifiers")
            }
            CompileError::UnknownType(s) => write!(f, "unknown type `{}'", &s),
            CompileError::MultipleTypes => {
                write!(f, "two or more data types in declaration specifiers")
            }
            CompileError::MultipleSignSpecifiers => {
                write!(f, "only one signed/unsigned is allowed")
            }
            CompileError::LongShortTogether => write!(f, "long and short cannot be together"),
            CompileError::TypeTooLong => write!(f, "too long"),
            CompileError::WrongModifiers(t) => write!(f, "wrong modifiers for {}", t),
            CompileError::TypeRedefinition(t) => write!(f, "type `{}' is redefined", t),
            CompileError::ConflictingTypes(s) => write!(f, "conflicting types for {}", s),
            CompileError::ConflictingStorageClass(s) => {
                write!(f, "conflicting storage classes for {}", s)
            }
            CompileError::VariablesForbidden => f.write_str("variables are forbidden here"),
            CompileError::NonConstInConstExpr => {
                f.write_str("non-const value in a constant expression")
            }
            CompileError::BadCast(t1, t2) => write!(f, "bad cast from `{}' to `{}'", t1, t2),
            CompileError::TypedefInitialized => f.write_str("typedef is initialized"),
            CompileError::AssignmentToConst => f.write_str("assignment to a constant expression"),
            CompileError::ArithmeticTypeRequired => {
                f.write_str("an arithmetic type is required here")
            }
            CompileError::IntegerTypeRequired => f.write_str("an integer type is required here"),
            CompileError::ScalarTypeRequired => f.write_str("a scalar type is required here"),
            CompileError::BadTypesForOperator(op) => write!(f, "bad types for operator `{}`", op),
            CompileError::DivisionByZero => f.write_str("division by zero"),
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
        }
    }
}
