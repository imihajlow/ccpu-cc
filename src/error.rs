use lang_c::span::Span;
use std::fmt::Formatter;

pub enum CompileError {
    TooManyErrors,
    Unimplemented(String),
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
}

pub enum CompileWarning {
    Unimplemented(String),
    ImplicitInt,
    EmptyDeclaration,
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
        }
    }
}
