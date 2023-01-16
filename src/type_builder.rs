use crate::ctype::CType;
use crate::ctype::QualifiedType;
use crate::ctype::Qualifiers;
use crate::ctype::DOUBLE_TYPE;
use crate::ctype::FLOAT_TYPE;
use crate::ctype::LDOUBLE_TYPE;
use crate::error::CompileError;
use crate::error::CompileWarning;
use crate::error::ErrorCollector;
use crate::machine;
use crate::type_registry::TypeRegistry;
use lang_c::span::Node;
use lang_c::span::Span;
use std::fmt::Formatter;

/**
 * Starts building a type from one or more lang_c::ast::DeclarationSpecifier.
 */
pub struct TypeBuilder {
    base_type: Option<BaseType>,
    modifier: TypeModifier,
    sign: SignModifier,
    qualifiers: Qualifiers,
}

/**
 * Continues building a type.
 */
pub struct TypeBuilderStage2 {
    base_type: QualifiedType,
}

enum BaseType {
    Void,
    Int,
    Char,
    Float,
    Double,
    Bool,
    Alias(String, QualifiedType),
}

enum TypeModifier {
    None,
    Short,
    Long,
    LongLong,
}

enum SignModifier {
    Default,
    Signed,
    Unsigned,
}

impl TypeBuilder {
    pub fn new() -> Self {
        Self {
            base_type: None,
            modifier: TypeModifier::None,
            sign: SignModifier::Default,
            qualifiers: Qualifiers::empty(),
        }
    }

    pub fn add_type_qualifier_node(
        &mut self,
        qual: Node<lang_c::ast::TypeQualifier>,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        self.qualifiers |= convert_qualifier_node(qual, ec)?;
        Ok(())
    }

    pub fn add_type_specifier_node(
        &mut self,
        spec: Node<lang_c::ast::TypeSpecifier>,
        reg: &TypeRegistry,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        use lang_c::ast::TypeSpecifier;
        let span = spec.span;
        let spec = spec.node;
        match spec {
            TypeSpecifier::Void => self.set_base_type(BaseType::Void, span, ec)?,
            TypeSpecifier::Char => self.set_base_type(BaseType::Char, span, ec)?,
            TypeSpecifier::Int => self.set_base_type(BaseType::Int, span, ec)?,
            TypeSpecifier::Float => self.set_base_type(BaseType::Float, span, ec)?,
            TypeSpecifier::Double => self.set_base_type(BaseType::Double, span, ec)?,
            TypeSpecifier::Bool => self.set_base_type(BaseType::Bool, span, ec)?,
            TypeSpecifier::Signed => self.set_signed(true, span, ec)?,
            TypeSpecifier::Unsigned => self.set_signed(false, span, ec)?,
            TypeSpecifier::Long => self.set_long(span, ec)?,
            TypeSpecifier::Short => self.set_short(span, ec)?,
            TypeSpecifier::TypedefName(n) => self.set_alias(n.node.name, n.span, reg, ec)?,
            TypeSpecifier::Struct(_) => todo!(),
            TypeSpecifier::Enum(_) => todo!(),
            TypeSpecifier::TypeOf(_) => todo!(),
            TypeSpecifier::Atomic(_) => unimplemented!("atomic"),
            TypeSpecifier::Complex => unimplemented!("complex"),
            TypeSpecifier::TS18661Float(_) => unimplemented!("TS 18551 float"),
        }
        Ok(())
    }

    pub fn stage2(&mut self, span: Span, ec: &mut ErrorCollector) -> Result<TypeBuilderStage2, ()> {
        if self.base_type.is_none() {
            ec.record_warning(CompileWarning::ImplicitInt, span)?;
        }

        let signed = match self.sign {
            SignModifier::Signed => true,
            SignModifier::Unsigned => false,
            SignModifier::Default => {
                if let Some(BaseType::Char) = self.base_type {
                    machine::CHAR_SIGNED
                } else {
                    // int is signed by default
                    true
                }
            }
        };

        let extra_qualifiers = match &self.base_type {
            Some(BaseType::Alias(_, t)) => t.qualifiers,
            _ => Qualifiers::empty(),
        };
        let t = match self.base_type.take() {
            None | Some(BaseType::Int) => {
                let size = match self.modifier {
                    TypeModifier::None => machine::INT_SIZE,
                    TypeModifier::Short => machine::SHORT_SIZE,
                    TypeModifier::Long => machine::LONG_SIZE,
                    TypeModifier::LongLong => machine::LLONG_SIZE,
                };
                CType::Int(size, signed)
            }
            Some(BaseType::Void) => CType::Void,
            Some(BaseType::Char) => CType::Int(1, signed),
            Some(BaseType::Float) => FLOAT_TYPE,
            Some(BaseType::Double) => match self.modifier {
                TypeModifier::None => DOUBLE_TYPE,
                TypeModifier::Long => LDOUBLE_TYPE,
                _ => unreachable!(),
            },
            Some(BaseType::Bool) => CType::Bool,
            Some(BaseType::Alias(_, t)) => t.t,
        };

        Ok(TypeBuilderStage2 {
            base_type: QualifiedType {
                t,
                qualifiers: self.qualifiers | extra_qualifiers,
            },
        })
    }

    fn set_base_type(
        &mut self,
        t: BaseType,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        if self.base_type.is_none() {
            self.base_type = Some(t);
            self.check_consistency(span, ec)
        } else {
            ec.record_error(CompileError::MultipleTypes, span)?;
            Err(())
        }
    }

    fn set_signed(&mut self, signed: bool, span: Span, ec: &mut ErrorCollector) -> Result<(), ()> {
        if let SignModifier::Default = self.sign {
            self.sign = match signed {
                true => SignModifier::Signed,
                false => SignModifier::Unsigned,
            };
            self.check_consistency(span, ec)
        } else {
            ec.record_error(CompileError::MultipleSignSpecifiers, span)?;
            Err(())
        }
    }

    fn set_short(&mut self, span: Span, ec: &mut ErrorCollector) -> Result<(), ()> {
        if let TypeModifier::None = self.modifier {
            self.modifier = TypeModifier::Short;
            self.check_consistency(span, ec)
        } else {
            ec.record_error(CompileError::LongShortTogether, span)?;
            Err(())
        }
    }

    fn set_long(&mut self, span: Span, ec: &mut ErrorCollector) -> Result<(), ()> {
        match self.modifier {
            TypeModifier::None => self.modifier = TypeModifier::Long,
            TypeModifier::Long => self.modifier = TypeModifier::LongLong,
            TypeModifier::Short => {
                ec.record_error(CompileError::LongShortTogether, span)?;
                return Err(());
            }
            TypeModifier::LongLong => {
                ec.record_error(CompileError::TypeTooLong, span)?;
                return Err(());
            }
        }
        self.check_consistency(span, ec)
    }

    fn set_alias(
        &mut self,
        name: String,
        span: Span,
        reg: &TypeRegistry,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let bt = match reg.lookup_alias(&name) {
            Some(t) => BaseType::Alias(name, t),
            None => {
                ec.record_error(CompileError::UnknownType(name), span)?;
                return Err(());
            }
        };
        self.set_base_type(bt, span, ec)
    }

    fn check_consistency(&self, span: Span, ec: &mut ErrorCollector) -> Result<(), ()> {
        match &self.base_type {
            None => Ok(()),
            Some(base_type) => match base_type {
                BaseType::Int => Ok(()),
                BaseType::Void | BaseType::Float | BaseType::Bool => {
                    match self.modifier {
                        TypeModifier::None => (),
                        _ => {
                            ec.record_error(
                                CompileError::WrongModifiers(format!("{}", &base_type)),
                                span,
                            )?;
                            return Err(());
                        }
                    };
                    match self.sign {
                        SignModifier::Default => (),
                        _ => {
                            ec.record_error(
                                CompileError::WrongModifiers(format!("{}", &base_type)),
                                span,
                            )?;
                            return Err(());
                        }
                    }
                    Ok(())
                }
                BaseType::Char => {
                    match self.modifier {
                        TypeModifier::None => (),
                        _ => {
                            ec.record_error(
                                CompileError::WrongModifiers(format!("{}", &base_type)),
                                span,
                            )?;
                            return Err(());
                        }
                    };
                    Ok(())
                }
                BaseType::Double => {
                    match self.modifier {
                        TypeModifier::None | TypeModifier::Long => (),
                        _ => {
                            ec.record_error(
                                CompileError::WrongModifiers(format!("{}", &base_type)),
                                span,
                            )?;
                            return Err(());
                        }
                    };
                    match self.sign {
                        SignModifier::Default => (),
                        _ => {
                            ec.record_error(
                                CompileError::WrongModifiers(format!("{}", &base_type)),
                                span,
                            )?;
                            return Err(());
                        }
                    }
                    Ok(())
                }
                BaseType::Alias(name, _) => match (&self.modifier, &self.sign) {
                    (TypeModifier::None, SignModifier::Default) => Ok(()),
                    _ => {
                        ec.record_error(
                            CompileError::WrongModifiers(format!("typedef {}", &name)),
                            span,
                        )?;
                        Err(())
                    }
                },
            },
        }
    }
}

impl TypeBuilderStage2 {
    pub fn add_derived_declarator_node(
        &mut self,
        dd: Node<lang_c::ast::DerivedDeclarator>,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        use lang_c::ast::{DerivedDeclarator, PointerQualifier};
        match dd.node {
            DerivedDeclarator::Pointer(pqs) => {
                let mut qualifiers = Qualifiers::empty();
                for pq in pqs {
                    match pq.node {
                        PointerQualifier::TypeQualifier(tq) => {
                            qualifiers |= convert_qualifier_node(tq, ec)?;
                        }
                        PointerQualifier::Extension(_) => unimplemented!(),
                    }
                }
                self.base_type.wrap_pointer(qualifiers);
            }
            _ => todo!(),
        }
        Ok(())
    }

    pub fn finalize(self) -> QualifiedType {
        self.base_type
    }
}

fn convert_qualifier_node(
    q: Node<lang_c::ast::TypeQualifier>,
    ec: &mut ErrorCollector,
) -> Result<Qualifiers, ()> {
    use lang_c::ast::TypeQualifier;
    let span = q.span;
    let qual = match q.node {
        TypeQualifier::Const => Qualifiers::CONST,
        TypeQualifier::Restrict => Qualifiers::RESTRICT,
        TypeQualifier::Volatile => Qualifiers::VOLATILE,
        TypeQualifier::Atomic => Qualifiers::ATOMIC,
        _ => {
            ec.record_warning(
                CompileWarning::Unimplemented("unknown qualifier".to_string()),
                span,
            )?;
            Qualifiers::empty()
        }
    };
    Ok(qual)
}

impl std::fmt::Display for BaseType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            BaseType::Void => f.write_str("void"),
            BaseType::Char => f.write_str("char"),
            BaseType::Int => f.write_str("int"),
            BaseType::Float => f.write_str("float"),
            BaseType::Double => f.write_str("double"),
            BaseType::Bool => f.write_str("_Bool"),
            BaseType::Alias(name, t) => write!(f, "{} (aka {})", name, t),
        }
    }
}
