use crate::attribute::Attribute;
use crate::attribute::Attributes;
use crate::builtin::get_builtin_type;
use crate::constant;
use crate::constant::check_static_assert;
use crate::ctype::CType;
use crate::ctype::EnumIdentifier;
use crate::ctype::FunctionArgs;
use crate::ctype::QualifiedType;
use crate::ctype::Qualifiers;
use crate::ctype::StructUnionIdentifier;
use crate::ctype::DOUBLE_TYPE;
use crate::ctype::FLOAT_TYPE;
use crate::ctype::LDOUBLE_TYPE;
use crate::error::CompileError;
use crate::error::CompileWarning;
use crate::error::ErrorCollector;
use crate::machine;
use crate::name_scope::NameScope;

use lang_c::ast::DeclarationSpecifier;
use lang_c::ast::Ellipsis;
use lang_c::ast::EnumType;
use lang_c::ast::Extension;
use lang_c::ast::FunctionSpecifier;
use lang_c::ast::SpecifierQualifier;
use lang_c::ast::StorageClassSpecifier;
use lang_c::ast::StructType;
use lang_c::ast::TypeName;
use lang_c::span::Node;
use lang_c::span::Span;
use std::collections::HashMap;
use std::fmt::Formatter;

/**
 * Starts building a type from one or more lang_c::ast::DeclarationSpecifier.
 */
pub struct TypeBuilder {
    base_type: Option<BaseType>,
    modifier: TypeModifier,
    sign: SignModifier,
    qualifiers: Qualifiers,
    attrs: Attributes,
}

/**
 * Continues building a type.
 */
pub struct TypeBuilderStage2 {
    base_type: QualifiedType,
    attrs: Attributes,
}

pub struct ExtraSpecifiers {
    pub is_inline: bool,
    pub is_noreturn: bool,
}

enum BaseType {
    Void,
    Int,
    Char,
    Float,
    Double,
    Bool,
    Alias(String, QualifiedType),
    StructUnion(StructUnionIdentifier),
    Enum(EnumIdentifier),
    Builtin(CType),
}

#[derive(PartialEq, Eq)]
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
            attrs: Attributes::new(),
        }
    }

    pub fn new_from_specifiers(
        specs: Vec<Node<DeclarationSpecifier>>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(Self, Option<Node<StorageClassSpecifier>>, ExtraSpecifiers), ()> {
        let mut extra = ExtraSpecifiers {
            is_inline: false,
            is_noreturn: false,
        };
        let mut type_builder = Self::new();
        let mut storage_class = None;
        // Process extension specifiers first
        for declspec in &specs {
            if let DeclarationSpecifier::Extension(ve) = &declspec.node {
                for ext in ve {
                    type_builder.add_extension_node(ext, ec)?;
                }
            }
        }
        for declspec in specs {
            match declspec.node {
                DeclarationSpecifier::StorageClass(stc) => {
                    if storage_class.is_none() {
                        storage_class = Some(stc);
                    } else {
                        ec.record_error(CompileError::MultipleStorageClasses, stc.span)?;
                    }
                }
                DeclarationSpecifier::Function(f) => match f.node {
                    FunctionSpecifier::Inline => extra.is_inline = true,
                    FunctionSpecifier::Noreturn => extra.is_noreturn = true,
                },
                DeclarationSpecifier::TypeSpecifier(ts) => {
                    type_builder.add_type_specifier_node(ts, scope, ec)?
                }
                DeclarationSpecifier::TypeQualifier(q) => {
                    type_builder.add_type_qualifier_node(q, ec)?
                }
                DeclarationSpecifier::Alignment(_) => {
                    ec.record_error(
                        CompileError::Unimplemented("alignment specifier".to_string()),
                        declspec.span,
                    )?;
                    unreachable!()
                }
                DeclarationSpecifier::Extension(_) => {
                    // already processed
                }
            }
        }
        Ok((type_builder, storage_class, extra))
    }

    pub fn new_from_specifiers_qualifiers(
        sqs: Vec<Node<SpecifierQualifier>>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<Self, ()> {
        let mut r = Self::new();
        for sq in &sqs {
            if let SpecifierQualifier::Extension(vext) = &sq.node {
                for ext in vext {
                    r.add_extension_node(ext, ec)?;
                }
            }
        }
        for sq in sqs {
            r.add_specifier_qualifier_node(sq, scope, ec)?;
        }
        Ok(r)
    }

    pub fn add_type_qualifier_node(
        &mut self,
        qual: Node<lang_c::ast::TypeQualifier>,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        self.qualifiers |= convert_qualifier_node(qual, ec)?;
        Ok(())
    }

    fn add_type_specifier_node(
        &mut self,
        spec: Node<lang_c::ast::TypeSpecifier>,
        scope: &mut NameScope,
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
            TypeSpecifier::TypedefName(n) => {
                if let Some(t) = get_builtin_type(&n.node.name) {
                    self.set_base_type(BaseType::Builtin(t), span, ec)?
                } else {
                    self.set_alias(n.node.name, n.span, scope, ec)?
                }
            }
            TypeSpecifier::Struct(s) => self.set_struct(s, scope, ec)?,
            TypeSpecifier::Enum(e) => self.set_enum(e, scope, ec)?,
            TypeSpecifier::TypeOf(_) => todo!(),
            TypeSpecifier::Atomic(_) => {
                ec.record_error(CompileError::Unimplemented("atomic".to_string()), span)?;
                unreachable!()
            }
            TypeSpecifier::Complex => {
                ec.record_error(CompileError::Unimplemented("complex".to_string()), span)?;
                unreachable!()
            }
            TypeSpecifier::TS18661Float(_) => {
                ec.record_error(
                    CompileError::Unimplemented("TS 18551 float".to_string()),
                    span,
                )?;
                unreachable!()
            }
        }
        Ok(())
    }

    fn add_extension_node(
        &mut self,
        ext: &Node<lang_c::ast::Extension>,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let span = ext.span;
        let attr = match &ext.node {
            Extension::Attribute(a) => Attribute::parse(a.clone(), span, ec)?,
            Extension::AsmLabel(_) => {
                ec.record_warning(CompileWarning::Unimplemented("asm label".to_string()), span)?;
                None
            }
            Extension::AvailabilityAttribute(_) => {
                ec.record_warning(
                    CompileWarning::Unimplemented("availability attribute".to_string()),
                    span,
                )?;
                None
            }
        };
        if let Some(attr) = attr {
            self.attrs.add_attribute(attr, span, ec)?;
        }

        Ok(())
    }

    fn add_specifier_qualifier_node(
        &mut self,
        sq: Node<lang_c::ast::SpecifierQualifier>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let sq = sq.node;
        match sq {
            SpecifierQualifier::TypeQualifier(q) => self.add_type_qualifier_node(q, ec),
            SpecifierQualifier::TypeSpecifier(spec) => {
                self.add_type_specifier_node(spec, scope, ec)
            }
            SpecifierQualifier::Extension(ve) => {
                for ext in ve {
                    self.add_extension_node(&ext, ec)?;
                }
                Ok(())
            }
        }
    }

    pub fn stage2(&self, span: Span, ec: &mut ErrorCollector) -> Result<TypeBuilderStage2, ()> {
        if self.base_type.is_none() && self.modifier == TypeModifier::None {
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
        let t = match &self.base_type {
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
            Some(BaseType::Alias(_, t)) => t.t.clone(),
            Some(BaseType::StructUnion(id)) => CType::StructUnion(id.clone()),
            Some(BaseType::Enum(id)) => CType::Enum(id.clone()),
            Some(BaseType::Builtin(t)) => t.clone(),
        };

        Ok(TypeBuilderStage2 {
            base_type: QualifiedType {
                t,
                qualifiers: self.qualifiers | extra_qualifiers,
            },
            attrs: self.attrs.clone(),
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
        scope: &NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let t = scope.get_type_alias(&name, span, ec)?;
        self.set_base_type(BaseType::Alias(name, t.clone()), span, ec)
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
                BaseType::Alias(_, _)
                | BaseType::StructUnion(_)
                | BaseType::Enum(_)
                | BaseType::Builtin(_) => match (&self.modifier, &self.sign) {
                    (TypeModifier::None, SignModifier::Default) => Ok(()),
                    _ => {
                        ec.record_error(
                            CompileError::WrongModifiers(format!("{}", &base_type)),
                            span,
                        )?;
                        Err(())
                    }
                },
            },
        }
    }

    fn set_struct(
        &mut self,
        s: Node<StructType>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        use lang_c::ast::{StructDeclaration, StructKind};
        let span = s.span;
        let kind = s.node.kind.node;
        let name = s.node.identifier.map(|id| id.node.name);
        let members = if let Some(v) = s.node.declarations {
            let mut members = Vec::new();
            let mut used_names = HashMap::new();
            for m in v {
                match m.node {
                    StructDeclaration::StaticAssert(sa) => check_static_assert(sa, scope, ec)?,
                    StructDeclaration::Field(f) => {
                        let type_builder = TypeBuilder::new_from_specifiers_qualifiers(
                            f.node.specifiers,
                            scope,
                            ec,
                        )?;
                        if !f.node.declarators.is_empty() {
                            for decl in f.node.declarators {
                                if decl.node.bit_width.is_some() {
                                    ec.record_error(
                                        CompileError::Unimplemented("bit width".to_string()),
                                        m.span,
                                    )?;
                                    unreachable!()
                                }
                                let stage2 = type_builder.stage2(decl.span, ec)?;
                                let (name, t, attrs) = if let Some(decl) = decl.node.declarator {
                                    stage2.process_declarator_node(decl, scope, ec)?
                                } else {
                                    let (t, attrs) = stage2.finalize();
                                    (None, t, attrs)
                                };
                                if attrs.is_packed() {
                                    ec.record_warning(
                                        CompileWarning::AttributeIgnored("packed".to_string()),
                                        decl.span,
                                    )?;
                                }
                                if attrs.get_section().is_some() {
                                    ec.record_warning(
                                        CompileWarning::AttributeIgnored("section".to_string()),
                                        decl.span,
                                    )?;
                                }
                                if let Some(name) = name {
                                    if used_names.insert(name.to_string(), decl.span).is_some() {
                                        ec.record_error(
                                            CompileError::MemberRedeclaration(name),
                                            decl.span,
                                        )?;
                                        unreachable!();
                                    }
                                    members.push((Some(name), t))
                                } else {
                                    if let Some(tti) = t.t.get_anon_struct_or_union_id() {
                                        let inner_names =
                                            scope.get_struct_union(tti).collect_member_names(scope);
                                        for name in inner_names.iter() {
                                            if let Some(span) =
                                                used_names.insert(name.to_string(), decl.span)
                                            {
                                                ec.record_error(
                                                    CompileError::MemberRedeclaration(
                                                        name.to_string(),
                                                    ),
                                                    span,
                                                )?;
                                                unreachable!();
                                            }
                                        }
                                        members.push((None, t))
                                    } else {
                                        ec.record_warning(
                                            CompileWarning::EmptyDeclaration,
                                            decl.span,
                                        )?;
                                    }
                                }
                            }
                        } else {
                            let (t, attrs) = type_builder.stage2(f.span, ec)?.finalize();
                            if attrs.is_packed() {
                                ec.record_warning(
                                    CompileWarning::AttributeIgnored("packed".to_string()),
                                    f.span,
                                )?;
                            }
                            if attrs.get_section().is_some() {
                                ec.record_warning(
                                    CompileWarning::AttributeIgnored("section".to_string()),
                                    f.span,
                                )?;
                            }
                            if let Some(tti) = t.t.get_anon_struct_or_union_id() {
                                let inner_names =
                                    scope.get_struct_union(tti).collect_member_names(scope);
                                for name in inner_names.iter() {
                                    if let Some(span) = used_names.insert(name.to_string(), f.span)
                                    {
                                        ec.record_error(
                                            CompileError::MemberRedeclaration(name.to_string()),
                                            span,
                                        )?;
                                        unreachable!();
                                    }
                                }
                                members.push((None, t))
                            } else {
                                ec.record_warning(CompileWarning::EmptyDeclaration, f.span)?;
                            }
                        }
                    }
                }
            }
            Some(members)
        } else {
            None
        };
        let t = match kind {
            StructKind::Struct => {
                scope.declare_struct(name, members, self.attrs.is_packed(), span, ec)?
            }
            StructKind::Union => {
                scope.declare_union(name, members, self.attrs.is_packed(), span, ec)?
            }
        };
        self.set_base_type(BaseType::StructUnion(t), span, ec)
    }

    fn set_enum(
        &mut self,
        e: Node<EnumType>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let mut cur_value = 0;
        let mut enum_values = Vec::new();
        let mut const_values = Vec::new();
        let span = e.span;
        for enumerator in e.node.enumerators {
            if let Some(expr) = enumerator.node.expression {
                let expr_span = expr.span;
                let c = constant::compute_constant_expr(*expr, true, scope, ec)?;
                if !c.t.t.is_integer() {
                    ec.record_error(CompileError::IntegerTypeRequired, expr_span)?;
                    unreachable!();
                }
                cur_value = c.unwrap_integer();
            }
            let name = enumerator.node.identifier.node.name;
            enum_values.push((name.clone(), cur_value));
            const_values.push((name, cur_value, enumerator.span));
            cur_value += 1;
        }
        let name = e.node.identifier.map(|n| n.node.name);
        let enum_values = if enum_values.is_empty() {
            None
        } else {
            Some(enum_values)
        };
        let id = scope.declare_enum(name, enum_values, span, ec)?;
        for (name, value, span) in const_values {
            scope.declare_int_constant(
                &name,
                QualifiedType {
                    t: CType::Enum(id.clone()),
                    qualifiers: Qualifiers::empty(),
                },
                value,
                span,
                ec,
            )?;
        }
        self.set_base_type(BaseType::Enum(id), span, ec)
    }
}

impl TypeBuilderStage2 {
    /**
     * Recursively continue building the type and return the name, the type and attributes.
     */
    pub fn process_declarator_node(
        mut self,
        declarator: Node<lang_c::ast::Declarator>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<(Option<String>, QualifiedType, Attributes), ()> {
        use lang_c::ast::DeclaratorKind;
        let Node {
            node: declarator, ..
        } = declarator;

        for derived in declarator.derived {
            self.add_derived_declarator_node(derived, scope, ec)?;
        }

        for ext in declarator.extensions {
            self.add_extension_node(&ext, ec)?;
        }

        let kind = declarator.kind.node;
        match kind {
            DeclaratorKind::Abstract | DeclaratorKind::Identifier(_) => {
                let (qt, attrs) = self.finalize();
                match kind {
                    DeclaratorKind::Abstract => Ok((None, qt, attrs)),
                    DeclaratorKind::Identifier(id) => Ok((Some(id.node.name), qt, attrs)),
                    DeclaratorKind::Declarator(_) => unreachable!(),
                }
            }
            DeclaratorKind::Declarator(decl) => self.process_declarator_node(*decl, scope, ec),
        }
    }

    pub fn finalize(self) -> (QualifiedType, Attributes) {
        (self.base_type, self.attrs)
    }

    fn add_extension_node(
        &mut self,
        ext: &Node<lang_c::ast::Extension>,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let span = ext.span;
        let attr = match &ext.node {
            Extension::Attribute(a) => Attribute::parse(a.clone(), span, ec)?,
            Extension::AsmLabel(_) => {
                ec.record_warning(CompileWarning::Unimplemented("asm label".to_string()), span)?;
                None
            }
            Extension::AvailabilityAttribute(_) => {
                ec.record_warning(
                    CompileWarning::Unimplemented("availability attribute".to_string()),
                    span,
                )?;
                None
            }
        };
        if let Some(attr) = attr {
            self.attrs.add_attribute(attr, span, ec)?;
        }

        Ok(())
    }

    fn add_derived_declarator_node(
        &mut self,
        dd: Node<lang_c::ast::DerivedDeclarator>,
        scope: &mut NameScope,
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
                        PointerQualifier::Extension(_) => {
                            ec.record_error(
                                CompileError::Unimplemented(
                                    "extension in pointer qualifiers".to_string(),
                                ),
                                pq.span,
                            )?;
                            unreachable!()
                        }
                    }
                }
                self.base_type.wrap_pointer(qualifiers);
            }
            DerivedDeclarator::Function(fd) => {
                // base_type is the function return type
                let mut params = Vec::new();
                let mut has_void = false;
                for param_decl in fd.node.parameters {
                    let mut builder = TypeBuilder::new();
                    for declspec in &param_decl.node.specifiers {
                        if let DeclarationSpecifier::Extension(vext) = &declspec.node {
                            for ext in vext {
                                builder.add_extension_node(ext, ec)?;
                            }
                        }
                    }
                    for declspec in param_decl.node.specifiers {
                        match declspec.node {
                            DeclarationSpecifier::StorageClass(sc) => match sc.node {
                                StorageClassSpecifier::Register => (), // register is ok for function parameters
                                _ => ec.record_error(CompileError::WrongStorageClass, sc.span)?,
                            },
                            DeclarationSpecifier::TypeSpecifier(ts) => {
                                builder.add_type_specifier_node(ts, scope, ec)?
                            }
                            DeclarationSpecifier::TypeQualifier(tq) => {
                                builder.add_type_qualifier_node(tq, ec)?
                            }
                            DeclarationSpecifier::Alignment(_) => todo!(),
                            DeclarationSpecifier::Function(_) => (), // ignore _Noreturn and inline
                            DeclarationSpecifier::Extension(_) => {
                                ec.record_warning(
                                    CompileWarning::Unimplemented(
                                        "extension in function declaration".to_string(),
                                    ),
                                    declspec.span,
                                )?;
                            }
                        }
                    }
                    let builder = builder.stage2(param_decl.span, ec)?;
                    let (name, t, attrs) = if let Some(decl) = param_decl.node.declarator {
                        builder.process_declarator_node(decl, scope, ec)?
                    } else {
                        let (t, attrs) = builder.finalize();
                        (None, t, attrs)
                    };
                    if attrs.is_packed() {
                        ec.record_warning(
                            CompileWarning::AttributeIgnored("packed".to_string()),
                            param_decl.span,
                        )?;
                    }
                    if attrs.get_section().is_some() {
                        ec.record_warning(
                            CompileWarning::AttributeIgnored("section".to_string()),
                            param_decl.span,
                        )?;
                    }
                    if t.t.is_void() {
                        if name.is_some() {
                            ec.record_error(CompileError::NamedVoidParameter, param_decl.span)?;
                        }
                        if !t.qualifiers.is_empty() {
                            ec.record_error(CompileError::QualifiedVoidParameter, param_decl.span)?;
                        }
                        has_void = true;
                    }
                    params.push((t, name, param_decl.span));
                }
                if has_void && params.len() > 1 {
                    ec.record_error(CompileError::VoidParameter, fd.span)?;
                }
                let vararg = if let Ellipsis::Some = fd.node.ellipsis {
                    true
                } else {
                    false
                };
                if params.len() == 0 {
                    self.base_type.wrap_function(FunctionArgs::Empty, vararg);
                } else if has_void {
                    self.base_type.wrap_function(FunctionArgs::Void, vararg);
                } else {
                    self.base_type
                        .wrap_function(FunctionArgs::List(params), vararg);
                }
            }
            DerivedDeclarator::Array(ad) => {
                use lang_c::ast::ArraySize;
                if !ad.node.qualifiers.is_empty() {
                    ec.record_error(
                        CompileError::Unimplemented(
                            "type qualifiers in array length declaration".to_string(),
                        ),
                        ad.span,
                    )?;
                    unreachable!()
                }
                if self.base_type.t.is_packed_object(scope) {
                    let size = self.base_type.t.sizeof(scope, dd.span, ec)?;
                    let align = self.base_type.t.alignof(scope, dd.span, ec)?;
                    if size != align {
                        ec.record_warning(
                            CompileWarning::ArrayOfPacked(self.base_type.clone()),
                            dd.span,
                        )?;
                    }
                }
                match ad.node.size {
                    ArraySize::Unknown => self.base_type.wrap_array(None),
                    ArraySize::VariableExpression(e) => {
                        let size = constant::compute_constant_expr(*e, true, scope, ec)?;
                        if !size.t.t.is_integer() {
                            ec.record_error(CompileError::ArraySizeNotInteger(size.t), ad.span)?;
                            unreachable!();
                        }
                        let size = size.unwrap_integer();
                        if size < 0 {
                            ec.record_error(CompileError::ArraySizeNegative, ad.span)?;
                            unreachable!();
                        }
                        self.base_type.wrap_array(Some(size as u32))
                    }
                    ArraySize::VariableUnknown => {
                        ec.record_error(
                            CompileError::Unimplemented("variable length arrays".to_string()),
                            dd.span,
                        )?;
                        unreachable!()
                    }
                    ArraySize::StaticExpression(_) => {
                        ec.record_error(
                            CompileError::Unimplemented("static array size".to_string()),
                            ad.span,
                        )?;
                        unreachable!()
                    }
                }
            }
            DerivedDeclarator::KRFunction(_) => {
                ec.record_error(
                    CompileError::Unimplemented("K&R function".to_string()),
                    dd.span,
                )?;
                unreachable!()
            }
            DerivedDeclarator::Block(_) => {
                ec.record_error(
                    CompileError::Unimplemented("block derived declarator".to_string()),
                    dd.span,
                )?;
                unreachable!()
            }
        }
        Ok(())
    }
}

pub fn build_type_from_ast_type_name(
    node: Node<TypeName>,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<(QualifiedType, Attributes), ()> {
    let mut builder = TypeBuilder::new();
    for sq in &node.node.specifiers {
        if let SpecifierQualifier::Extension(vext) = &sq.node {
            for ext in vext {
                builder.add_extension_node(ext, ec)?;
            }
        }
    }
    for sq in node.node.specifiers {
        builder.add_specifier_qualifier_node(sq, scope, ec)?;
    }
    let stage2 = builder.stage2(node.span, ec)?;
    if let Some(declarator) = node.node.declarator {
        let (_name, t, attrs) = stage2.process_declarator_node(declarator, scope, ec)?;
        Ok((t, attrs))
    } else {
        Ok(stage2.finalize())
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
            BaseType::Builtin(t) => write!(f, "{}", t),
            BaseType::Alias(name, t) => write!(f, "{} (aka {})", name, t),
            BaseType::StructUnion(t) => write!(f, "{}", t),
            BaseType::Enum(t) => write!(f, "{}", t),
        }
    }
}
