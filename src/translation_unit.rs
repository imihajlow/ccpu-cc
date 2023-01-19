use std::collections::HashMap;

use crate::constant::compute_constant_initializer;
use crate::ctype::QualifiedType;
use crate::error::{CompileError, CompileWarning, ErrorCollector};
use crate::type_builder::{TypeBuilder, TypeBuilderStage2};
use crate::type_registry::TypeRegistry;
use crate::initializer::{Value, self};
use lang_c::ast::{
    Declaration, DeclarationSpecifier, Declarator, DeclaratorKind, ExternalDeclaration,
    FunctionDefinition, FunctionSpecifier, InitDeclarator, StorageClassSpecifier,
};
use lang_c::span::Node;

pub struct TranslationUnit {
    pub type_registry: TypeRegistry,
    global_declarations: HashMap<String, GlobalDeclaration>,
    global_symbols: Vec<String>,
}

pub struct GlobalDeclaration {
    pub t: QualifiedType,
    pub storage_class: GlobalStorageClass,
    pub initializer: Option<Value>,
}

#[derive(Debug, PartialEq)]
pub enum GlobalStorageClass {
    Default,
    Static,
    Extern,
}

impl TranslationUnit {
    pub fn translate(
        tu: lang_c::ast::TranslationUnit,
        ec: &mut ErrorCollector,
    ) -> Result<Self, ()> {
        let mut r = Self {
            type_registry: TypeRegistry::new(),
            global_declarations: HashMap::new(),
            global_symbols: Vec::new(),
        };
        let mut has_error = false;
        for Node { node: ed, .. } in tu.0.into_iter() {
            match ed {
                ExternalDeclaration::StaticAssert(_) => todo!(),
                ExternalDeclaration::Declaration(n) => {
                    if r.add_declaration(n, ec).is_err() {
                        has_error = true;
                    }
                }
                ExternalDeclaration::FunctionDefinition(n) => {
                    if r.add_function_definition(n, ec).is_err() {
                        has_error = true;
                    }
                }
            }
        }
        if has_error {
            Err(())
        } else {
            Ok(r)
        }
    }

    pub fn lookup_global_declaration(&self, id: &str) -> Option<&GlobalDeclaration> {
        self.global_declarations.get(id)
    }

    fn add_declaration(&mut self, n: Node<Declaration>, ec: &mut ErrorCollector) -> Result<(), ()> {
        let decl = n.node;
        let mut storage_class = None;
        let mut type_builder = TypeBuilder::new();
        let mut is_inline = false;
        for Node {
            node: declspec,
            span: declspec_span,
        } in decl.specifiers
        {
            match declspec {
                DeclarationSpecifier::StorageClass(Node {
                    node: stclass,
                    span: stclass_span,
                }) => {
                    if storage_class.is_none() {
                        if let StorageClassSpecifier::Auto | StorageClassSpecifier::Register =
                            stclass
                        {
                            return ec.record_error(CompileError::WrongStorageClass, stclass_span);
                        }
                        storage_class = Some(stclass);
                    } else {
                        return ec.record_error(CompileError::MultipleStorageClasses, stclass_span);
                    }
                }
                DeclarationSpecifier::TypeSpecifier(typespec) => {
                    type_builder.add_type_specifier_node(typespec, &self.type_registry, ec)?
                }
                DeclarationSpecifier::TypeQualifier(typequal) => {
                    type_builder.add_type_qualifier_node(typequal, ec)?
                }
                DeclarationSpecifier::Function(Node {
                    node: fnspec,
                    span: fnspec_span,
                }) => match fnspec {
                    FunctionSpecifier::Inline => is_inline = true,
                    FunctionSpecifier::Noreturn => ec.record_warning(
                        CompileWarning::Unimplemented("_Noreturn".to_string()),
                        fnspec_span,
                    )?,
                },
                DeclarationSpecifier::Alignment(_) => ec.record_warning(
                    CompileWarning::Unimplemented("alignment".to_string()),
                    declspec_span,
                )?,
                DeclarationSpecifier::Extension(_) => ec.record_error(
                    CompileError::Unimplemented("extension".to_string()),
                    declspec_span,
                )?,
            }
        }
        let mut has_error = false;
        for init_declarator in decl.declarators {
            if self
                .process_init_declarator_node(
                    init_declarator,
                    &storage_class,
                    &mut type_builder,
                    ec,
                )
                .is_err()
            {
                has_error = true;
            }
        }
        if has_error {
            Err(())
        } else {
            Ok(())
        }
    }

    fn process_init_declarator_node(
        &mut self,
        init_declarator: Node<InitDeclarator>,
        storage_class: &Option<StorageClassSpecifier>,
        type_builder: &mut TypeBuilder,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let init_declarator_span = init_declarator.span;
        let init_declarator = init_declarator.node;
        let type_builder = type_builder.stage2(init_declarator_span, ec)?;
        let (id, t) = type_builder.process_declarator_node(init_declarator.declarator, ec)?;
        match id {
            None => ec.record_warning(CompileWarning::EmptyDeclaration, init_declarator_span)?,
            Some(id) => {
                match storage_class {
                    Some(StorageClassSpecifier::Typedef) => {
                        if let Some(initializer) = init_declarator.initializer {
                            return ec.record_error(CompileError::TypedefInitialized, initializer.span);
                        }
                        if self.type_registry.add_alias(&id, t).is_err() {
                            return ec.record_error(
                                CompileError::TypeRedefinition(id),
                                init_declarator_span,
                            );
                        }
                    }
                    None
                    | Some(StorageClassSpecifier::Extern)
                    | Some(StorageClassSpecifier::Static) => {
                        let st_class = match storage_class {
                            None => GlobalStorageClass::Default,
                            Some(StorageClassSpecifier::Extern) => GlobalStorageClass::Extern,
                            Some(StorageClassSpecifier::Static) => GlobalStorageClass::Static,
                            _ => unreachable!(),
                        };
                        match self.global_declarations.get(&id) {
                            Some(old) => {
                                // redefinition, check type match and storage class
                                if !old.t.is_same_as(&t) {
                                    return ec.record_error(
                                        CompileError::ConflictingTypes(id),
                                        init_declarator_span,
                                    );
                                }
                                if !match_storage_classes(&old.storage_class, &st_class) {
                                    return ec.record_error(
                                        CompileError::ConflictingStorageClass(id),
                                        init_declarator_span,
                                    );
                                }
                            }
                            None => {
                                let initializer = if let Some(initializer) = init_declarator.initializer {
                                    Some(compute_constant_initializer(initializer, &t, false, self, ec)?)
                                } else {
                                    None
                                };
                                self.global_declarations.insert(
                                    id,
                                    GlobalDeclaration {
                                        t,
                                        storage_class: st_class,
                                        initializer,
                                    },
                                );
                            }
                        }
                    }
                    Some(StorageClassSpecifier::Auto) | Some(StorageClassSpecifier::Register) => {
                        // handled in add_declaration
                        unreachable!()
                    }
                    Some(StorageClassSpecifier::ThreadLocal) => unimplemented!(),
                }
            }
        }
        Ok(())
    }

    fn process_initializer_node(&self, ec: &mut ErrorCollector) -> Result<Option<Value>, ()> {
        todo!()
    }

    fn add_function_definition(
        &mut self,
        n: Node<FunctionDefinition>,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        todo!()
    }
}

fn match_storage_classes(old: &GlobalStorageClass, new: &GlobalStorageClass) -> bool {
    use GlobalStorageClass::*;
    // same classes match
    // static may not follow non-static
    // extern may follow static
    match (old, new) {
        (x, y) if x == y => true,
        (_, Static) => false,
        (Static, Extern) => true,
        (Static, Default) => false,
        (_, _) => true,
    }
}

#[cfg(test)]
mod test {
    #![feature(assert_matches)]
    use std::assert_matches::assert_matches;

    use crate::ctype::{self, CType, QualifiedType, Qualifiers};

    use super::*;

    use lang_c::driver::{parse_preprocessed, Config, Flavor};

    fn translate(code: &str) -> (Result<TranslationUnit, ()>, ErrorCollector) {
        let mut cfg = Config::default();
        cfg.flavor = Flavor::StdC11;
        let p = parse_preprocessed(&cfg, code.to_string()).unwrap();
        let mut ec = ErrorCollector::new();
        (TranslationUnit::translate(p.unit, &mut ec), ec)
    }

    #[test]
    fn test_global_var_1() {
        let (tu_result, ec) = translate("int x;");
        assert!(tu_result.is_ok());
        assert_eq!(ec.get_error_count(), 0);
        let tu = tu_result.unwrap();
        let decl = tu.global_declarations.get("x").unwrap();
        assert_eq!(decl.t.t, ctype::INT_TYPE);
        assert_eq!(decl.t.qualifiers, Qualifiers::empty());
        assert_eq!(decl.storage_class, GlobalStorageClass::Default);
    }

    #[test]
    fn test_global_var_2() {
        let (tu_result, ec) = translate("long long unsigned int x;");
        assert!(tu_result.is_ok());
        assert_eq!(ec.get_error_count(), 0);
        let tu = tu_result.unwrap();
        let decl = tu.global_declarations.get("x").unwrap();
        assert_eq!(decl.t.t, ctype::ULLONG_TYPE);
        assert_eq!(decl.t.qualifiers, Qualifiers::empty());
        assert_eq!(decl.storage_class, GlobalStorageClass::Default);
    }

    #[test]
    fn test_global_var_3() {
        let (tu_result, ec) = translate("static const char x;");
        assert!(tu_result.is_ok());
        assert_eq!(ec.get_error_count(), 0);
        let tu = tu_result.unwrap();
        let decl = tu.global_declarations.get("x").unwrap();
        assert_eq!(decl.t.t, ctype::UCHAR_TYPE);
        assert_eq!(decl.t.qualifiers, Qualifiers::CONST);
        assert_eq!(decl.storage_class, GlobalStorageClass::Static);
    }

    #[test]
    fn test_global_var_4() {
        let (tu_result, ec) = translate("signed int x, * const volatile y;");
        assert!(tu_result.is_ok());
        assert_eq!(ec.get_error_count(), 0);
        let tu = tu_result.unwrap();
        let decl = tu.global_declarations.get("x").unwrap();
        assert_eq!(decl.t.t, ctype::INT_TYPE);
        assert_eq!(decl.t.qualifiers, Qualifiers::empty());
        assert_eq!(decl.storage_class, GlobalStorageClass::Default);

        let decl = tu.global_declarations.get("y").unwrap();
        assert_eq!(decl.t.qualifiers, Qualifiers::CONST | Qualifiers::VOLATILE);
        assert_eq!(decl.storage_class, GlobalStorageClass::Default);
        assert_eq!(
            decl.t.t,
            CType::Pointer(Box::new(QualifiedType {
                t: ctype::INT_TYPE,
                qualifiers: Qualifiers::empty()
            }))
        );
    }

    #[test]
    fn test_global_var_5() {
        let (tu_result, ec) = translate("typedef char new_char; static const new_char x;");
        assert!(tu_result.is_ok());
        assert_eq!(ec.get_error_count(), 0);
        let tu = tu_result.unwrap();
        let decl = tu.global_declarations.get("x").unwrap();
        assert_eq!(decl.t.t, ctype::UCHAR_TYPE);
        assert_eq!(decl.t.qualifiers, Qualifiers::CONST);
        assert_eq!(decl.storage_class, GlobalStorageClass::Static);
    }

    #[test]
    fn test_global_var_err_1() {
        let (tu_result, ec) = translate("static extern int x;");
        assert!(tu_result.is_err());
        assert_eq!(ec.get_error_count(), 1);

        let (tu_result, ec) = translate("static int x; int x;");
        assert!(tu_result.is_err());
        assert_eq!(ec.get_error_count(), 1);

        let (tu_result, ec) = translate("long short int x;");
        assert!(tu_result.is_err());
        assert_eq!(ec.get_error_count(), 1);
    }

    #[test]
    fn test_global_var_init() {
        let (tu_result, ec) = translate("const long x = 42;");
        assert!(tu_result.is_ok());
        assert_eq!(ec.get_error_count(), 0);
        let tu = tu_result.unwrap();
        let decl = tu.global_declarations.get("x").unwrap();
        assert_eq!(decl.t.t, ctype::SLONG_TYPE);
        assert_eq!(decl.t.qualifiers, Qualifiers::CONST);
        assert_eq!(decl.storage_class, GlobalStorageClass::Default);
        assert_matches!(decl.initializer, Some(Value::Int(42)));
        // assert_eq!(initializer.)
    }
}
