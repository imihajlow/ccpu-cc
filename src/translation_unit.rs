use std::collections::HashMap;

use crate::ctype::QualifiedType;
use crate::error::{CompileError, CompileWarning, ErrorCollector};
use crate::type_builder::{TypeBuilder, TypeBuilderStage2};
use crate::type_registry::TypeRegistry;
use lang_c::ast::{
    Declaration, DeclarationSpecifier, Declarator, DeclaratorKind, ExternalDeclaration,
    FunctionDefinition, FunctionSpecifier, InitDeclarator, StorageClassSpecifier,
};
use lang_c::span::Node;

pub struct TranslationUnit {
    type_registry: TypeRegistry,
    global_declarations: HashMap<String, (QualifiedType, GlobalStorageClass)>,
    global_symbols: Vec<String>,
}

#[derive(Debug, PartialEq)]
enum GlobalStorageClass {
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
        let (id, t) = process_declarator_node(init_declarator.declarator, type_builder, ec)?;
        match id {
            None => ec.record_warning(CompileWarning::EmptyDeclaration, init_declarator_span)?,
            Some(id) => {
                match storage_class {
                    Some(StorageClassSpecifier::Typedef) => {
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
                            Some((old_t, old_storage_class)) => {
                                // redefinition, check type match and storage class
                                if !old_t.is_same_as(&t) {
                                    return ec.record_error(
                                        CompileError::ConflictingTypes(id),
                                        init_declarator_span,
                                    );
                                }
                                if !match_storage_classes(old_storage_class, &st_class) {
                                    return ec.record_error(
                                        CompileError::ConflictingStorageClass(id),
                                        init_declarator_span,
                                    );
                                }
                            }
                            None => {
                                self.global_declarations.insert(id, (t, st_class));
                            }
                        }
                    }
                    Some(StorageClassSpecifier::Auto) | Some(StorageClassSpecifier::Register) => {
                        unreachable!()
                    }
                    Some(StorageClassSpecifier::ThreadLocal) => unimplemented!(),
                }
            }
        }
        Ok(())
    }

    fn add_function_definition(
        &mut self,
        n: Node<FunctionDefinition>,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        todo!()
    }
}

/**
 * Recursively continue building the type and return the name and the type.
 */
fn process_declarator_node(
    declarator: Node<Declarator>,
    mut type_builder: TypeBuilderStage2,
    ec: &mut ErrorCollector,
) -> Result<(Option<String>, QualifiedType), ()> {
    let Node {
        node: declarator,
        span,
    } = declarator;

    for derived in declarator.derived {
        type_builder.add_derived_declarator_node(derived, ec)?;
    }

    if !declarator.extensions.is_empty() {
        ec.record_warning(CompileWarning::Unimplemented("extension".to_string()), span)?;
    }

    let kind = declarator.kind.node;
    match kind {
        DeclaratorKind::Abstract | DeclaratorKind::Identifier(_) => {
            let qt = type_builder.finalize();
            match kind {
                DeclaratorKind::Abstract => Ok((None, qt)),
                DeclaratorKind::Identifier(id) => Ok((Some(id.node.name), qt)),
                DeclaratorKind::Declarator(_) => unreachable!(),
            }
        }
        DeclaratorKind::Declarator(decl) => process_declarator_node(*decl, type_builder, ec),
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
        let (t, sc) = tu.global_declarations.get("x").unwrap();
        assert_eq!(t.t, ctype::INT_TYPE);
        assert_eq!(t.qualifiers, Qualifiers::empty());
        assert_eq!(*sc, GlobalStorageClass::Default);
    }

    #[test]
    fn test_global_var_2() {
        let (tu_result, ec) = translate("long long unsigned int x;");
        assert!(tu_result.is_ok());
        assert_eq!(ec.get_error_count(), 0);
        let tu = tu_result.unwrap();
        let (t, sc) = tu.global_declarations.get("x").unwrap();
        assert_eq!(t.t, ctype::ULLONG_TYPE);
        assert_eq!(t.qualifiers, Qualifiers::empty());
        assert_eq!(*sc, GlobalStorageClass::Default);
    }

    #[test]
    fn test_global_var_3() {
        let (tu_result, ec) = translate("static const char x;");
        assert!(tu_result.is_ok());
        assert_eq!(ec.get_error_count(), 0);
        let tu = tu_result.unwrap();
        let (t, sc) = tu.global_declarations.get("x").unwrap();
        assert_eq!(t.t, ctype::UCHAR_TYPE);
        assert_eq!(t.qualifiers, Qualifiers::CONST);
        assert_eq!(*sc, GlobalStorageClass::Static);
    }

    #[test]
    fn test_global_var_4() {
        let (tu_result, ec) = translate("signed int x, * const volatile y;");
        assert!(tu_result.is_ok());
        assert_eq!(ec.get_error_count(), 0);
        let tu = tu_result.unwrap();
        let (t, sc) = tu.global_declarations.get("x").unwrap();
        assert_eq!(t.t, ctype::INT_TYPE);
        assert_eq!(t.qualifiers, Qualifiers::empty());
        assert_eq!(*sc, GlobalStorageClass::Default);

        let (t, sc) = tu.global_declarations.get("y").unwrap();
        assert_eq!(t.qualifiers, Qualifiers::CONST | Qualifiers::VOLATILE);
        assert_eq!(*sc, GlobalStorageClass::Default);
        assert_eq!(
            t.t,
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
        let (t, sc) = tu.global_declarations.get("x").unwrap();
        assert_eq!(t.t, ctype::UCHAR_TYPE);
        assert_eq!(t.qualifiers, Qualifiers::CONST);
        assert_eq!(*sc, GlobalStorageClass::Static);
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
}
