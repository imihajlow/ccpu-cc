use std::fmt::Formatter;

use crate::constant::{self, compute_constant_initializer};
use crate::error::{CompileWarning, ErrorCollector};
use crate::function::Function;
use crate::name_scope::{GlobalStorageClass, NameScope};
use crate::type_builder::TypeBuilder;

use lang_c::ast::{
    Declaration, ExternalDeclaration, FunctionDefinition, InitDeclarator, StorageClassSpecifier,
};
use lang_c::span::Node;

pub struct TranslationUnit {
    pub scope: NameScope,
    pub functions: Vec<Function>,
}

impl TranslationUnit {
    pub fn translate(
        tu: lang_c::ast::TranslationUnit,
        ec: &mut ErrorCollector,
    ) -> Result<Self, ()> {
        let mut r = Self {
            scope: NameScope::new(),
            functions: Vec::new(),
        };
        let mut has_error = false;
        for Node { node: ed, .. } in tu.0.into_iter() {
            match ed {
                ExternalDeclaration::StaticAssert(node) => {
                    constant::check_static_assert(node, &mut r.scope, ec)?;
                }
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
        let (mut type_builder, storage_class, _extra) =
            TypeBuilder::new_from_specifiers(decl.specifiers, &mut self.scope, ec)?;

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
        storage_class: &Option<Node<StorageClassSpecifier>>,
        type_builder: &mut TypeBuilder,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let init_declarator_span = init_declarator.span;
        let init_declarator = init_declarator.node;
        let type_builder = type_builder.stage2(init_declarator_span, ec)?;
        let (id, t) = type_builder.process_declarator_node(
            init_declarator.declarator,
            &mut self.scope,
            ec,
        )?;

        let initializer = if let Some(initializer) = init_declarator.initializer {
            Some(compute_constant_initializer(
                initializer,
                &t,
                false,
                &mut self.scope,
                ec,
            )?)
        } else {
            None
        };
        match id {
            None => ec.record_warning(CompileWarning::EmptyDeclaration, init_declarator_span)?,
            Some(id) => {
                self.scope
                    .declare(&id, t, storage_class, initializer, init_declarator_span, ec)?
            }
        }
        Ok(())
    }

    fn add_function_definition(
        &mut self,
        n: Node<FunctionDefinition>,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let f = Function::new_from_node(n, &mut self.scope, ec)?;
        self.functions.push(f);
        Ok(())
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

impl std::fmt::Display for TranslationUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        for fun in &self.functions {
            writeln!(f, "{}", fun)?;
        }
        Ok(())
    }
}
