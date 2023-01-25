use lang_c::{
    ast::{DeclarationSpecifier, FunctionDefinition, FunctionSpecifier, StorageClassSpecifier},
    span::Node,
};

use crate::{
    ctype::{CType, FunctionArgs, QualifiedType},
    error::{CompileError, ErrorCollector},
    translation_unit::{GlobalStorageClass, TranslationUnit},
    type_builder::TypeBuilder,
};

pub struct Function {
    is_inline: bool,
    is_noreturn: bool,
    is_vararg: bool,
    storage_class: GlobalStorageClass,
    return_type: QualifiedType,
    args: FunctionArgs,
    body: Option<()>,
}

impl Function {
    pub fn new_from_node(
        node: Node<FunctionDefinition>,
        tu: &TranslationUnit,
        ec: &mut ErrorCollector,
    ) -> Result<Self, ()> {
        if !node.node.declarations.is_empty() {
            unimplemented!("K&R functions");
        }
        let mut is_inline = false;
        let mut is_noreturn = false;
        let mut type_builder = TypeBuilder::new();
        let mut storage_class = None;
        for declspec in node.node.specifiers {
            match declspec.node {
                DeclarationSpecifier::StorageClass(stc) => {
                    if storage_class.is_none() {
                        storage_class = Some(stc);
                    } else {
                        ec.record_error(CompileError::MultipleStorageClasses, stc.span)?;
                    }
                }
                DeclarationSpecifier::Function(f) => match f.node {
                    FunctionSpecifier::Inline => is_inline = true,
                    FunctionSpecifier::Noreturn => is_noreturn = true,
                },
                DeclarationSpecifier::TypeSpecifier(ts) => {
                    type_builder.add_type_specifier_node(ts, &tu.type_registry, ec)?
                }
                DeclarationSpecifier::TypeQualifier(q) => {
                    type_builder.add_type_qualifier_node(q, ec)?
                }
                DeclarationSpecifier::Alignment(_) => unimplemented!(),
                DeclarationSpecifier::Extension(_) => unimplemented!(),
            }
        }
        let storage_class = match storage_class {
            None => GlobalStorageClass::Default,
            Some(stc) => match stc.node {
                StorageClassSpecifier::Typedef
                | StorageClassSpecifier::Auto
                | StorageClassSpecifier::Register
                | StorageClassSpecifier::ThreadLocal => {
                    ec.record_error(CompileError::WrongStorageClass, stc.span)?;
                    unreachable!();
                }
                StorageClassSpecifier::Extern => GlobalStorageClass::Extern,
                StorageClassSpecifier::Static => GlobalStorageClass::Static,
            },
        };
        let type_builder = type_builder.stage2(node.span, ec)?;
        let (name, t) =
            type_builder.process_declarator_node(node.node.declarator, &tu.type_registry, ec)?;
        let _name = name.unwrap();
        let (return_type, args, is_vararg) = if let CType::Function {
            result,
            args,
            vararg,
        } = t.t
        {
            (*result, args, vararg)
        } else {
            unreachable!()
        };
        Ok(Self {
            is_inline,
            is_noreturn,
            storage_class,
            is_vararg,
            return_type,
            args,
            body: None,
        })
    }
}
