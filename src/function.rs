use std::fmt::Formatter;

use crate::{name_scope::{GlobalStorageClass, NameScope}, compile, block_emitter::{BlockEmitter, LabeledBlock}};
use lang_c::{
    ast::{FunctionDefinition, StorageClassSpecifier},
    span::Node,
};

use crate::{
    ctype::{CType, FunctionArgs, QualifiedType},
    error::{CompileError, ErrorCollector},
    type_builder::TypeBuilder,
};

pub struct Function {
    is_inline: bool,
    is_noreturn: bool,
    is_vararg: bool,
    name: String,
    storage_class: GlobalStorageClass,
    return_type: QualifiedType,
    args: FunctionArgs,
    body: Vec<LabeledBlock>,
}

impl Function {
    pub fn new_from_node(
        node: Node<FunctionDefinition>,
        scope: &mut NameScope,
        ec: &mut ErrorCollector,
    ) -> Result<Self, ()> {
        if !node.node.declarations.is_empty() {
            unimplemented!("K&R functions");
        }
        let (mut type_builder, storage_class, extra) =
            TypeBuilder::new_from_specifiers(node.node.specifiers, scope, ec)?;
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
        let (name, t) = type_builder.process_declarator_node(node.node.declarator, scope, ec)?;
        let name = name.unwrap();
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
        let mut be = BlockEmitter::new();
        compile::compile_statement(node.node.statement, scope, &mut be, ec)?;
        Ok(Self {
            is_inline: extra.is_inline,
            is_noreturn: extra.is_noreturn,
            storage_class,
            is_vararg,
            return_type,
            args,
            name,
            body: be.finalize(),
        })
    }
}


impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(f, "function {}", self.name)?;
        for (i, b) in self.body.iter().enumerate() {
            writeln!(f, "{}:\n{}\n", i, b)?;
        }
        Ok(())
    }
}
