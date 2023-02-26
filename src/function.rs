use std::fmt::Formatter;

use crate::{
    block_emitter::{BlockEmitter, LabeledBlock},
    compile,
    name_scope::{GlobalStorageClass, NameScope},
};
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
    frame_size: u32,
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
        let (type_builder, storage_class, extra) =
            TypeBuilder::new_from_specifiers(node.node.specifiers, scope, ec)?;
        let type_builder = type_builder.stage2(node.span, ec)?;
        let (name, t) = type_builder.process_declarator_node(node.node.declarator, scope, ec)?;
        let name = name.unwrap();
        scope.declare(&name, t.clone(), &storage_class, None, node.span, ec)?;
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
        scope.start_function(&args, &return_type);
        let mut be = BlockEmitter::new();
        compile::compile_statement(node.node.statement, scope, &mut be, ec)?;
        scope.pop_and_collect_initializers();
        Ok(Self {
            is_inline: extra.is_inline,
            is_noreturn: extra.is_noreturn,
            storage_class,
            is_vararg,
            return_type,
            args,
            name,
            body: be.finalize(),
            frame_size: scope.reset_frame_size(),
        })
    }

    #[cfg(test)]
    pub fn get_body(&self) -> &Vec<LabeledBlock> {
        &self.body
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(f, "function {}({})", self.name, self.frame_size)?;
        for (i, b) in self.body.iter().enumerate() {
            writeln!(f, "{}:\n{}", i, b)?;
        }
        Ok(())
    }
}
