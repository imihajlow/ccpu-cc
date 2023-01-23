use lang_c::{span::Node, ast::FunctionDefinition};

use crate::{error::ErrorCollector, translation_unit::TranslationUnit};

pub struct Function {

}

impl Function {
    pub fn new_from_node(node: Node<FunctionDefinition>, tu: &TranslationUnit, ec: &mut ErrorCollector) -> Result<Self, ()> {
        todo!()
    }
}
