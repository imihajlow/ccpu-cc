use lang_c::{
    ast::{BlockItem, Declaration, Expression, Statement, DeclarationSpecifier},
    span::Node,
};

use crate::{block_emitter::BlockEmitter, error::ErrorCollector, ir, name_scope::NameScope, type_builder::{self, TypeBuilder}, type_registry::TypeRegistry};

pub fn compile_statement(
    stat: Node<Statement>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    reg: &TypeRegistry,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    match stat.node {
        Statement::Compound(items) => compile_block(items, scope, be, reg, ec)?,
        Statement::If(ifs) => be.append_if(ifs, scope, reg, ec)?,
        Statement::While(whiles) => be.append_while(whiles, scope, ec)?,
        Statement::For(fors) => be.append_for(fors, scope, ec)?,
        Statement::DoWhile(whiles) => be.append_do_while(whiles, scope, ec)?,
        _ => todo!(),
    }
    todo!()
}

pub fn compile_expression(
    expr: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<ir::Src, ()> {
    todo!()
}

fn compile_block(
    block: Vec<Node<BlockItem>>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    reg: &TypeRegistry,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    scope.push();
    for item in block {
        match item.node {
            BlockItem::Statement(stat) => compile_statement(stat, scope, be, reg, ec)?,
            BlockItem::Declaration(decl) => compile_declaration(decl, scope, be, reg, ec)?,
            _ => todo!(),
        }
    }
    scope.pop();
    Ok(())
}

fn compile_declaration(
    decl: Node<Declaration>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    reg: &TypeRegistry,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    let (mut type_builder, stclass, extra) = TypeBuilder::new_from_specifiers(decl.node.specifiers, reg, ec)?;
    todo!()
}
