use lang_c::ast::{OffsetMember, OffsetOfExpression};
use lang_c::span::Node;

use crate::constant::compute_constant_expr;
use crate::error::CompileError;
use crate::name_scope::NameScope;
use crate::{type_builder, ErrorCollector};

pub fn compute_offsetof(
    node: Node<OffsetOfExpression>,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<u32, ()> {
    let t = type_builder::build_type_from_ast_type_name(node.node.type_name, scope, ec)?;

    let designator = node.node.designator.node;
    let (mut offset, mut t) =
        t.get_field(&designator.base.node.name, scope, designator.base.span, ec)?;
    for d in designator.members {
        match d.node {
            OffsetMember::Member(id) => {
                let (new_offset, new_t) = t.get_field(&id.node.name, scope, id.span, ec)?;
                offset += new_offset;
                t = new_t;
            }
            OffsetMember::Index(e) => {
                let span = e.span;
                let el_type = if let Ok(t) = t.dereference() {
                    t
                } else {
                    ec.record_error(CompileError::PointerTypeRequired, d.span)?;
                    unreachable!()
                };
                let el_size = el_type.t.sizeof(scope, span, ec)?;
                let val = compute_constant_expr(e, true, scope, ec)?;
                if !val.t.t.is_integer() {
                    ec.record_error(CompileError::IntegerTypeRequired, span)?;
                    unreachable!()
                }
                let index = val.unwrap_integer() as u32;
                offset += index * el_size;
                t = el_type;
            }
            OffsetMember::IndirectMember(_) => todo!(),
        }
    }
    Ok(offset)
}
