use lang_c::ast::{AlignOf, SizeOfTy, SizeOfVal};
use lang_c::span::Node;

use crate::block_emitter::BlockEmitter;
use crate::ctype::{self, QualifiedType, Qualifiers};
use crate::error::ErrorCollector;
use crate::name_scope::NameScope;
use crate::rvalue::{RValue, TypedRValue};
use crate::type_builder::TypeBuilder;

use super::compile_expression;

pub fn compile_sizeof_val(
    si: Node<SizeOfVal>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    // compile, find out type and then drop
    let t = {
        let mut be = be.clone();
        let expr = compile_expression(*si.node.0, scope, &mut be, ec)?;
        expr.t
    };

    Ok(TypedRValue {
        src: RValue::new_const(t.t.sizeof(scope, si.span, ec)? as u64),
        t: QualifiedType {
            t: ctype::INT_TYPE,
            qualifiers: Qualifiers::empty(),
        },
    })
}

pub fn compile_sizeof_type(
    si: Node<SizeOfTy>,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let type_builder =
        TypeBuilder::new_from_specifiers_qualifiers(si.node.0.node.specifiers, scope, ec)?;
    let stage2 = type_builder.stage2(si.span, ec)?;
    let t = if let Some(decl) = si.node.0.node.declarator {
        stage2.process_declarator_node(decl, scope, ec)?.1
    } else {
        stage2.finalize()
    };

    Ok(TypedRValue {
        src: RValue::new_const(t.t.sizeof(scope, si.span, ec)? as u64),
        t: QualifiedType {
            t: ctype::INT_TYPE,
            qualifiers: Qualifiers::empty(),
        },
    })
}

pub fn compile_alignof(
    si: Node<AlignOf>,
    scope: &mut NameScope,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let type_builder =
        TypeBuilder::new_from_specifiers_qualifiers(si.node.0.node.specifiers, scope, ec)?;
    let stage2 = type_builder.stage2(si.span, ec)?;
    let t = if let Some(decl) = si.node.0.node.declarator {
        stage2.process_declarator_node(decl, scope, ec)?.1
    } else {
        stage2.finalize()
    };

    Ok(TypedRValue {
        src: RValue::new_const(t.t.alignof(scope, si.span, ec)? as u64),
        t: QualifiedType {
            t: ctype::INT_TYPE,
            qualifiers: Qualifiers::empty(),
        },
    })
}

