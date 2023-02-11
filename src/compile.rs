use lang_c::span::Span;
use lang_c::{
    ast::{
        BinaryOperatorExpression, BlockItem, Declaration, Expression, Initializer, Statement,
        StorageClassSpecifier,
    },
    span::Node,
};

use crate::ctype::{CType, Qualifiers};
use crate::error::CompileError;
use crate::lvalue::{self, TypedLValue};
use crate::{
    block_emitter::BlockEmitter,
    constant,
    ctype::QualifiedType,
    error::{CompileWarning, ErrorCollector},
    initializer::TypedValue,
    ir,
    name_scope::NameScope,
    type_builder::TypeBuilder,
};

mod add;
mod assign;
mod sub;
mod binary;

pub struct TypedSrc {
    pub src: ir::Src,
    pub t: QualifiedType,
}

impl TypedSrc {
    pub fn new_from_typed_value(tv: TypedValue) -> Self {
        use crate::initializer::Value;
        match tv.val {
            Value::Int(x) => Self {
                src: ir::Src::ConstInt(x as u64),
                t: tv.t,
            },
            Value::Void => panic!("void constant"),
            _ => todo!(),
        }
    }
}

pub fn compile_statement(
    stat: Node<Statement>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    match stat.node {
        Statement::Expression(None) => {}
        Statement::Expression(Some(e)) => {
            compile_expression(*e, scope, be, ec)?;
            ()
        }
        Statement::Compound(items) => compile_block(items, scope, be, ec)?,
        Statement::If(ifs) => be.append_if(ifs, scope, ec)?,
        Statement::While(whiles) => be.append_while(whiles, scope, ec)?,
        Statement::For(fors) => be.append_for(fors, scope, ec)?,
        Statement::DoWhile(whiles) => be.append_do_while(whiles, scope, ec)?,
        _ => todo!(),
    }
    Ok(())
}

pub fn compile_expression(
    expr: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    match expr.node {
        Expression::Constant(_) => {
            let v = constant::compute_constant_expr(expr, false, scope, ec)?;
            Ok(TypedSrc::new_from_typed_value(v))
        }
        Expression::Identifier(id) => {
            let (t, v) = scope.get_var(&id.node.name, id.span, ec)?;
            Ok(TypedSrc {
                src: ir::Src::Var(v),
                t: t.clone(),
            })
        }
        Expression::BinaryOperator(o) => binary::compile_binary_operator(*o, scope, be, ec),
        Expression::GenericSelection(_) => unimplemented!(),
        _ => todo!(),
    }
}

pub fn compile_initializer(
    initializer: Node<Initializer>,
    target: &str,
    target_span: Span,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    match initializer.node {
        Initializer::Expression(e) => {
            let lvalue = TypedLValue::new_from_name(target, target_span, scope, ec)?;
            let rhs_span = e.span;
            let rhs = compile_expression(*e, scope, be, ec)?;
            assign::compile_assign_to_lval(lvalue, (rhs, rhs_span), scope, be, ec)?;
            Ok(())
        }
        Initializer::List(_) => todo!(),
    }
}

/**
 * Do the integer promotion.
 *
 * Panics if src is not an integer.
 */
pub fn int_promote(src: TypedSrc, scope: &mut NameScope, be: &mut BlockEmitter) -> TypedSrc {
    if !src.t.t.is_integer() {
        panic!("not an integer");
    }
    let promoted_t = src.t.clone().promote();
    if src.t == promoted_t {
        src
    } else {
        let (src_width, src_sign) = src.t.t.get_width_sign().unwrap();
        let (dst_width, dst_sign) = promoted_t.t.get_width_sign().unwrap();
        let dst = scope.alloc_temp();
        be.append_operation(ir::Op::Conv(ir::ConvOp {
            src_width,
            src_sign,
            dst_width,
            dst_sign,
            dst: dst.clone(),
            src: src.src,
        }));

        TypedSrc {
            src: ir::Src::Var(dst),
            t: promoted_t,
        }
    }
}

fn compile_block(
    block: Vec<Node<BlockItem>>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    scope.push();
    for item in block {
        match item.node {
            BlockItem::Statement(stat) => compile_statement(stat, scope, be, ec)?,
            BlockItem::Declaration(decl) => compile_declaration(decl, scope, be, ec)?,
            BlockItem::StaticAssert(sa) => constant::check_static_assert(sa, scope, ec)?,
        }
    }
    scope.pop_and_collect_initializers();
    Ok(())
}

fn compile_declaration(
    decl: Node<Declaration>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<(), ()> {
    let (mut type_builder, stclass, _extra) =
        TypeBuilder::new_from_specifiers(decl.node.specifiers, scope, ec)?;
    for init_declarator in decl.node.declarators {
        let tb = type_builder.stage2(init_declarator.span, ec)?;
        let (name, t) = tb.process_declarator_node(init_declarator.node.declarator, scope, ec)?;
        if name.is_none() {
            ec.record_warning(CompileWarning::EmptyDeclaration, init_declarator.span)?;
            continue;
        }
        let name = name.unwrap();
        let is_static = matches!(
            stclass,
            Some(Node {
                node: StorageClassSpecifier::Static,
                ..
            })
        );
        if is_static {
            let initializer = if let Some(initializer) = init_declarator.node.initializer {
                Some(constant::compute_constant_initializer(
                    initializer,
                    &t,
                    true,
                    scope,
                    ec,
                )?)
            } else {
                None
            };
            scope.declare(&name, t, &stclass, initializer, init_declarator.span, ec)?;
        } else {
            scope.declare(&name, t, &stclass, None, init_declarator.span, ec)?;
            if let Some(initializer) = init_declarator.node.initializer {
                compile_initializer(initializer, &name, init_declarator.span, scope, be, ec)?;
            }
        };
    }
    Ok(())
}

fn cast_if_needed(
    src: TypedSrc,
    target_type: &CType,
    span: Span,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    let (dst_width, dst_sign) = target_type.get_width_sign().unwrap();
    let (src_width, src_sign) = src.t.t.get_width_sign().unwrap();
    if (dst_width, dst_sign) != (src_width, src_sign) {
        let dst = scope.alloc_temp();
        be.append_operation(ir::Op::Conv(ir::ConvOp {
            dst_width,
            dst_sign,
            src_width,
            src_sign,
            dst: dst.clone(),
            src: src.src,
        }));
        Ok(TypedSrc {
            src: ir::Src::Var(dst),
            t: QualifiedType {
                t: target_type.clone(),
                qualifiers: Qualifiers::empty(),
            },
        })
    } else {
        Ok(src)
    }
}

fn integer_promote(
    src: (TypedSrc, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    let promoted_type = src.0.t.clone().promote();

    cast_if_needed(src.0, &promoted_type.t, src.1, scope, be, ec)
}

/**
 * Perform usual arithmetic conversions according to 6.3.1.8
 */
fn usual_arithmetic_convert(
    lhs: (TypedSrc, Span),
    rhs: (TypedSrc, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<(TypedSrc, TypedSrc), ()> {
    let (lhs, lhs_span) = lhs;
    let (rhs, rhs_span) = rhs;
    if lhs.t.t.is_long_double() || rhs.t.t.is_long_double() {
        todo!()
    } else if lhs.t.t.is_double() || rhs.t.t.is_double() {
        todo!()
    } else if lhs.t.t.is_float() || rhs.t.t.is_float() {
        todo!()
    } else if lhs.t.t.is_integer() && rhs.t.t.is_integer() {
        // Otherwise, the integer promotions are performed on both operands.
        let lhs = integer_promote((lhs, lhs_span), scope, be, ec)?;
        let rhs = integer_promote((rhs, rhs_span), scope, be, ec)?;
        // Then the following rules are applied to the promoted operands
        let common_type = lhs.t.t.least_common_int_type(&rhs.t.t);
        Ok((
            cast_if_needed(lhs, &common_type, lhs_span, scope, be, ec)?,
            cast_if_needed(rhs, &common_type, rhs_span, scope, be, ec)?,
        ))
    } else {
        unreachable!()
    }
}
