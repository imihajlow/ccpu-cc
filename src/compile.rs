use lang_c::span::Span;
use lang_c::{
    ast::{BlockItem, Declaration, Expression, Initializer, Statement, StorageClassSpecifier},
    span::Node,
};

use crate::ctype::{self, CType, Qualifiers};
use crate::error::CompileError;
use crate::lvalue::TypedLValue;
use crate::machine;
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
mod binary;
mod relational;
mod shift;
mod sub;
mod unary;

#[derive(Clone)]
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
        Expression::UnaryOperator(o) => unary::compile_unary_operator(*o, scope, be, ec),
        Expression::Comma(c) => compile_comma(*c, scope, be, ec),
        Expression::Conditional(c) => be.append_conditional(*c, scope, ec),
        Expression::Cast(_) => todo!(),
        Expression::Call(_) => todo!(),
        Expression::SizeOfTy(_) => todo!(),
        Expression::SizeOfVal(_) => todo!(),
        Expression::AlignOf(_) => todo!(),
        Expression::OffsetOf(_) => todo!(),
        Expression::StringLiteral(_) => todo!(),
        Expression::Member(_) => todo!(),
        Expression::CompoundLiteral(_) => todo!(),
        Expression::VaArg(_) => todo!(),
        Expression::GenericSelection(_) => unimplemented!(),
        Expression::Statement(_) => unimplemented!(),
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

pub fn convert_to_bool(
    src: TypedSrc,
    span: Span,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    if !src.t.t.is_scalar() {
        ec.record_error(CompileError::ScalarTypeRequired, span)?;
        unreachable!();
    }
    let dst = scope.alloc_temp();
    let width = src.t.t.get_scalar_width().unwrap();
    be.append_operation(ir::Op::Bool(ir::UnaryUnsignedOp {
        dst: dst.clone(),
        src: src.src,
        width,
    }));
    Ok(TypedSrc {
        src: ir::Src::Var(dst),
        t: QualifiedType {
            t: CType::Int(1, false),
            qualifiers: Qualifiers::empty(),
        },
    })
}

pub fn cast(
    src: TypedSrc,
    target_type: &CType,
    copy_if_same_type: bool,
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
        if copy_if_same_type {
            let dst = scope.alloc_temp();
            be.append_operation(ir::Op::Copy(ir::UnaryUnsignedOp {
                dst: dst.clone(),
                src: src.src,
                width: dst_width,
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
}

/**
 * Given a pointer and an index, make an absolute address.
 *
 * Integer promotions are performed on index.
 */
pub fn compile_pointer_offset(
    ptr: (TypedSrc, Span),
    index: (TypedSrc, Span),
    subtract: bool,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    let (ptr, ptr_span) = ptr;
    let (index, index_span) = index;

    if !ptr.t.t.is_dereferencable() {
        ec.record_error(CompileError::PointerTypeRequired, ptr_span)?;
        unreachable!();
    }
    if !index.t.t.is_integer() {
        ec.record_error(CompileError::IntegerTypeRequired, ptr_span)?;
        unreachable!();
    }
    if !ptr.t.t.dereferences_to_complete() {
        ec.record_error(CompileError::SizeOfIncomplete(ptr.t), ptr_span)?;
        unreachable!();
    }

    let index = int_promote(index, scope, be);

    if index.t.t.sizeof(index_span, ec)? > machine::PTR_SIZE.into() {
        ec.record_warning(CompileWarning::IndexTooWide(index.t.clone()), index_span)?;
    }

    let index_ssize = cast(index, &ctype::SSIZE_TYPE, false, index_span, scope, be, ec)?;
    let element_size = ptr
        .t
        .clone()
        .dereference()
        .unwrap()
        .t
        .sizeof(ptr_span, ec)?;
    let element_size_src = TypedSrc {
        t: ctype::QualifiedType {
            t: ctype::SSIZE_TYPE,
            qualifiers: Qualifiers::empty(),
        },
        src: ir::Src::ConstInt(element_size.into()),
    };
    let offset_var = scope.alloc_temp();
    let target_var = scope.alloc_temp();
    let (width, sign) = ctype::SSIZE_TYPE.get_width_sign().unwrap();
    be.append_operation(ir::Op::Mul(ir::BinaryOp {
        dst: offset_var.clone(),
        lhs: index_ssize.src,
        rhs: element_size_src.src,
        width,
        sign,
    }));
    let op = if !subtract { ir::Op::Add } else { ir::Op::Sub };
    be.append_operation(op(ir::BinaryOp {
        dst: target_var.clone(),
        lhs: ptr.src,
        rhs: ir::Src::Var(offset_var),
        width,
        sign: false,
    }));
    Ok(TypedSrc {
        src: ir::Src::Var(target_var),
        t: ptr.t,
    })
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
    let (type_builder, stclass, _extra) =
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

pub fn integer_promote(
    src: (TypedSrc, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    let promoted_type = src.0.t.clone().promote();

    cast(src.0, &promoted_type.t, false, src.1, scope, be, ec)
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
            cast(lhs, &common_type, false, lhs_span, scope, be, ec)?,
            cast(rhs, &common_type, false, rhs_span, scope, be, ec)?,
        ))
    } else {
        unreachable!()
    }
}

fn compile_comma(
    c: Vec<Node<Expression>>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    let mut result = None;
    for e in c {
        let r = compile_expression(e, scope, be, ec)?;
        result.replace(r);
    }
    Ok(result.unwrap())
}
