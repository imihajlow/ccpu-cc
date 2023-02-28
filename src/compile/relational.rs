use lang_c::span::Span;
use lang_c::{ast::Expression, span::Node};

use crate::block_emitter::BlockEmitter;
use crate::ctype::{QualifiedType, Qualifiers};
use crate::error::{CompileWarning, ErrorCollector};
use crate::name_scope::NameScope;
use crate::rvalue::{RValue, TypedRValue};
use crate::{ctype, ir};

use super::{compile_expression, usual_arithmetic_convert};

pub fn compile_equal_to(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs = compile_expression(lhs, scope, be, ec)?;
    let rhs = compile_expression(rhs, scope, be, ec)?;

    compile_cmp_inner(
        (lhs, lhs_span),
        (rhs, rhs_span),
        ir::CompareKind::Equal,
        scope,
        be,
        ec,
    )
}

pub fn compile_not_equal_to(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs = compile_expression(lhs, scope, be, ec)?;
    let rhs = compile_expression(rhs, scope, be, ec)?;

    compile_cmp_inner(
        (lhs, lhs_span),
        (rhs, rhs_span),
        ir::CompareKind::NotEqual,
        scope,
        be,
        ec,
    )
}

pub fn compile_less_than(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs = compile_expression(lhs, scope, be, ec)?;
    let rhs = compile_expression(rhs, scope, be, ec)?;

    compile_cmp_inner(
        (lhs, lhs_span),
        (rhs, rhs_span),
        ir::CompareKind::LessThan,
        scope,
        be,
        ec,
    )
}

pub fn compile_less_or_equal(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs = compile_expression(lhs, scope, be, ec)?;
    let rhs = compile_expression(rhs, scope, be, ec)?;

    compile_cmp_inner(
        (lhs, lhs_span),
        (rhs, rhs_span),
        ir::CompareKind::LessOrEqual,
        scope,
        be,
        ec,
    )
}

pub fn compile_greater_than(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs = compile_expression(lhs, scope, be, ec)?;
    let rhs = compile_expression(rhs, scope, be, ec)?;

    compile_cmp_inner(
        (lhs, lhs_span),
        (rhs, rhs_span),
        ir::CompareKind::GreaterThan,
        scope,
        be,
        ec,
    )
}

pub fn compile_greater_or_equal(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs = compile_expression(lhs, scope, be, ec)?;
    let rhs = compile_expression(rhs, scope, be, ec)?;

    compile_cmp_inner(
        (lhs, lhs_span),
        (rhs, rhs_span),
        ir::CompareKind::GreaterOrEqual,
        scope,
        be,
        ec,
    )
}

pub fn compile_cmp_inner(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    kind: ir::CompareKind,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    let (lhs, lhs_span) = lhs;
    let (rhs, rhs_span) = rhs;

    if lhs.t.t.is_arithmetic() && rhs.t.t.is_arithmetic() {
        let (lhs, rhs) = usual_arithmetic_convert((lhs, lhs_span), (rhs, rhs_span), scope, be, ec)?;

        assert!(lhs.t.t == rhs.t.t);

        if lhs.t.t.is_integer() {
            let (width, sign) = lhs.t.t.get_width_sign().unwrap();
            let dst = scope.alloc_temp();
            be.append_operation(ir::Op::Compare(ir::CompareOp {
                kind,
                dst: dst.clone(),
                dst_width: ir::Width::INT_WIDTH,
                lhs: lhs.unwrap_scalar(),
                rhs: rhs.unwrap_scalar(),
                width,
                sign,
            }));
            Ok(TypedRValue {
                src: RValue::new_var(dst),
                t: QualifiedType {
                    t: ctype::INT_TYPE,
                    qualifiers: Qualifiers::empty(),
                },
            })
        } else {
            todo!()
        }
    } else if lhs.t.t.is_dereferencable() && rhs.t.t.is_dereferencable() {
        let lhs_inner = lhs.t.clone().dereference().unwrap();
        let rhs_inner = rhs.t.clone().dereference().unwrap();
        if !lhs_inner.is_compatible_to(&rhs_inner, false) {
            if (kind != ir::CompareKind::Equal && kind != ir::CompareKind::NotEqual)
                || (!lhs_inner.t.is_void() && !rhs_inner.t.is_void())
            {
                ec.record_warning(
                    CompileWarning::IncompatibleTypes(lhs_inner, rhs_inner),
                    rhs_span,
                )?;
            }
        }
        let dst = scope.alloc_temp();
        let (width, sign) = lhs.t.t.get_width_sign().unwrap();
        be.append_operation(ir::Op::Compare(ir::CompareOp {
            kind,
            dst: dst.clone(),
            dst_width: ir::Width::INT_WIDTH,
            lhs: lhs.unwrap_scalar(),
            rhs: rhs.unwrap_scalar(),
            width,
            sign,
        }));
        Ok(TypedRValue {
            src: RValue::new_var(dst),
            t: QualifiedType {
                t: ctype::INT_TYPE,
                qualifiers: Qualifiers::empty(),
            },
        })
    } else {
        todo!()
    }
}
