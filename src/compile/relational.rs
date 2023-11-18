use lang_c::span::Span;
use lang_c::{ast::Expression, span::Node};

use crate::block_emitter::BlockEmitter;
use crate::ctype::{QualifiedType, Qualifiers};
use crate::error::{CompileWarning, ErrorCollector};
use crate::ir::Width;
use crate::name_scope::NameScope;
use crate::rvalue::{RValue, TypedRValue};
use crate::{ctype, ir};

use super::{compile_expression, integer_promote, usual_arithmetic_convert};

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
                dst: dst.clone(),
                dst_width: ir::Width::INT_WIDTH,
                desc: ir::CompareDesc {
                    kind,
                    lhs: lhs.unwrap_scalar(),
                    rhs: rhs.unwrap_scalar(),
                    width,
                    sign,
                },
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
            dst: dst.clone(),
            dst_width: ir::Width::INT_WIDTH,
            desc: ir::CompareDesc {
                kind,
                lhs: lhs.unwrap_scalar(),
                rhs: rhs.unwrap_scalar(),
                width,
                sign,
            },
        }));
        Ok(TypedRValue {
            src: RValue::new_var(dst),
            t: QualifiedType {
                t: ctype::INT_TYPE,
                qualifiers: Qualifiers::empty(),
            },
        })
    } else if (lhs.t.t.is_integer() && rhs.t.t.is_pointer())
        || (lhs.t.t.is_pointer() && rhs.t.t.is_integer())
    {
        ec.record_warning(
            CompileWarning::IncompatibleTypes(lhs.t.clone(), rhs.t.clone()),
            lhs_span,
        )?;
        let (pointer, integer, integer_span, kind) = if lhs.t.t.is_dereferencable() {
            (lhs, rhs, rhs_span, kind)
        } else {
            (rhs, lhs, lhs_span, kind.flip())
        };
        let integer = integer_promote((integer, integer_span), scope, be, ec)?;
        let (integer_width, integer_sign) = integer.t.t.get_width_sign().unwrap();
        let rhs_reg = scope.alloc_temp();
        be.append_operation(ir::Op::Conv(ir::ConvOp {
            dst: rhs_reg.clone(),
            dst_sign: false,
            dst_width: Width::PTR_WIDTH,
            src_width: integer_width,
            src_sign: integer_sign,
            src: integer.unwrap_scalar(),
        }));
        let dst = scope.alloc_temp();
        be.append_operation(ir::Op::Compare(ir::CompareOp {
            dst: dst.clone(),
            dst_width: ir::Width::INT_WIDTH,
            desc: ir::CompareDesc {
                kind,
                lhs: pointer.unwrap_scalar(),
                rhs: ir::Scalar::Var(rhs_reg),
                width: Width::PTR_WIDTH,
                sign: false,
            },
        }));
        Ok(TypedRValue {
            src: RValue::new_var(dst),
            t: QualifiedType {
                t: ctype::INT_TYPE,
                qualifiers: Qualifiers::empty(),
            },
        })
    } else {
        todo!("compare {} with {} by {}", lhs.t, rhs.t, kind)
    }
}
