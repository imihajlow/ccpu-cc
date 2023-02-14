use lang_c::ast::BinaryOperatorExpression;
use lang_c::span::Span;
use lang_c::{ast::Expression, span::Node};

use crate::block_emitter::BlockEmitter;
use crate::compile::add::compile_index;
use crate::compile::relational;
use crate::compile::shift::{
    compile_lshift, compile_lshift_inner, compile_rshift, compile_rshift_inner,
};
use crate::error::{CompileError, ErrorCollector};
use crate::ir;
use crate::lvalue::TypedLValue;
use crate::name_scope::NameScope;
use crate::rvalue::{RValue, TypedRValue};

use super::{add, assign, sub};
use super::{compile_expression, usual_arithmetic_convert};

pub fn compile_binary_operator(
    op: Node<BinaryOperatorExpression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    use lang_c::ast::BinaryOperator;
    match op.node.operator.node {
        BinaryOperator::Assign => assign::compile_assign(*op.node.lhs, *op.node.rhs, scope, be, ec),
        BinaryOperator::Plus => add::compile_add(*op.node.lhs, *op.node.rhs, scope, be, ec),
        BinaryOperator::Minus => sub::compile_sub(*op.node.lhs, *op.node.rhs, scope, be, ec),
        BinaryOperator::Multiply => {
            compile_multiplicative(*op.node.lhs, *op.node.rhs, ir::Op::Mul, scope, be, ec)
        }
        BinaryOperator::Divide => {
            compile_multiplicative(*op.node.lhs, *op.node.rhs, ir::Op::Div, scope, be, ec)
        }
        BinaryOperator::Modulo => {
            compile_multiplicative(*op.node.lhs, *op.node.rhs, ir::Op::Mod, scope, be, ec)
        }
        BinaryOperator::BitwiseAnd => {
            compile_bitwise(*op.node.lhs, *op.node.rhs, ir::Op::BAnd, scope, be, ec)
        }
        BinaryOperator::BitwiseOr => {
            compile_bitwise(*op.node.lhs, *op.node.rhs, ir::Op::BOr, scope, be, ec)
        }
        BinaryOperator::BitwiseXor => {
            compile_bitwise(*op.node.lhs, *op.node.rhs, ir::Op::BXor, scope, be, ec)
        }
        BinaryOperator::ShiftLeft => compile_lshift(*op.node.lhs, *op.node.rhs, scope, be, ec),
        BinaryOperator::ShiftRight => compile_rshift(*op.node.lhs, *op.node.rhs, scope, be, ec),
        BinaryOperator::AssignPlus => compile_binary_and_assign(
            *op.node.lhs,
            *op.node.rhs,
            add::compile_add_inner,
            scope,
            be,
            ec,
        ),
        BinaryOperator::AssignMinus => compile_binary_and_assign(
            *op.node.lhs,
            *op.node.rhs,
            sub::compile_sub_inner,
            scope,
            be,
            ec,
        ),
        BinaryOperator::AssignMultiply => {
            compile_binary_and_assign(*op.node.lhs, *op.node.rhs, compile_mul_inner, scope, be, ec)
        }
        BinaryOperator::AssignDivide => {
            compile_binary_and_assign(*op.node.lhs, *op.node.rhs, compile_div_inner, scope, be, ec)
        }
        BinaryOperator::AssignModulo => {
            compile_binary_and_assign(*op.node.lhs, *op.node.rhs, compile_mod_inner, scope, be, ec)
        }
        BinaryOperator::AssignBitwiseAnd => compile_binary_and_assign(
            *op.node.lhs,
            *op.node.rhs,
            compile_band_inner,
            scope,
            be,
            ec,
        ),
        BinaryOperator::AssignBitwiseOr => {
            compile_binary_and_assign(*op.node.lhs, *op.node.rhs, compile_bor_inner, scope, be, ec)
        }
        BinaryOperator::AssignBitwiseXor => compile_binary_and_assign(
            *op.node.lhs,
            *op.node.rhs,
            compile_bxor_inner,
            scope,
            be,
            ec,
        ),
        BinaryOperator::AssignShiftLeft => compile_binary_and_assign(
            *op.node.lhs,
            *op.node.rhs,
            compile_lshift_inner,
            scope,
            be,
            ec,
        ),
        BinaryOperator::AssignShiftRight => compile_binary_and_assign(
            *op.node.lhs,
            *op.node.rhs,
            compile_rshift_inner,
            scope,
            be,
            ec,
        ),
        BinaryOperator::Less => {
            relational::compile_less_than(*op.node.lhs, *op.node.rhs, scope, be, ec)
        }
        BinaryOperator::LessOrEqual => {
            relational::compile_less_or_equal(*op.node.lhs, *op.node.rhs, scope, be, ec)
        }
        BinaryOperator::Greater => {
            relational::compile_greater_than(*op.node.lhs, *op.node.rhs, scope, be, ec)
        }
        BinaryOperator::GreaterOrEqual => {
            relational::compile_greater_or_equal(*op.node.lhs, *op.node.rhs, scope, be, ec)
        }
        BinaryOperator::Equals => {
            relational::compile_equal_to(*op.node.lhs, *op.node.rhs, scope, be, ec)
        }
        BinaryOperator::NotEquals => {
            relational::compile_not_equal_to(*op.node.lhs, *op.node.rhs, scope, be, ec)
        }
        BinaryOperator::LogicalAnd => be.append_logical_and(*op.node.lhs, *op.node.rhs, scope, ec),
        BinaryOperator::LogicalOr => be.append_logical_or(*op.node.lhs, *op.node.rhs, scope, ec),
        BinaryOperator::Index => compile_index(*op.node.lhs, *op.node.rhs, scope, be, ec),
    }
}

fn compile_multiplicative<F>(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    constr: F,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()>
where
    F: FnOnce(ir::BinaryOp) -> ir::Op,
{
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs = compile_expression(lhs, scope, be, ec)?;
    let rhs = compile_expression(rhs, scope, be, ec)?;

    compile_multiplicative_inner((lhs, lhs_span), (rhs, rhs_span), constr, scope, be, ec)
}

fn compile_multiplicative_inner<F>(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    constr: F,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()>
where
    F: FnOnce(ir::BinaryOp) -> ir::Op,
{
    let (lhs, lhs_span) = lhs;
    let (rhs, rhs_span) = rhs;

    if lhs.t.t.is_arithmetic() && rhs.t.t.is_arithmetic() {
        let (lhs, rhs) = usual_arithmetic_convert((lhs, lhs_span), (rhs, rhs_span), scope, be, ec)?;
        let (lhs_scalar, lhs_type) = lhs.unwrap_scalar_and_type();
        if lhs_type.t.is_integer() {
            let (width, sign) = lhs_type.t.get_width_sign().unwrap();
            let target = scope.alloc_temp();
            be.append_operation(constr(ir::BinaryOp {
                dst: target.clone(),
                width,
                sign,
                lhs: lhs_scalar,
                rhs: rhs.unwrap_scalar(),
            }));
            Ok(TypedRValue {
                src: RValue::new_var(target),
                t: lhs_type,
            })
        } else {
            todo!()
        }
    } else {
        if !lhs.t.t.is_arithmetic() {
            ec.record_error(CompileError::ArithmeticTypeRequired, lhs_span)?;
        } else {
            ec.record_error(CompileError::ArithmeticTypeRequired, rhs_span)?;
        }
        unreachable!();
    }
}

fn compile_mul_inner(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    compile_multiplicative_inner(lhs, rhs, ir::Op::Mul, scope, be, ec)
}

fn compile_div_inner(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    compile_multiplicative_inner(lhs, rhs, ir::Op::Div, scope, be, ec)
}

fn compile_mod_inner(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    compile_multiplicative_inner(lhs, rhs, ir::Op::Mod, scope, be, ec)
}

fn compile_bitwise<F>(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    constr: F,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()>
where
    F: FnOnce(ir::BinaryUnsignedOp) -> ir::Op,
{
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs = compile_expression(lhs, scope, be, ec)?;
    let rhs = compile_expression(rhs, scope, be, ec)?;

    compile_bitwise_inner((lhs, lhs_span), (rhs, rhs_span), constr, scope, be, ec)
}

fn compile_bitwise_inner<F>(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    constr: F,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()>
where
    F: FnOnce(ir::BinaryUnsignedOp) -> ir::Op,
{
    let (lhs, lhs_span) = lhs;
    let (rhs, rhs_span) = rhs;

    if lhs.t.t.is_integer() && rhs.t.t.is_integer() {
        let (lhs, rhs) = usual_arithmetic_convert((lhs, lhs_span), (rhs, rhs_span), scope, be, ec)?;
        let (lhs_scalar, lhs_type) = lhs.unwrap_scalar_and_type();
        let (width, _) = lhs_type.t.get_width_sign().unwrap();
        let target = scope.alloc_temp();
        be.append_operation(constr(ir::BinaryUnsignedOp {
            dst: target.clone(),
            width,
            lhs: lhs_scalar,
            rhs: rhs.unwrap_scalar(),
        }));
        Ok(TypedRValue {
            src: RValue::new_var(target),
            t: lhs_type,
        })
    } else {
        if !lhs.t.t.is_integer() {
            ec.record_error(CompileError::IntegerTypeRequired, lhs_span)?;
        } else {
            ec.record_error(CompileError::IntegerTypeRequired, rhs_span)?;
        }
        unreachable!();
    }
}

fn compile_band_inner(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    compile_bitwise_inner(lhs, rhs, ir::Op::BAnd, scope, be, ec)
}

fn compile_bor_inner(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    compile_bitwise_inner(lhs, rhs, ir::Op::BOr, scope, be, ec)
}

fn compile_bxor_inner(
    lhs: (TypedRValue, Span),
    rhs: (TypedRValue, Span),
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    compile_bitwise_inner(lhs, rhs, ir::Op::BXor, scope, be, ec)
}

fn compile_binary_and_assign<F>(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    inner: F,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()>
where
    F: FnOnce(
        (TypedRValue, Span),
        (TypedRValue, Span),
        &mut NameScope,
        &mut BlockEmitter,
        &mut ErrorCollector,
    ) -> Result<TypedRValue, ()>,
{
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs_lval = TypedLValue::new_compile(lhs, scope, be, ec)?;
    let rhs_val = compile_expression(rhs, scope, be, ec)?;
    let lhs_val = lhs_lval.clone().compile_into_rvalue(scope, be)?;
    let inner_result = inner((lhs_val, lhs_span), (rhs_val, rhs_span), scope, be, ec)?;
    assign::compile_assign_to_lval(lhs_lval, (inner_result, rhs_span), scope, be, ec)
}
