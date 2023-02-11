use lang_c::ast::BinaryOperatorExpression;
use lang_c::{ast::Expression, span::Node};

use crate::block_emitter::BlockEmitter;
use crate::error::{CompileError, ErrorCollector};
use crate::ir;
use crate::name_scope::NameScope;

use super::{add, assign, sub};
use super::{compile_expression, usual_arithmetic_convert, TypedSrc};

pub fn compile_binary_operator(
    op: Node<BinaryOperatorExpression>,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()> {
    use lang_c::ast::BinaryOperator;
    match op.node.operator.node {
        BinaryOperator::Assign => assign::compile_assign(*op.node.lhs, *op.node.rhs, scope, be, ec),
        BinaryOperator::Plus => add::compile_add(*op.node.lhs, *op.node.rhs, scope, be, ec),
        BinaryOperator::Minus => sub::compile_sub(*op.node.lhs, *op.node.rhs, scope, be, ec),
        BinaryOperator::Multiply => compile_multiplicative(*op.node.lhs, *op.node.rhs, ir::Op::Mul, scope, be, ec),
        BinaryOperator::Divide => compile_multiplicative(*op.node.lhs, *op.node.rhs, ir::Op::Div, scope, be, ec),
        BinaryOperator::Modulo => compile_multiplicative(*op.node.lhs, *op.node.rhs, ir::Op::Mod, scope, be, ec),
        BinaryOperator::BitwiseAnd => compile_bitwise(*op.node.lhs, *op.node.rhs, ir::Op::BAnd, scope, be, ec),
        BinaryOperator::BitwiseOr => compile_bitwise(*op.node.lhs, *op.node.rhs, ir::Op::BOr, scope, be, ec),
        BinaryOperator::BitwiseXor => compile_bitwise(*op.node.lhs, *op.node.rhs, ir::Op::BXor, scope, be, ec),
        BinaryOperator::AssignPlus => add::compile_assign_add(*op.node.lhs, *op.node.rhs, scope, be, ec),
        _ => todo!(),
    }
}

fn compile_multiplicative<F>(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    constr: F,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()>
where
    F: FnOnce(ir::BinaryOp) -> ir::Op,
{
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs = compile_expression(lhs, scope, be, ec)?;
    let rhs = compile_expression(rhs, scope, be, ec)?;

    if lhs.t.t.is_arithmetic() && rhs.t.t.is_arithmetic() {
        let (lhs, rhs) = usual_arithmetic_convert((lhs, lhs_span), (rhs, rhs_span), scope, be, ec)?;
        if lhs.t.t.is_integer() {
            let (width, sign) = lhs.t.t.get_width_sign().unwrap();
            let target = scope.alloc_temp();
            be.append_operation(constr(ir::BinaryOp {
                dst: target.clone(),
                width,
                sign,
                lhs: lhs.src,
                rhs: rhs.src,
            }));
            Ok(TypedSrc {
                src: ir::Src::Var(target),
                t: lhs.t,
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

fn compile_bitwise<F>(
    lhs: Node<Expression>,
    rhs: Node<Expression>,
    constr: F,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedSrc, ()>
where
    F: FnOnce(ir::BinaryUnsignedOp) -> ir::Op,
{
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;
    let lhs = compile_expression(lhs, scope, be, ec)?;
    let rhs = compile_expression(rhs, scope, be, ec)?;

    if lhs.t.t.is_integer() && rhs.t.t.is_integer() {
        let (lhs, rhs) = usual_arithmetic_convert((lhs, lhs_span), (rhs, rhs_span), scope, be, ec)?;
        let (width, _) = lhs.t.t.get_width_sign().unwrap();
        let target = scope.alloc_temp();
        be.append_operation(constr(ir::BinaryUnsignedOp {
            dst: target.clone(),
            width,
            lhs: lhs.src,
            rhs: rhs.src,
        }));
        Ok(TypedSrc {
            src: ir::Src::Var(target),
            t: lhs.t,
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
