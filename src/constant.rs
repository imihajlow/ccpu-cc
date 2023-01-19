use lang_c::span::Span;
use lang_c::{
    ast::{CastExpression, Constant, Expression, Initializer},
    span::Node,
};

use crate::{
    ctype::{CType, QualifiedType, Qualifiers},
    error::{CompileError, CompileWarning, ErrorCollector},
    initializer::{TypedValue, Value},
    machine,
    translation_unit::TranslationUnit,
    type_builder::{self, TypeBuilder},
    type_registry::TypeRegistry,
};

pub fn compute_constant_initializer(
    initializer: Node<Initializer>,
    target_type: &QualifiedType,
    allow_var: bool,
    tu: &TranslationUnit,
    ec: &mut ErrorCollector,
) -> Result<Value, ()> {
    match initializer.node {
        Initializer::Expression(expr) => {
            let v = compute_constant_expr(*expr, allow_var, tu, ec)?;
            cast(v, target_type, initializer.span, ec)
        }
        Initializer::List(_) => todo!(),
    }
}

pub fn compute_constant_expr(
    expr: Node<Expression>,
    allow_var: bool,
    tu: &TranslationUnit,
    ec: &mut ErrorCollector,
) -> Result<TypedValue, ()> {
    let span = expr.span;
    let expr = expr.node;
    match expr {
        Expression::Identifier(id) => {
            if !allow_var {
                ec.record_error(CompileError::VariablesForbidden, id.span)?;
                unreachable!();
            }
            match tu.lookup_global_declaration(&id.node.name) {
                None => {
                    ec.record_error(CompileError::UnknownIdentifier(id.node.name), id.span)?;
                    unreachable!();
                }
                Some(decl) => {
                    if !decl.t.is_const() || decl.initializer.is_none() {
                        ec.record_error(CompileError::NonConstInConstExpr, id.span)?;
                        unreachable!();
                    }
                    Ok(TypedValue {
                        t: decl.t.clone(),
                        val: decl.initializer.as_ref().unwrap().clone(),
                    })
                }
            }
        }
        Expression::Constant(c) => {
            match c.node {
                Constant::Integer(i) => {
                    use lang_c::ast::{IntegerBase, IntegerSize};
                    let radix = match i.base {
                        IntegerBase::Decimal => 10,
                        IntegerBase::Octal => 8,
                        IntegerBase::Hexadecimal => 16,
                        IntegerBase::Binary => 2,
                    };
                    let num = i128::from_str_radix(&i.number, radix).unwrap(); // should be already checked by lang_c
                    if i.suffix.imaginary {
                        unimplemented!();
                    }
                    let size = match i.suffix.size {
                        IntegerSize::Int => machine::INT_SIZE,
                        IntegerSize::Long => machine::LONG_SIZE,
                        IntegerSize::LongLong => machine::LLONG_SIZE,
                    };
                    let t = QualifiedType {
                        t: CType::Int(size, !i.suffix.unsigned),
                        qualifiers: Qualifiers::CONST,
                    };
                    let val = Value::Int(num);
                    Ok(TypedValue { t, val })
                }
                Constant::Float(_) => todo!(),
                Constant::Character(_) => todo!(),
            }
        }
        Expression::Cast(c) => process_cast_expression_node(*c, allow_var, tu, ec),
        Expression::StringLiteral(_) => todo!(),
        Expression::Member(_) => todo!(),
        Expression::Call(_) => todo!(),
        Expression::CompoundLiteral(_) => todo!(),
        Expression::SizeOfTy(_) => todo!(),
        Expression::SizeOfVal(_) => todo!(),
        Expression::AlignOf(_) => todo!(),
        Expression::UnaryOperator(_) => todo!(),
        Expression::BinaryOperator(_) => todo!(),
        Expression::Conditional(_) => todo!(),
        Expression::Comma(_) => todo!(),
        Expression::OffsetOf(_) => todo!(),
        Expression::VaArg(_) => todo!(),
        Expression::Statement(_) => unimplemented!(), // GNU extension
        Expression::GenericSelection(_) => unimplemented!(),
    }
}

fn process_cast_expression_node(
    c: Node<CastExpression>,
    allow_var: bool,
    tu: &TranslationUnit,
    ec: &mut ErrorCollector,
) -> Result<TypedValue, ()> {
    let mut type_builder = TypeBuilder::new();
    for sq in c.node.type_name.node.specifiers {
        type_builder.add_specifier_qualifier_node(sq, &tu.type_registry, ec)?;
    }
    let type_builder = type_builder.stage2(c.span, ec)?;
    let new_type = if let Some(decl) = c.node.type_name.node.declarator {
        type_builder.process_declarator_node(decl, ec)?.1
    } else {
        type_builder.finalize()
    };
    let value = compute_constant_expr(*c.node.expression, allow_var, tu, ec)?;

    Ok(TypedValue {
        val: cast(value, &new_type, c.span, ec)?,
        t: new_type
    })
}

fn cast(value: TypedValue, new_type: &QualifiedType, span: Span, ec: &mut ErrorCollector) -> Result<Value, ()> {
    if !value.t.is_explicit_castable_to(&new_type) {
        ec.record_error(
            CompileError::BadCast(format!("{}", value.t), format!("{}", new_type)),
            span,
        )?;
    }
    let new_v = match new_type.t {
        CType::Void => Value::Void,
        CType::Int(new_size, new_sign) => match value.t.t {
            CType::Int(old_size, old_sign) => {
                cast_int(old_size, old_sign, new_size, new_sign, value.val)
            }
            CType::Bool => cast_from_bool(value.val),
            CType::Pointer(_) => cast_int(machine::PTR_SIZE, false, new_size, new_sign, value.val),
            CType::Float(_) => todo!(),
            CType::Array(_, _) => {
                ec.record_error(CompileError::NonConstInConstExpr, span)?;
                unreachable!()
            }
            _ => unreachable!(),
        },
        CType::Float(_) => todo!(),
        CType::Pointer(_) => match value.t.t {
            CType::Int(old_size, old_sign) => {
                cast_int(old_size, old_sign, machine::PTR_SIZE, false, value.val)
            }
            CType::Bool => cast_from_bool(value.val),
            CType::Pointer(_) => value.val,
            CType::Float(_) => todo!(),
            CType::Array(_, _) => {
                ec.record_error(CompileError::NonConstInConstExpr, span)?;
                unreachable!()
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };
    Ok(new_v)
}

fn cast_int_ff<F>(size_to: u8, sign_to: bool, value: F) -> i128
where
    F: num_traits::AsPrimitive<u8>,
    F: num_traits::AsPrimitive<i8>,
    F: num_traits::AsPrimitive<u16>,
    F: num_traits::AsPrimitive<i16>,
    F: num_traits::AsPrimitive<u32>,
    F: num_traits::AsPrimitive<i32>,
    F: num_traits::AsPrimitive<u64>,
    F: num_traits::AsPrimitive<i64>,
{
    use num_traits::AsPrimitive;
    match (size_to, sign_to) {
        (1, true) => AsPrimitive::<i8>::as_(value) as i128,
        (1, false) => AsPrimitive::<u8>::as_(value) as i128,
        (2, true) => AsPrimitive::<i16>::as_(value) as i128,
        (2, false) => AsPrimitive::<u16>::as_(value) as i128,
        (4, true) => AsPrimitive::<i32>::as_(value) as i128,
        (4, false) => AsPrimitive::<u32>::as_(value) as i128,
        (8, true) => AsPrimitive::<i64>::as_(value) as i128,
        (8, false) => AsPrimitive::<u64>::as_(value) as i128,
        _ => unreachable!(),
    }
}

fn cast_int(size_from: u8, sign_from: bool, size_to: u8, sign_to: bool, value: Value) -> Value {
    if let Value::Int(value) = value {
        let new_val = match (size_from, sign_from) {
            (1, true) => cast_int_ff(size_to, sign_to, value as i8),
            (1, false) => cast_int_ff(size_to, sign_to, value as u8),
            (2, true) => cast_int_ff(size_to, sign_to, value as i16),
            (2, false) => cast_int_ff(size_to, sign_to, value as u16),
            (4, true) => cast_int_ff(size_to, sign_to, value as i32),
            (4, false) => cast_int_ff(size_to, sign_to, value as u32),
            (8, true) => cast_int_ff(size_to, sign_to, value as i64),
            (8, false) => cast_int_ff(size_to, sign_to, value as u64),
            _ => unreachable!(),
        };
        Value::Int(new_val)
    } else {
        panic!("value doesn't match type")
    }
}

fn cast_from_bool(value: Value) -> Value {
    if let Value::Int(value) = value {
        let new_val = if value == 0 { 0 } else { 1 };
        Value::Int(new_val)
    } else {
        panic!("value doesn't match type")
    }
}

fn parse_char_literal(s: &str, ec: &mut ErrorCollector) -> Result<TypedValue, ()> {
    todo!()
}
