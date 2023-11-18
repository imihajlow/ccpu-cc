use crate::block_emitter::BlockEmitter;
use crate::compile::compile_argument;
use crate::error::CompileError;
use crate::generic_ir::{Op, UnaryUnsignedOp, VaListIncOp, VaStartOp, Width};
use crate::name_scope::NameScope;
use crate::rvalue::{RValue, TypedRValue};
use crate::ErrorCollector;
use std::fmt::Formatter;

use lang_c::span::Span;

use crate::ctype::{CType, FunctionArgs, QualifiedType, Qualifiers};

#[derive(Debug, Copy, Clone)]
pub enum BuiltinFunction {
    VaStart,
    VaIncrement,
    BSwap(Width),
}

pub fn is_builtin_name(name: &str) -> bool {
    match name {
        "__builtin_va_list" => true,
        "__builtin_va_start" => true,
        "__builtin_va_increment" => true,
        "__builtin_bswap16" => true,
        "__builtin_bswap32" => true,
        _ => false,
    }
}

pub fn get_builtin_type(name: &str) -> Option<CType> {
    match name {
        "__builtin_va_list" => Some(CType::VaList),
        _ => None,
    }
}

pub fn get_builtin_function(name: &str) -> Option<(BuiltinFunction, QualifiedType)> {
    match name {
        "__builtin_va_start" => Some((
            BuiltinFunction::VaStart,
            QualifiedType {
                qualifiers: Qualifiers::empty(),
                t: CType::Function {
                    result: Box::new(QualifiedType {
                        qualifiers: Qualifiers::empty(),
                        t: CType::Void,
                    }),
                    args: FunctionArgs::List(vec![(
                        QualifiedType {
                            qualifiers: Qualifiers::empty(),
                            t: CType::VaList,
                        },
                        Some("ap".to_owned()),
                        Span::none(),
                    )]),
                    vararg: true,
                },
            },
        )),
        "__builtin_va_increment" => Some((
            BuiltinFunction::VaIncrement,
            QualifiedType {
                qualifiers: Qualifiers::empty(),
                t: CType::Function {
                    result: Box::new(QualifiedType {
                        qualifiers: Qualifiers::empty(),
                        t: CType::VaList,
                    }),
                    args: FunctionArgs::List(vec![(
                        QualifiedType {
                            qualifiers: Qualifiers::empty(),
                            t: CType::VaList,
                        },
                        Some("ap".to_owned()),
                        Span::none(),
                    )]),
                    vararg: false,
                },
            },
        )),
        "__builtin_bswap16" => Some((
            BuiltinFunction::BSwap(Width::Word),
            QualifiedType {
                qualifiers: Qualifiers::empty(),
                t: CType::Function {
                    result: Box::new(QualifiedType {
                        qualifiers: Qualifiers::empty(),
                        t: CType::Int(2, false),
                    }),
                    args: FunctionArgs::List(vec![(
                        QualifiedType {
                            qualifiers: Qualifiers::empty(),
                            t: CType::Int(2, false),
                        },
                        Some("x16".to_owned()),
                        Span::none(),
                    )]),
                    vararg: false,
                },
            },
        )),
        "__builtin_bswap32" => Some((
            BuiltinFunction::BSwap(Width::Dword),
            QualifiedType {
                qualifiers: Qualifiers::empty(),
                t: CType::Function {
                    result: Box::new(QualifiedType {
                        qualifiers: Qualifiers::empty(),
                        t: CType::Int(4, false),
                    }),
                    args: FunctionArgs::List(vec![(
                        QualifiedType {
                            qualifiers: Qualifiers::empty(),
                            t: CType::Int(4, false),
                        },
                        Some("x16".to_owned()),
                        Span::none(),
                    )]),
                    vararg: false,
                },
            },
        )),
        _ => None,
    }
}

pub fn compile_builtin_call(
    c: BuiltinFunction,
    mut args: Vec<(TypedRValue, Span)>,
    span: Span,
    scope: &mut NameScope,
    be: &mut BlockEmitter,
    ec: &mut ErrorCollector,
) -> Result<TypedRValue, ()> {
    match c {
        BuiltinFunction::VaStart => {
            let dst = scope.alloc_temp();
            be.append_operation(Op::VaStart(VaStartOp { dst: dst.clone() }));
            Ok(TypedRValue {
                src: RValue::new_var(dst),
                t: QualifiedType {
                    t: CType::VaList,
                    qualifiers: Qualifiers::empty(),
                },
            })
        }
        BuiltinFunction::VaIncrement => {
            if args.len() > 1 {
                ec.record_error(CompileError::TooManyArguments(args.len(), 1), span)?;
                unreachable!()
            }
            if args.len() < 1 {
                ec.record_error(CompileError::TooFewArguments(args.len(), 1), span)?;
                unreachable!()
            }
            let (arg, arg_span) = args.pop().unwrap();
            if !arg.t.t.is_valist() {
                ec.record_error(
                    CompileError::IncompatibleTypes(
                        arg.t,
                        QualifiedType {
                            t: CType::VaList,
                            qualifiers: Qualifiers::empty(),
                        },
                    ),
                    arg_span,
                )?;
                unreachable!()
            }
            let dst = scope.alloc_temp();
            be.append_operation(Op::VaListInc(VaListIncOp {
                dst: dst.clone(),
                src: arg.src.unwrap_scalar(),
            }));
            Ok(TypedRValue {
                src: RValue::new_var(dst),
                t: QualifiedType {
                    t: CType::VaList,
                    qualifiers: Qualifiers::empty(),
                },
            })
        }
        BuiltinFunction::BSwap(w) => {
            if args.len() > 1 {
                ec.record_error(CompileError::TooManyArguments(args.len(), 1), span)?;
                unreachable!()
            }
            if args.len() < 1 {
                ec.record_error(CompileError::TooFewArguments(args.len(), 1), span)?;
                unreachable!()
            }
            let (arg, arg_span) = args.pop().unwrap();
            let arg_type = QualifiedType {
                qualifiers: Qualifiers::empty(),
                t: CType::Int(w as u8, false),
            };
            let (arg_location, arg_width) =
                compile_argument(Some(arg_type.clone()), arg, arg_span, scope, be, ec)?;
            assert!(arg_width == w);
            let dst = scope.alloc_temp();
            be.append_operation(Op::ByteSwap(UnaryUnsignedOp {
                dst: dst.clone(),
                src: arg_location,
                width: w,
            }));
            Ok(TypedRValue {
                src: RValue::new_var(dst),
                t: arg_type,
            })
        }
    }
}

impl std::fmt::Display for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            BuiltinFunction::VaStart => f.write_str("__builtin_va_start"),
            BuiltinFunction::VaIncrement => f.write_str("__builtin_va_increment"),
            BuiltinFunction::BSwap(Width::Word) => f.write_str("__buitin_bswap16"),
            BuiltinFunction::BSwap(Width::Dword) => f.write_str("__buitin_bswap32"),
            BuiltinFunction::BSwap(_) => unreachable!(),
        }
    }
}
