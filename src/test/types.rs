use lang_c::span::Span;

use crate::ctype::FunctionArgs;
use crate::ctype::{self, CType, QualifiedType, Qualifiers};
use crate::error::CompileError;

use super::util::*;

#[test]
fn test_function_decl_1() {
    let (tu_result, ec) = translate("void f(int x);");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("f").unwrap().unwrap_static_var();
    assert_eq!(
        decl.0.t,
        CType::Function {
            result: Box::new(QualifiedType {
                t: CType::Void,
                qualifiers: Qualifiers::empty()
            }),
            args: FunctionArgs::List(vec![(
                QualifiedType {
                    t: ctype::INT_TYPE,
                    qualifiers: Qualifiers::empty()
                },
                Some("x".to_string()),
                Span::span(7, 12)
            )]),
            vararg: false
        }
    );
}

#[test]
fn test_function_decl_2() {
    let (tu_result, ec) = translate("int f(void);");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("f").unwrap().unwrap_static_var();
    assert_eq!(
        decl.0.t,
        CType::Function {
            result: Box::new(QualifiedType {
                t: ctype::INT_TYPE,
                qualifiers: Qualifiers::empty()
            }),
            args: FunctionArgs::Void,
            vararg: false
        }
    );
}

#[test]
fn test_function_decl_3() {
    let (tu_result, ec) = translate("int f(void, void);");
    assert!(tu_result.is_err());
    assert_eq!(ec.get_error_count(), 1);
    assert_eq!(ec.get_first_error().unwrap().0, CompileError::VoidParameter);
}

#[test]
fn test_function_decl_4() {
    let (tu_result, ec) = translate("int f(const void);");
    assert!(tu_result.is_err());
    assert_eq!(ec.get_error_count(), 1);
    assert_eq!(
        ec.get_first_error().unwrap().0,
        CompileError::QualifiedVoidParameter
    );
}

#[test]
fn test_function_decl_5() {
    let (tu_result, ec) = translate("int f(void x);");
    assert!(tu_result.is_err());
    assert_eq!(ec.get_error_count(), 1);
    assert_eq!(
        ec.get_first_error().unwrap().0,
        CompileError::NamedVoidParameter
    );
}

#[test]
fn test_2d_array_decl_1() {
    let (tu_result, ec) = translate("int a[2][3];"); // declare a as array 2 of array 3 of int
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("a").unwrap().unwrap_static_var();
    assert_eq!(
        decl.0.t,
        CType::Array(
            Box::new(QualifiedType {
                t: CType::Array(
                    Box::new(QualifiedType {
                        t: ctype::INT_TYPE,
                        qualifiers: Qualifiers::empty()
                    }),
                    Some(3)
                ),
                qualifiers: Qualifiers::empty()
            }),
            Some(2)
        )
    );
}

#[test]
fn test_2d_array_decl_2() {
    let (tu_result, ec) = translate("typedef int arr[3]; arr a[2];");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("a").unwrap().unwrap_static_var();
    assert_eq!(
        decl.0.t,
        CType::Array(
            Box::new(QualifiedType {
                t: CType::Array(
                    Box::new(QualifiedType {
                        t: ctype::INT_TYPE,
                        qualifiers: Qualifiers::empty()
                    }),
                    Some(3)
                ),
                qualifiers: Qualifiers::empty()
            }),
            Some(2)
        )
    );
}

#[test]
fn test_2d_array_decl_3() {
    let (tu_result, ec) = translate("int * const * volatile a[2][4];");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("a").unwrap().unwrap_static_var();
    assert_eq!(
        decl.0.t,
        CType::Array(
            Box::new(QualifiedType {
                t: CType::Array(
                    Box::new(QualifiedType {
                        t: CType::Pointer(Box::new(QualifiedType {
                            t: CType::Pointer(Box::new(QualifiedType {
                                t: ctype::INT_TYPE,
                                qualifiers: Qualifiers::empty()
                            })),
                            qualifiers: Qualifiers::CONST
                        })),
                        qualifiers: Qualifiers::VOLATILE
                    }),
                    Some(4)
                ),
                qualifiers: Qualifiers::empty()
            }),
            Some(2)
        )
    );
}
