use std::assert_matches::assert_matches;

use crate::ctype::{self, CType, QualifiedType, Qualifiers};
use crate::initializer::Constant;
use crate::name_scope::GlobalStorageClass;

use super::util::*;

#[test]
fn test_global_var_1() {
    let (tu_result, ec) = translate("int x;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::INT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::empty());
    assert_eq!(decl.2, &GlobalStorageClass::Default);
}

#[test]
fn test_global_var_2() {
    let (tu_result, ec) = translate("long long unsigned int x;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::ULLONG_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::empty());
    assert_eq!(decl.2, &GlobalStorageClass::Default);
}

#[test]
fn test_global_var_3() {
    let (tu_result, ec) = translate("static const char x;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UCHAR_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Static);
}

#[test]
fn test_global_var_4() {
    let (tu_result, ec) = translate("signed int x, * const volatile y;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::INT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::empty());
    assert_eq!(decl.2, &GlobalStorageClass::Default);

    let decl = tu.scope.get("y").unwrap().unwrap_static_var();
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST | Qualifiers::VOLATILE);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_eq!(
        decl.0.t,
        CType::Pointer(Box::new(QualifiedType {
            t: ctype::INT_TYPE,
            qualifiers: Qualifiers::empty()
        }))
    );
}

#[test]
fn test_global_var_5() {
    let (tu_result, ec) = translate("typedef char new_char; static const new_char x;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UCHAR_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Static);
}

#[test]
fn test_global_var_err_1() {
    let (tu_result, ec) = translate("static extern int x;");
    assert!(tu_result.is_err());
    assert_eq!(ec.get_error_count(), 1);

    let (tu_result, ec) = translate("static int x; int x;");
    assert!(tu_result.is_err());
    assert_eq!(ec.get_error_count(), 1);

    let (tu_result, ec) = translate("long short int x;");
    assert!(tu_result.is_err());
    assert_eq!(ec.get_error_count(), 1);
}

#[test]
fn test_global_var_init_1() {
    let (tu_result, ec) = translate("const long x = 42;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::SLONG_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(42));
}

#[test]
fn test_global_var_init_2() {
    let (tu_result, ec) = translate("unsigned char x = 0x01020304LLU;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UCHAR_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::empty());
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0x04));
}

#[test]
fn test_global_var_init_3() {
    let (tu_result, ec) = translate("const long x = (char)0x55667788;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::SLONG_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0x88));
}

#[test]
fn test_global_var_init_4() {
    let (tu_result, ec) = translate("const long x = 0x0102030405;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::SLONG_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0x02030405));
}

#[test]
fn test_global_var_init_unary_1() {
    let (tu_result, ec) = translate("const int x = +22;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::SINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(22));
}

#[test]
fn test_global_var_init_unary_2() {
    let (tu_result, ec) = translate("const int x = -2200;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::SINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(-2200));
}

#[test]
fn test_global_var_init_unary_3() {
    let (tu_result, ec) = translate("const unsigned char x = ~0x55;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UCHAR_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0xaa));
}

#[test]
fn test_global_var_init_unary_4() {
    let (tu_result, ec) = translate("const long x = !2;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::SLONG_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0));
}

#[test]
fn test_global_var_init_unary_5() {
    let (tu_result, ec) = translate("const unsigned long x = -1;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::ULONG_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0xffffffff));
}

#[test]
fn test_global_var_init_unary_6() {
    let (tu_result, ec) = translate("const unsigned long x = -1u;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::ULONG_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0xffff));
}

#[test]
fn test_global_var_init_unary_7() {
    let (tu_result, ec) = translate("const int x = -32768;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::INT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(-32768));
}

#[test]
fn test_global_var_init_unary_8() {
    let (tu_result, ec) = translate("const long x = -32768;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::LONG_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(-32768));
}

#[test]
fn test_global_var_init_sum_1() {
    let (tu_result, ec) = translate("const int x = 5 + 8;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::INT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(13));
}

#[test]
fn test_global_var_init_sum_2() {
    let (tu_result, ec) = translate("const unsigned char x = 200 + 57;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UCHAR_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(1));
}

#[test]
fn test_global_var_init_sum_3() {
    let (tu_result, ec) = translate("const int x = 5 + 0x10001;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::INT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(6));
}

#[test]
fn test_global_var_init_sub_1() {
    let (tu_result, ec) = translate("const int x = 10 - 7;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::INT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(3));
}

#[test]
fn test_global_var_init_sub_2() {
    let (tu_result, ec) = translate("const int x = 100 - 200;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::INT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(-100));
}

#[test]
fn test_global_var_init_sub_3() {
    let (tu_result, ec) = translate("const unsigned int x = 10 - 11;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0xffff));
}

#[test]
fn test_global_var_init_mul_1() {
    let (tu_result, ec) = translate("const unsigned int x = 100 * 200;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(20000));
}

#[test]
fn test_global_var_init_mul_2() {
    let (tu_result, ec) = translate("const unsigned int x = 200 / 100;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(2));
}

#[test]
fn test_global_var_init_mul_3() {
    let (tu_result, ec) = translate("const unsigned int x = 50 % 3;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(2));
}

#[test]
fn test_global_var_init_mul_4() {
    let (tu_result, ec) = translate("const unsigned int x = 100 / 0;");
    assert!(tu_result.is_err());
    assert_eq!(ec.get_error_count(), 1);
}

#[test]
fn test_global_var_init_shift_1() {
    let (tu_result, ec) = translate("const unsigned int x = 0x1f0 >> 4;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0x1f));
}

#[test]
fn test_global_var_init_shift_2() {
    let (tu_result, ec) = translate("const unsigned int x = 0x1f0 << 8;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0xf000));
}

#[test]
fn test_global_var_init_cmp_1() {
    let (tu_result, ec) = translate("const unsigned int x = 5 > 8;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0));
}

#[test]
fn test_global_var_init_cmp_2() {
    let (tu_result, ec) = translate("const unsigned int x = 8 >= 8;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(1));
}

#[test]
fn test_global_var_init_cmp_3() {
    let (tu_result, ec) = translate("const unsigned int x = 50 < 100;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(1));
}

#[test]
fn test_global_var_init_cmp_4() {
    let (tu_result, ec) = translate("const unsigned int x = 50 <= 8;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0));
}

#[test]
fn test_global_var_init_cmp_5() {
    let (tu_result, ec) = translate("const unsigned int x = 1000 == 1000;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(1));
}

#[test]
fn test_global_var_init_cmp_6() {
    let (tu_result, ec) = translate("const unsigned int x = 5 != 8;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(1));
}

#[test]
fn test_global_var_init_bitwise_1() {
    let (tu_result, ec) = translate("const unsigned int x = 0xf0 | 0x0f;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0xff));
}

#[test]
fn test_global_var_init_bitwise_2() {
    let (tu_result, ec) = translate("const unsigned int x = 0x10e0 & 0xff;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0xe0));
}

#[test]
fn test_global_var_init_bitwise_3() {
    let (tu_result, ec) = translate("const unsigned int x = 0xff ^ 0x111;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(0x1ee));
}

#[test]
fn test_global_var_init_logical_1() {
    let (tu_result, ec) = translate("const unsigned int x = 5 || 0;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(1));
}

#[test]
fn test_global_var_init_logical_2() {
    let (tu_result, ec) = translate("const unsigned int x = 20 > 8 && 7 < 14;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(1));
}

#[test]
fn test_global_var_init_cond_1() {
    let (tu_result, ec) = translate("const unsigned int x = 0 ? 1 : 2;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(2));
}

#[test]
fn test_global_var_init_cond_2() {
    let (tu_result, ec) = translate("const unsigned int x = 5 ? 1 : 2;");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(1));
}

#[test]
fn test_global_var_init_cond_3() {
    let (tu_result, ec) = translate("const unsigned int x = 5 ? (int*)5 : (char*)6;");
    assert!(tu_result.is_err());
    assert_eq!(ec.get_error_count(), 1);
}

#[test]
fn test_global_var_init_comma_1() {
    let (tu_result, ec) = translate("const unsigned int x = (1,2,(void)5,3);");
    ec.print_issues();
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::UINT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(3));
}

#[test]
fn test_static_assert_1() {
    let (tu_result, ec) = translate("_Static_assert(1, \"ok\");");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
}

#[test]
fn test_static_assert_2() {
    let (tu_result, ec) = translate("_Static_assert(0, \"fail\");");
    assert!(tu_result.is_err());
    assert_eq!(ec.get_error_count(), 1);
}

#[test]
fn test_global_var_init_char_1() {
    let (tu_result, ec) = translate("const int x = 'A';");
    assert!(tu_result.is_ok());
    assert_eq!(ec.get_error_count(), 0);
    let tu = tu_result.unwrap();
    let decl = tu.scope.get("x").unwrap().unwrap_static_var();
    assert_eq!(decl.0.t, ctype::INT_TYPE);
    assert_eq!(decl.0.qualifiers, Qualifiers::CONST);
    assert_eq!(decl.2, &GlobalStorageClass::Default);
    assert_matches!(decl.3.as_ref().unwrap().val, Constant::Int(65));
}
