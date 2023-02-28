use super::util::*;

#[test]
fn test_struct_1() {
    let (tu, ec) = compile("struct X { int x; }; void foo(void) { struct X x; struct X *p = &x; }");
    ec.print_issues();
    assert_eq!(ec.get_warning_count(), 0);
    let body = get_first_body(&tu);
    assert_eq!(body.len(), 1);
}
