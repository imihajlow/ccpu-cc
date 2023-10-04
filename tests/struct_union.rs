use crate::build::build;

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_packed_1() {
    build(
        "
          struct Foo {
            char a;
            int b;
            struct {
              int c;
              char d;
              int e;
            } __attribute__((packed)) ;
          } __attribute__((packed)) ;

          _Static_assert(sizeof(struct Foo) == 8, \"sizeof\");
          _Static_assert(_Alignof(struct Foo) == 8, \"alignof\");
          _Static_assert(__builtin_offsetof(struct Foo, b) == 1, \"b\");
          _Static_assert(__builtin_offsetof(struct Foo, c) == 3, \"c\");
          _Static_assert(__builtin_offsetof(struct Foo, d) == 5, \"d\");
          _Static_assert(__builtin_offsetof(struct Foo, e) == 6, \"e\");

          void main(void) {}
    ",
    );
}
