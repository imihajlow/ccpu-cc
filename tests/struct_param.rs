use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_struct_param_1() {
    let (bin, map) = build(
        "
        struct Foo { int a, b; };

        int foob;

        static int bar(struct Foo foo) {
          return foo.a + foo.b;
        }

        void main(void) {
              struct Foo foo;
              foo.a = 1;
              foo.b = 3210;
              foob = bar(foo);
        }
    ",
    );

    run(&bin, &map, &["u main", "u main_exit", "check w foob 3211"]);
}
