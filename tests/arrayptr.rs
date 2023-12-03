use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_array_ptr() {
    let (bin, map) = build(
        "
        typedef int T[4];

        int foo(T *a) {
            return (*a)[2];
        }

        int bar[4] = {10,20,30,40};
        int baz;

        void main(void) {
            baz = foo(&bar);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main_exit",
            "check w baz 30",
        ],
    );
}
