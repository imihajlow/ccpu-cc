use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_struct_init_1() {
    let (bin, map) = build(
        "
        struct Foo {
            int a; char b; long c;
        };

        struct Foo foo = { 0x1234, 0x55, 0x6789abcd };

        int fooa;
        char foob;
        long fooc;

        void main(void) {
            fooa = foo.a;
            foob = foo.b;
            fooc = foo.c;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main_exit",
            "check w fooa 0x1234",
            "check b foob 0x55",
            "check d food 0x6789abcd",
        ],
    );
}

#[test]
fn test_struct_init_2() {
    let (bin, map) = build(
        "
        struct Foo {
            int a; char b; long c;
        };

        struct Foo foo = { .b = 0x55, .c = 0x6789abcd, .a = 0x1234 };

        int fooa;
        char foob;
        long fooc;

        void main(void) {
            fooa = foo.a;
            foob = foo.b;
            fooc = foo.c;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main_exit",
            "check w fooa 0x1234",
            "check b foob 0x55",
            "check d food 0x6789abcd",
        ],
    );
}

#[test]
fn test_struct_init_3() {
    let (bin, map) = build(
        "
        struct Foo {
            int a; struct { char b; long c; };
        };

        struct Foo foo = { .b = 0x55, .c = 0x6789abcd, .a = 0x1234 };

        int fooa;
        char foob;
        long fooc;

        void main(void) {
            fooa = foo.a;
            foob = foo.b;
            fooc = foo.c;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main_exit",
            "check w fooa 0x1234",
            "check b foob 0x55",
            "check d food 0x6789abcd",
        ],
    );
}

#[test]
fn test_struct_init_4() {
    let (bin, map) = build(
        "
        struct Foo {
            int a; struct { char b; long c; };
        };

        struct Foo foo = { .a = 0x1234, .b = 0x55, 0x6789abcd };

        int fooa;
        char foob;
        long fooc;

        void main(void) {
            fooa = foo.a;
            foob = foo.b;
            fooc = foo.c;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main_exit",
            "check w fooa 0x1234",
            "check b foob 0x55",
            "check d food 0x6789abcd",
        ],
    );
}
