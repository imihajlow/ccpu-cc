use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_object_1() {
    let (bin, map) = build(
        "
        struct foo { int a, b; };

        struct foo fooa;
        int foob;

        void main(void) {
            foob = fooa.a + fooa.b;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d fooa 0x01000002",
            "u main_exit",
            "check w foob 0x0102",
        ],
    );
}

#[test]
fn test_object_2() {
    let (bin, map) = build(
        "
        struct foo { int a, b; };

        int foob;

        void main(void) {
            struct foo fooa;
            foob = fooa.a + fooa.b;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d __cc_main_frame 0x01000002",
            "u main_exit",
            "check w foob 0x0102",
        ],
    );
}

#[test]
fn test_object_3() {
    let (bin, map) = build(
        "
        struct foo { int a, b; };

        int foob;

        void main(void) {
            static struct foo fooa;
            foob = fooa.a + fooa.b;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d __static_main_0_fooa 0x01000002",
            "u main_exit",
            "check w foob 0x0102",
        ],
    );
}

#[test]
fn test_object_4() {
    let (bin, map) = build(
        "
        struct foo { int a[4]; };

        struct foo fooa;
        int foob;

        void main(void) {
            foob = fooa.a[1];
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0x123456789abcdef0",
            "u main_exit",
            "check w foob 0x9abc",
        ],
    );
}

#[test]
fn test_object_5() {
    let (bin, map) = build(
        "
        struct foo { int a[4]; };

        struct foo fooa;
        int foob;

        void main(void) {
            fooa.a[1] = foob;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0x123456789abcdef0",
            "poke w foob 0x5a77",
            "u main_exit",
            "check q fooa 0x123456785a77def0",
        ],
    );
}

#[test]
fn test_object_6() {
    let (bin, map) = build(
        "

        int fooa[4];
        int foob;

        void main(void) {
            fooa[1] = foob;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0x123456789abcdef0",
            "poke w foob 0x5a77",
            "u main_exit",
            "check q fooa 0x123456785a77def0",
        ],
    );
}
