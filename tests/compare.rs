use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_compare_tail_unsigned_16() {
    let (bin, map) = build(
        "
        unsigned int fooa, foob, fooc;

        void main(void) {
            if (fooa > foob)
                fooc = 0x5678;
            else
                fooc = 0xffff;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x1234",
            "poke w foob 0x1235",
            "u main_exit",
            "check w fooc 0xffff",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0xffff",
            "poke w foob 0x1234",
            "u main_exit",
            "check w fooc 0x5678",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x1234",
            "poke w foob 0xffff",
            "u main_exit",
            "check w fooc 0xffff",
        ],
    );
}

#[test]
fn test_compare_unsigned_16() {
    let (bin, map) = build(
        "
        unsigned int fooa, foob, fooc;

        void main(void) {
            fooc = fooa > foob;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x1234",
            "poke w foob 0x1235",
            "u main_exit",
            "check w fooc 0x0000",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0xffff",
            "poke w foob 0x1234",
            "u main_exit",
            "check w fooc 0x0001",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x1234",
            "poke w foob 0xffff",
            "u main_exit",
            "check w fooc 0x0000",
        ],
    );
}

#[test]
fn test_compare_tail_signed_16() {
    let (bin, map) = build(
        "
        int fooa, foob, fooc;

        void main(void) {
            if (fooa > foob)
                fooc = 0x5678;
            else
                fooc = 0xffff;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x1234",
            "poke w foob 0x1235",
            "u main_exit",
            "check w fooc 0xffff",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0xffff",
            "poke w foob 0x1234",
            "u main_exit",
            "check w fooc 0xffff",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x1234",
            "poke w foob 0xffff",
            "u main_exit",
            "check w fooc 0x5678",
        ],
    );
}

#[test]
fn test_compare_signed_16() {
    let (bin, map) = build(
        "
        int fooa, foob, fooc;

        void main(void) {
            fooc = fooa > foob;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x1234",
            "poke w foob 0x1235",
            "u main_exit",
            "check w fooc 0x0000",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0xffff",
            "poke w foob 0x1234",
            "u main_exit",
            "check w fooc 0x0000",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x1234",
            "poke w foob 0xffff",
            "u main_exit",
            "check w fooc 0x0001",
        ],
    );
}

#[test]
fn test_compare_eq_8() {
    let (bin, map) = build(
        "
        char fooa, foob, fooc;

        static char cmp(char a, char b) {
            return a == b;
        }

        void main(void) {
            fooc = cmp(fooa, foob);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 0x00",
            "poke b foob 0x00",
            "u main_exit",
            "check w fooc 0x01",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 0xff",
            "poke b foob 0x34",
            "u main_exit",
            "check b fooc 0x00",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 0x00",
            "poke b foob 0x01",
            "u main_exit",
            "check b fooc 0x00",
        ],
    );
}

#[test]
fn test_compare_eq_8_and() {
    let (bin, map) = build(
        "
        char fooa, foob, fooc;

        static char cmp(char f, char a, char b) {
            if (f && a == b)
                return 1;
            else
                return 0;
        }

        void main(void) {
            fooc = cmp(1, fooa, foob);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 0x00",
            "poke b foob 0x00",
            "u main_exit",
            "check w fooc 0x01",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 0xff",
            "poke b foob 0x34",
            "u main_exit",
            "check b fooc 0x00",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 0x00",
            "poke b foob 0x01",
            "u main_exit",
            "check b fooc 0x00",
        ],
    );
}
