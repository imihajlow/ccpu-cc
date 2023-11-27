use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_neg_16() {
    let (bin, map) = build(
        "
        int fooa, foob;

        void main(void) {
            foob = -fooa;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x0001",
            "u main_exit",
            "check w foob 0xffff",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x0000",
            "u main_exit",
            "check w foob 0x0000",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x0100",
            "u main_exit",
            "check w foob 0xFF00",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x0101",
            "u main_exit",
            "check w foob 0xFEFF",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x80FF",
            "u main_exit",
            "check w foob 0x7F01",
        ],
    );
}

#[test]
fn test_neg_32() {
    let (bin, map) = build(
        "
        long fooa, foob;

        void main(void) {
            foob = -fooa;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d fooa 0x00000001",
            "u main_exit",
            "check d foob 0xffffffff",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d fooa 0x00000000",
            "u main_exit",
            "check d foob 0x00000000",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d fooa 0x00100000",
            "u main_exit",
            "check d foob 0xfff00000",
        ],
    );
}

#[test]
fn test_neg_32_reg() {
    let (bin, map) = build(
        "
        long fooa, foob;

        long baz(long x) { return -x; }

        void main(void) {
            foob = baz(fooa);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d fooa 0x00000001",
            "u main_exit",
            "check d foob 0xffffffff",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d fooa 0x00000000",
            "u main_exit",
            "check d foob 0x00000000",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d fooa 0x00100000",
            "u main_exit",
            "check d foob 0xfff00000",
        ],
    );
}
