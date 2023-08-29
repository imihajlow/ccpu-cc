use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_upcast_signed_byte() {
    let (bin, map) = build(
        "
        signed char fooa;
        int foob;
        long fooc;
        long long food;

        void main(void) {
            foob = fooa;
            fooc = fooa;
            food = fooa;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 0x12",
            "poke w foob 0xa5a5",
            "poke d fooc 0xa5a5a5a5",
            "poke q food 0xa5a5a5a5a5a5a5a5",
            "u main_exit",
            "check w foob 0x0012",
            "check d fooc 0x00000012",
            "check q food 0x0000000000000012",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 0xb2",
            "poke w foob 0xa5a5",
            "poke d fooc 0xa5a5a5a5",
            "poke q food 0xa5a5a5a5a5a5a5a5",
            "u main_exit",
            "check w foob 0xffb2",
            "check d fooc 0xffffffb2",
            "check q food 0xffffffffffffffb2",
        ],
    );
}

#[test]
fn test_upcast_signed_word() {
    let (bin, map) = build(
        "
        int foob;
        long fooc;
        long long food;

        void main(void) {
            fooc = foob;
            food = foob;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w foob 0x1234",
            "poke d fooc 0xa5a5a5a5",
            "poke q food 0xa5a5a5a5a5a5a5a5",
            "u main_exit",
            "check d fooc 0x00001234",
            "check q food 0x0000000000001234",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w foob 0x8234",
            "poke d fooc 0xa5a5a5a5",
            "poke q food 0xa5a5a5a5a5a5a5a5",
            "u main_exit",
            "check d fooc 0xffff8234",
            "check q food 0xffffffffffff8234",
        ],
    );
}

#[test]
fn test_upcast_signed_dword() {
    let (bin, map) = build(
        "
        long fooc;
        long long food;

        void main(void) {
            food = fooc;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d fooc 0x12345678",
            "poke q food 0xa5a5a5a5a5a5a5a5",
            "u main_exit",
            "check q food 0x0000000012345678",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d fooc 0x82345678",
            "poke q food 0xa5a5a5a5a5a5a5a5",
            "u main_exit",
            "check q food 0xffffffff82345678",
        ],
    );
}

#[test]
fn test_upcast_unsigned_word() {
    let (bin, map) = build(
        "
        unsigned int foob;
        long fooc;
        long long food;

        void main(void) {
            fooc = foob;
            food = foob;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w foob 0x1234",
            "poke d fooc 0xa5a5a5a5",
            "poke q food 0xa5a5a5a5a5a5a5a5",
            "u main_exit",
            "check d fooc 0x00001234",
            "check q food 0x0000000000001234",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w foob 0x8234",
            "poke d fooc 0xa5a5a5a5",
            "poke q food 0xa5a5a5a5a5a5a5a5",
            "u main_exit",
            "check d fooc 0x00008234",
            "check q food 0x00000000000000008234",
        ],
    );
}
