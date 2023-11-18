use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_bswap_16() {
    let (bin, map) = build(
        "
        int fooa, foob;

        void main(void) {
            fooa = __builtin_bswap16(foob);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w foob 0x1234",
            "u main_exit",
            "check w fooa 0x3412",
        ],
    );
}

#[test]
fn test_bswap_32() {
    let (bin, map) = build(
        "
        long int fooa, foob;

        void main(void) {
            fooa = __builtin_bswap32(foob);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d foob 0x12345678",
            "u main_exit",
            "check d fooa 0x78563412",
        ],
    );
}
