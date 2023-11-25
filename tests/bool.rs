use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_bool_1() {
    let (bin, map) = build(
        "
        int fooa, foob;

        void main(void) {
            fooa = !foob;
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
            "check w fooa 0x0000",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w foob 0x1200",
            "u main_exit",
            "check w fooa 0x0000",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w foob 0x0000",
            "u main_exit",
            "check w fooa 0x0001",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w foob 0x0034",
            "u main_exit",
            "check w fooa 0x0000",
        ],
    );
}
