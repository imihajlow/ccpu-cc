
use crate::build::{run, build};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_add_int() {
    let (bin, map) = build("
        int fooa, foob, fooc;

        void main(void) {
            fooc = fooa + foob;
        }
    ");

    run(&bin, &map, &["u main", "poke w fooa 0xf0", "poke w foob 0x12", "u main_exit", "check w fooc 0x102"]);
}

#[test]
fn test_add_long() {
    let (bin, map) = build("
        long fooa, foob, fooc;

        void main(void) {
            fooc = fooa + foob;
        }
    ");

    run(&bin, &map, &["u main", "poke d fooa 0xf0", "poke d foob 0x12", "u main_exit", "check d fooc 0x102"]);
    run(&bin, &map, &["u main", "poke d fooa 0x02fffff0", "poke d foob 0x12", "u main_exit", "check d fooc 0x3000002"]);
}


#[test]
fn test_add_llong() {
    let (bin, map) = build("
        long long fooa, foob, fooc;

        void main(void) {
            fooc = fooa + foob;
        }
    ");

    run(&bin, &map, &["u main", "poke q fooa 0xf0", "poke q foob 0x12", "u main_exit", "check q fooc 0x102"]);
    run(&bin, &map, &["u main", "poke q fooa 0x02fffffffffffff0", "poke q foob 0x12", "u main_exit", "check q fooc 0x300000000000002"]);
}
