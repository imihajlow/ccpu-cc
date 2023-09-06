use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_switch_long() {
    let (bin, map) = build(
        "
        int foo(long c) {
            switch (c) {
            case 0x00011020:
                return 5;
            case 0x00021020:
                return 6;
            case 0x00031021:
                return 7;
            case 0x00041021:
                return 8;
            case 0x00051122:
                return 9;
            default:
                return 10;
            }
        }
        int foo1, foo2, foo3, foo4, foo5, foo6, foo7, foo8;
        void main(void) {
            foo1 = foo(0x00011020);
            foo2 = foo(0x00021020);
            foo3 = foo(0x00031021);
            foo4 = foo(0x00041021);
            foo5 = foo(0x00051122);
            foo6 = foo(0x00051022);
            foo7 = foo(0x00051121);
            foo8 = foo(0x01011020);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main_exit",
            "check w foo1 5",
            "check w foo2 6",
            "check w foo3 7",
            "check w foo4 8",
            "check w foo5 9",
            "check w foo6 10",
            "check w foo7 10",
            "check w foo8 10",
        ],
    );
}
