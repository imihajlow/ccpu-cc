use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_div_byte() {
    let (bin, map) = build(
        "
        signed char fooa, foob, fooc;

        void main(void) {
            fooc = fooa / foob;
        }
    ",
    );

    let tests = [
        (30 as i8, 2 as i8),
        (-100, 20),
        (50, 50),
        (111, 11),
        (100, -20),
        (-100, -20),
    ];

    for (a, b) in &tests {
        let c = a.wrapping_div(*b);
        run(
            &bin,
            &map,
            &[
                "u main",
                &format!("poke b fooa {}", *a as u8),
                &format!("poke b foob {}", *b as u8),
                "u main_exit",
                &format!("check b fooc {}", c as u8),
            ],
        );
    }
}

#[test]
fn test_div_word() {
    let (bin, map) = build(
        "
        signed int fooa, foob, fooc;

        void main(void) {
            fooc = fooa / foob;
        }
    ",
    );

    let tests = [
        (30 as i16, 2 as i16),
        (-100, 20),
        (50, 50),
        (111, 11),
        (100, -20),
        (-100, -20),
        (15000, 75),
        (-15000, 75),
        (15000, -75),
        (-15000, -75),
        (30000, 1111),
    ];

    for (a, b) in &tests {
        let c = a.wrapping_div(*b);
        run(
            &bin,
            &map,
            &[
                "u main",
                &format!("poke w fooa {}", *a as u16),
                &format!("poke w foob {}", *b as u16),
                "u main_exit",
                &format!("check w fooc {}", c as u16),
            ],
        );
    }
}

#[test]
fn test_udiv_word() {
    let (bin, map) = build(
        "
        unsigned int fooa, foob, fooc;

        void main(void) {
            fooc = fooa / foob;
        }
    ",
    );

    let tests = [
        (30 as u16, 2 as u16),
        (100, 20),
        (50, 50),
        (111, 11),
        (15000, 75),
        (30000, 1111),
        (52000, 123),
    ];

    for (a, b) in &tests {
        let c = a.wrapping_div(*b);
        run(
            &bin,
            &map,
            &[
                "u main",
                &format!("poke w fooa {}", *a as u16),
                &format!("poke w foob {}", *b as u16),
                "u main_exit",
                &format!("check w fooc {}", c as u16),
            ],
        );
    }
}

#[test]
fn test_rem_byte() {
    let (bin, map) = build(
        "
        signed char fooa, foob, fooc;

        void main(void) {
            fooc = fooa % foob;
        }
    ",
    );

    let tests = [
        (30 as i8, 2 as i8),
        (-101, 20),
        (50, 50),
        (111, 11),
        (101, -20),
        (-101, -20),
    ];

    for (a, b) in &tests {
        let q = a.wrapping_div(*b);
        let c = a.wrapping_sub(q.wrapping_mul(*b));
        run(
            &bin,
            &map,
            &[
                "u main",
                &format!("poke b fooa {}", *a as u8),
                &format!("poke b foob {}", *b as u8),
                "u main_exit",
                &format!("check b fooc {}", c as u8),
            ],
        );
    }
}

#[test]
fn test_rem_word() {
    let (bin, map) = build(
        "
        signed int fooa, foob, fooc;

        void main(void) {
            fooc = fooa % foob;
        }
    ",
    );

    let tests = [
        (30 as i16, 2 as i16),
        (-101, 20),
        (50, 50),
        (111, 11),
        (101, -20),
        (-101, -20),
        (15022, 75),
        (-15010, 75),
        (15101, -75),
        (-15034, -75),
        (30000, 1111),
    ];

    for (a, b) in &tests {
        let q = a.wrapping_div(*b);
        let c = a.wrapping_sub(q.wrapping_mul(*b));
        run(
            &bin,
            &map,
            &[
                "u main",
                &format!("poke w fooa {}", *a as u16),
                &format!("poke w foob {}", *b as u16),
                "u main_exit",
                &format!("check w fooc {}", c as u16),
            ],
        );
    }
}
