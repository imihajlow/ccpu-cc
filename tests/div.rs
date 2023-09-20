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
fn test_udiv_dword() {
    let (bin, map) = build(
        "
        unsigned long fooa, foob, fooc, food;

        void main(void) {
            fooc = fooa / foob;
            food = fooa % foob;
        }
    ",
    );

    let tests = [
        (30 as u32, 2 as u32),
        (100, 20),
        (50, 50),
        (111, 11),
        (15000, 75),
        (30000, 1111),
        (52000, 123),
        (1000000, 10),
        (10, 1000000),
        (3123456789, 7654321),
        (0x102031, 0x70),
        (0x102031, 0x1070),
        (0x102031, 0x100970),
        (0xbb102031, 0x70),
        (0xbb102031, 0x1070),
        (0xbb102031, 0x100970),
        (0xbb102031, 0x12100970),
    ];

    for (a, b) in &tests {
        let c = a.wrapping_div(*b);
        let d = a.wrapping_rem(*b);
        run(
            &bin,
            &map,
            &[
                "u main",
                &format!("poke d fooa {}", *a as u32),
                &format!("poke d foob {}", *b as u32),
                "u main_exit",
                &format!("check d fooc {}", c as u32),
                &format!("check d food {}", d as u32),
            ],
        );
    }
}

#[test]
fn test_div_dword() {
    let (bin, map) = build(
        "
        long fooa, foob, fooc, food;

        void main(void) {
            fooc = fooa / foob;
            food = fooa % foob;
        }
    ",
    );

    let tests = [
        (30 as i32, 2 as i32),
        (100, 20),
        (50, 50),
        (111, 11),
        (-111, 11),
        (111, -11),
        (-111, -11),
        (15000, 75),
        (30000, 1111),
        (52000, 123),
        (1000000, 10),
        (10, 1000000),
        (1123456789, 7654321),
        (0x102031, 0x70),
        (0x102031, 0x1070),
        (0x102031, 0x100970),
        (0x7b102031, 0x70),
        (0x7b102031, 0x1070),
        (0x7b102031, 0x100970),
        (0x7b102031, 0x12100970),
        (-0x7b102031, 0x12100970),
        (0x7b102031, -0x12100970),
        (-0x7b102031, -0x12100970),
    ];

    for (a, b) in &tests {
        let c = a.wrapping_div(*b);
        let d = a.wrapping_rem(*b);
        run(
            &bin,
            &map,
            &[
                "u main",
                &format!("poke d fooa {}", *a as u32),
                &format!("poke d foob {}", *b as u32),
                "u main_exit",
                &format!("check d fooc {}", c as u32),
                &format!("check d food {}", d as u32),
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
