use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_mul_byte() {
    let (bin, map) = build(
        "
        signed char fooa, foob, fooc;

        void main(void) {
            fooc = fooa * foob;
        }
    ",
    );

    let tests = [(3 as i8, 4 as i8), (-1, 20), (50, 50)];

    for (a, b) in &tests {
        let c = a.wrapping_mul(*b);
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
fn test_mul_word() {
    let (bin, map) = build(
        "
        signed int fooa, foob, fooc;

        void main(void) {
            fooc = fooa * foob;
        }
    ",
    );

    let tests = [
        (3 as i16, 4 as i16),
        (-1, 20),
        (50, 50),
        (0x101, 23),
        (-1, -1),
        (1000, 1000),
    ];

    for (a, b) in &tests {
        let c = a.wrapping_mul(*b);
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
fn test_mul_dword() {
    let (bin, map) = build(
        "
        signed long fooa, foob, fooc;

        void main(void) {
            fooc = fooa * foob;
        }
    ",
    );

    let tests = [
        (3 as i32, 4 as i32),
        (-1, 20),
        (50, 50),
        (0x101, 23),
        (-1, -1),
        (1000, 1000),
        (70000, 2),
        (93000, 239),
        (93000, -239),
        (-93000, -239),
    ];

    for (a, b) in &tests {
        let c = a.wrapping_mul(*b);
        run(
            &bin,
            &map,
            &[
                "u main",
                &format!("poke d fooa {}", *a as u32),
                &format!("poke d foob {}", *b as u32),
                "u main_exit",
                &format!("check d fooc {}", c as u32),
            ],
        );
    }
}
