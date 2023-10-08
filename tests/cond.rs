use std::path::PathBuf;

use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_if_eq_byte() {
    let (bin, map) = build(
        "
        char fooa, foob;
        int fooc;

        static int bar(char a, char b) {
            if (a == b) {
                return 0xaa;
            } else {
                return 0x55;
            }
        }

        void main(void) {
            fooc = bar(fooa, foob);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 10",
            "poke b foob 10",
            "u main_exit",
            "check w fooc 0x00aa",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 10",
            "poke b foob 20",
            "u main_exit",
            "check w fooc 0x0055",
        ],
    );
}

#[test]
fn test_if_ne_byte() {
    let (bin, map) = build(
        "
        char fooa, foob;
        int fooc;

        static int bar(char a, char b) {
            if (a != b) {
                return 0x55;
            } else {
                return 0xaa;
            }
        }

        void main(void) {
            fooc = bar(fooa, foob);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 10",
            "poke b foob 10",
            "u main_exit",
            "check w fooc 0x00aa",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 10",
            "poke b foob 20",
            "u main_exit",
            "check w fooc 0x0055",
        ],
    );
}

#[test]
fn test_if_eq_qword() {
    let (bin, map) = build(
        "
        long long fooa, foob;
        int fooc;

        static int bar(long long a, long long b) {
            if (a == b) {
                return 0xaa;
            } else {
                return 0x55;
            }
        }

        void main(void) {
            fooc = bar(fooa, foob);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0x1020304050607080",
            "poke q foob 0x1020304050607080",
            "u main_exit",
            "check w fooc 0x00aa",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0x1020304050607080",
            "poke q foob 0x2020304050607080",
            "u main_exit",
            "check w fooc 0x0055",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0x1020304050607080",
            "poke q foob 0x1000304050607080",
            "u main_exit",
            "check w fooc 0x0055",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0x1020304050607080",
            "poke q foob 0x1020004050607080",
            "u main_exit",
            "check w fooc 0x0055",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0x1020304050607080",
            "poke q foob 0x1020300050607080",
            "u main_exit",
            "check w fooc 0x0055",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0x1020304050607080",
            "poke q foob 0x1020304000607080",
            "u main_exit",
            "check w fooc 0x0055",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0x1020304050607080",
            "poke q foob 0x1020304050007080",
            "u main_exit",
            "check w fooc 0x0055",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0x1020304050607080",
            "poke q foob 0x1020304050600080",
            "u main_exit",
            "check w fooc 0x0055",
        ],
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0x1020304050607080",
            "poke q foob 0x1020304050607000",
            "u main_exit",
            "check w fooc 0x0055",
        ],
    );
}

fn build_if_cmp_common(t: &str, op: &str) -> (PathBuf, PathBuf) {
    build(&format!(
        "
        {} fooa, foob;
        int fooc;

        static int bar({} a, {} b) {{
            if (a {} b) {{
                return 0xaa;
            }} else {{
                return 0x55;
            }}
        }}

        void main(void) {{
            fooc = bar(fooa, foob);
        }}
    ",
        t, t, t, op
    ))
}

#[test]
fn test_if_cmp_lt_signed_dword() {
    let (bin, map) = build_if_cmp_common("long", "<");

    let t: u16 = 0xaa;
    let f: u16 = 0x55;

    let tests = [
        (0x01020304 as u32, 0x01020303 as u32, f),
        (0x01020304, 0x01020304, f),
        (0x01020304, 0x010203ff, t),
        (0x81020304, 0x02020303, t),
        (0x7fffffff, 0x80000022, f),
    ];
    for (fooa, foob, fooc) in tests {
        run(
            &bin,
            &map,
            &[
                "u main",
                &format!("poke d fooa 0x{:X}", fooa),
                &format!("poke d foob 0x{:X}", foob),
                "u main_exit",
                &format!("check w fooc 0x{:X}", fooc),
            ],
        );
    }
}

#[test]
fn test_if_cmp_le_signed_dword() {
    let (bin, map) = build_if_cmp_common("long", "<=");

    let t: u16 = 0xaa;
    let f: u16 = 0x55;

    let tests = [
        (0x01020304 as u32, 0x01020303 as u32, f),
        (0x01020304, 0x01020304, t),
        (0x01020304, 0x010203ff, t),
        (0x81020304, 0x02020303, t),
        (0x7fffffff, 0x80000022, f),
    ];
    for (fooa, foob, fooc) in tests {
        run(
            &bin,
            &map,
            &[
                "u main",
                &format!("poke d fooa 0x{:X}", fooa),
                &format!("poke d foob 0x{:X}", foob),
                "u main_exit",
                &format!("check w fooc 0x{:X}", fooc),
            ],
        );
    }
}

#[test]
fn test_if_cmp_gt_signed_dword() {
    let (bin, map) = build_if_cmp_common("long", ">");

    let t: u16 = 0xaa;
    let f: u16 = 0x55;

    let tests = [
        (0x01020304 as u32, 0x01020303 as u32, t),
        (0x01020304, 0x01020304, f),
        (0x01020304, 0x010203ff, f),
        (0x81020304, 0x02020303, f),
        (0x7fffffff, 0x80000022, t),
    ];
    for (fooa, foob, fooc) in tests {
        run(
            &bin,
            &map,
            &[
                "u main",
                &format!("poke d fooa 0x{:X}", fooa),
                &format!("poke d foob 0x{:X}", foob),
                "u main_exit",
                &format!("check w fooc 0x{:X}", fooc),
            ],
        );
    }
}

#[test]
fn test_if_cmp_ge_signed_dword() {
    let (bin, map) = build_if_cmp_common("long", ">=");

    let t: u16 = 0xaa;
    let f: u16 = 0x55;

    let tests = [
        (0x01020304 as u32, 0x01020303 as u32, t),
        (0x01020304, 0x01020304, t),
        (0x01020304, 0x010203ff, f),
        (0x81020304, 0x02020303, f),
        (0x7fffffff, 0x80000022, t),
    ];
    for (fooa, foob, fooc) in tests {
        run(
            &bin,
            &map,
            &[
                "u main",
                &format!("poke d fooa 0x{:X}", fooa),
                &format!("poke d foob 0x{:X}", foob),
                "u main_exit",
                &format!("check w fooc 0x{:X}", fooc),
            ],
        );
    }
}
