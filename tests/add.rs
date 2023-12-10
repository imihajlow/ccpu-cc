use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_add_int() {
    let (bin, map) = build(
        "
        int fooa, foob, fooc;

        void main(void) {
            fooc = fooa + foob;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0xf0",
            "poke w foob 0x12",
            "u main_exit",
            "check w fooc 0x102",
        ],
    );
}

#[test]
fn test_add_long() {
    let (bin, map) = build(
        "
        long fooa, foob, fooc;

        void main(void) {
            fooc = fooa + foob;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d fooa 0xf0",
            "poke d foob 0x12",
            "u main_exit",
            "check d fooc 0x102",
        ],
    );
    run(
        &bin,
        &map,
        &[
            "u main",
            "poke d fooa 0x02fffff0",
            "poke d foob 0x12",
            "u main_exit",
            "check d fooc 0x3000002",
        ],
    );
}

#[test]
fn test_add_llong() {
    let (bin, map) = build(
        "
        long long fooa, foob, fooc;

        void main(void) {
            fooc = fooa + foob;
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0xf0",
            "poke q foob 0x12",
            "u main_exit",
            "check q fooc 0x102",
        ],
    );
    run(
        &bin,
        &map,
        &[
            "u main",
            "poke q fooa 0x02fffffffffffff0",
            "poke q foob 0x12",
            "u main_exit",
            "check q fooc 0x300000000000002",
        ],
    );
}

#[test]
fn test_add_byte_const_inplace() {
    let (bin, map) = build(
        "
        char fooa, r0, r1, r2, r3;

        char add0(char x) {
            return x + 0;
        }

        char add1(char x) {
            return x + 1;
        }

        char add2(char x) {
            return x + 2;
        }

        char addff(char x) {
            return x + 0xff;
        }

        void main(void) {
            r0 = add0(fooa);
            r1 = add1(fooa);
            r2 = add2(fooa);
            r3 = addff(fooa);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 0xa5",
            "u main_exit",
            "check b r0 0xa5",
            "check b r1 0xa6",
            "check b r2 0xa7",
            "check b r3 0xa4",
        ],
    );
}

#[test]
fn test_add_byte_const() {
    let (bin, map) = build(
        "
        char fooa, r0, r1, r2, r3;

        char bar(char x) { return 0; }

        char add0(char x) {
            char y = x + 0;
            return y + bar(x);
        }

        char add1(char x) {
            char y = x + 1;
            return y + bar(x);
        }

        char add2(char x) {
            char y = x + 2;
            return y + bar(x);
        }

        char addff(char x) {
            char y = x + 0xff;
            return y + bar(x);
        }

        void main(void) {
            r0 = add0(fooa);
            r1 = add1(fooa);
            r2 = add2(fooa);
            r3 = addff(fooa);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke b fooa 0xa5",
            "u main_exit",
            "check b r0 0xa5",
            "check b r1 0xa6",
            "check b r2 0xa7",
            "check b r3 0xa4",
        ],
    );
}

#[test]
fn test_add_word_const_inplace() {
    let (bin, map) = build(
        "
        int fooa, r0, r1, r2, r3, r4, r5, r6, r7, r8;

        int add0(int x) {
            return x + 0;
        }

        int add1(int x) {
            return x + 1;
        }

        int add2(int x) {
            return x + 2;
        }

        int addff(int x) {
            return x + 0xff;
        }

        int add100(int x) {
            return x + 0x100;
        }

        int add200(int x) {
            return x + 0x200;
        }

        int addff00(int x) {
            return x + 0xff00;
        }

        int add1201(int x) {
            return x + 0x1201;
        }

        int add1202(int x) {
            return x + 0x1202;
        }

        void main(void) {
            r0 = add0(fooa);
            r1 = add1(fooa);
            r2 = add2(fooa);
            r3 = addff(fooa);
            r4 = add100(fooa);
            r5 = add200(fooa);
            r6 = addff00(fooa);
            r7 = add1201(fooa);
            r8 = add1202(fooa);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x12fe",
            "u main_exit",
            "check w r0 0x12fe",
            "check w r1 0x12ff",
            "check w r2 0x1300",
            "check w r3 0x13fd",
            "check w r4 0x13fe",
            "check w r5 0x14fe",
            "check w r6 0x11fe",
            "check w r7 0x24ff",
            "check w r8 0x2500",
        ],
    );
}

#[test]
fn test_add_word_const() {
    let (bin, map) = build(
        "
        int fooa, r0, r1, r2, r3, r4, r5, r6, r7, r8;

        int zero(int x) { return 0; }

        int add0(int x) {
            int y = x + 0;
            return y + zero(x);
        }

        int add1(int x) {
            int y = x + 1;
            return y + zero(x);
        }

        int add2(int x) {
            int y = x + 2;
            return y + zero(x);
        }

        int addff(int x) {
            int y = x + 0xff;
            return y + zero(x);
        }

        int add100(int x) {
            int y = x + 0x100;
            return y + zero(x);
        }

        int add200(int x) {
            int y = x + 0x200;
            return y + zero(x);
        }

        int addff00(int x) {
            int y = x + 0xff00;
            return y + zero(x);
        }

        int add1201(int x) {
            int y = x + 0x1201;
            return y + zero(x);
        }

        int add1202(int x) {
            int y = x + 0x1202;
            return y + zero(x);
        }

        void main(void) {
            r0 = add0(fooa);
            r1 = add1(fooa);
            r2 = add2(fooa);
            r3 = addff(fooa);
            r4 = add100(fooa);
            r5 = add200(fooa);
            r6 = addff00(fooa);
            r7 = add1201(fooa);
            r8 = add1202(fooa);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main",
            "poke w fooa 0x12fe",
            "u main_exit",
            "check w r0 0x12fe",
            "check w r1 0x12ff",
            "check w r2 0x1300",
            "check w r3 0x13fd",
            "check w r4 0x13fe",
            "check w r5 0x14fe",
            "check w r6 0x11fe",
            "check w r7 0x24ff",
            "check w r8 0x2500",
        ],
    );
}

#[test]
fn test_add_long_const() {
    let values = [0 as u8, 1, 20, 0xff];
    for b1 in &values {
        for b2 in &values {
            for b3 in &values {
                for b4 in &values {
                    let le = [*b1, *b2, *b3, *b4];
                    let c = u32::from_le_bytes(le);
                    let (bin, map) = build(&format!(
                        "
                            long fooa, r0, r1, r2;

                            long zero(long x) {{ return 0; }}

                            long add(long x) {{
                                long y = x + {0};
                                return y + zero(x);
                            }}

                            long add_inplace(long x) {{
                                return x + {0};
                            }}

                            void main(void) {{
                                r0 = add_inplace(fooa);
                                r1 = add(fooa);
                                r2 = fooa + {0};
                            }}
                        ",
                        c
                    ));

                    let input: u32 = 0x12345678;
                    let result = input.wrapping_add(c);

                    run(
                        &bin,
                        &map,
                        &[
                            "u main",
                            &format!("poke d fooa {}", input),
                            "u main_exit",
                            &format!("check d r0 {}", result),
                            &format!("check d r1 {}", result),
                            &format!("check d r2 {}", result),
                        ],
                    );
                }
            }
        }
    }
}
