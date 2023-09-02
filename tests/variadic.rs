use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_va_1() {
    let (bin, map) = build(
        "
        typedef __builtin_va_list va_list;
        #define va_start(v,l)   do { (v) = __builtin_va_start((l)); } while (0)
        #define va_end(x)
        #define va_arg(v, t)    ((v) = __builtin_va_increment((v)), __builtin_va_arg((v), t))
        #define __va_copy(d,s)  __builtin_va_copy((d),(s))

        int fooa, foob, fooc;

        int sum_int(int count, ...) {
            va_list va;
            va_start(va, count);
            int sum = 0;
            for (int i = 0; i != count; ++i) {
                sum += va_arg(va, int);
            }
            va_end(va);
            return sum;
        }

        void main(void) {
            fooa = sum_int(0);
            foob = sum_int(3, 1, 10, 100);
            fooc = sum_int(3, 0x10001, 0x100, 0x1000);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main_exit",
            "check w fooa 0",
            "check w foob 111",
            "check w fooc 0x1101",
            "check b 0xfc00 0",
            "check b 0xfc01 1",
        ],
    );
}

#[test]
fn test_va_2() {
    let (bin, map) = build(
        "
        typedef __builtin_va_list va_list;
        #define va_start(v,l)   do { (v) = __builtin_va_start((l)); } while (0)
        #define va_end(x)
        #define va_arg(v, t)    ((v) = __builtin_va_increment((v)), __builtin_va_arg((v), t))
        #define __va_copy(d,s)  __builtin_va_copy((d),(s))

        long fooa, foob, fooc;

        long bar(long a, long b) {
            return a + b;
        }

        long baz(int _, ...) {
            va_list va;
            va_start(va, _);
            int sum = 0;
            long a = va_arg(va, long);
            long b = va_arg(va, long);
            long r = bar(a, b);
            va_end(va);
            return r;
        }

        void main(void) {
            fooa = baz(0, 1, 2);
            foob = baz(0, 0x00010002L, 0x00101010L);
            fooc = baz(0, 0xaaaa12345678ULL, 0xbbbb87654321ULL);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main_exit",
            "check d fooa 3",
            "check d foob 0x00111012",
            "check d fooc 0x99999999",
            "check b 0xfc00 0",
            "check b 0xfc01 1",
        ],
    );
}

#[test]
fn test_va_3() {
    let (bin, map) = build(
        "
        typedef __builtin_va_list va_list;
        #define va_start(v,l)   do { (v) = __builtin_va_start((l)); } while (0)
        #define va_end(x)
        #define va_arg(v, t)    ((v) = __builtin_va_increment((v)), __builtin_va_arg((v), t))
        #define __va_copy(d,s)  __builtin_va_copy((d),(s))

        int fooa, foob, fooc;

        int vsum_int(int count, va_list va) {
            int sum = 0;
            for (int i = 0; i != count; ++i) {
                sum += va_arg(va, int);
            }
            va_end(va);
            return sum;
        }

        int sum_int(int count, ...) {
            va_list va;
            va_start(va, count);
            return vsum_int(count, va);
        }

        void main(void) {
            fooa = sum_int(0);
            foob = sum_int(3, 1, 10, 100);
            fooc = sum_int(3, 0x10001, 0x100, 0x1000);
        }
    ",
    );

    run(
        &bin,
        &map,
        &[
            "u main_exit",
            "check w fooa 0",
            "check w foob 111",
            "check w fooc 0x1101",
            "check b 0xfc00 0",
            "check b 0xfc01 1",
        ],
    );
}
