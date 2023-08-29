use crate::build::{build, run};

mod build;

#[macro_use]
extern crate lazy_static;

fn test_shift_common_const<F>(ctype: &str, w: &str, op: &str, count: usize, val: u64, shift: F)
where
    F: Fn(usize) -> u64,
{
    let mut code = format!("{} fooa", ctype);
    for i in 0..count {
        code += &format!(", foob{}", i);
    }
    code += "; void main(void) {";
    for i in 0..count {
        code += &format!("foob{} = fooa {} {};", i, op, i);
    }
    code += "}";
    let (bin, map) = build(&code);

    let mut script = vec![
        "u main".to_string(),
        format!("poke {} fooa {}", w, val),
        "u main_exit".to_string(),
    ];
    for i in 0..count {
        script.push(format!("check {} foob{} {}", w, i, shift(i)));
    }
    let script_refs: Vec<&str> = script.iter().map(|s| s.as_str()).collect();

    run(&bin, &map, &script_refs);
}

fn test_shift_common_var<F>(ctype: &str, w: &str, op: &str, count: usize, val: u64, shift: F)
where
    F: Fn(usize) -> u64,
{
    let mut code = format!("{} fooa", ctype);
    for i in 0..count {
        code += &format!(", foob{}", i);
        code += &format!(", fooc{}", i);
    }
    code += "; void main(void) {";
    for i in 0..count {
        code += &format!("fooc{} = fooa {} foob{};", i, op, i);
    }
    code += "}";
    let (bin, map) = build(&code);

    let mut script = vec!["u main".to_string(), format!("poke {} fooa {}", w, val)];
    for i in 0..count {
        script.push(format!("poke {} foob{} {}", w, i, i));
    }
    script.push("u main_exit".to_string());
    for i in 0..count {
        script.push(format!("check {} fooc{} {}", w, i, shift(i)));
    }
    let script_refs: Vec<&str> = script.iter().map(|s| s.as_str()).collect();

    run(&bin, &map, &script_refs);
}

#[test]
fn test_shl_const_byte() {
    let c: u64 = 0xa5;
    test_shift_common_const("char", "b", "<<", 10, c, |i| {
        if i < 8 {
            (c as u8).wrapping_shl(i as u32).into()
        } else {
            0
        }
    });
}

#[test]
fn test_shl_const_word() {
    let c: u64 = 0x5aa5;
    test_shift_common_const("unsigned int", "w", "<<", 18, c, |i| {
        if i < 16 {
            (c as u16).wrapping_shl(i as u32).into()
        } else {
            0
        }
    });
}

#[test]
fn test_shl_const_dword() {
    let c: u64 = 0xab125aa5;
    test_shift_common_const("unsigned long", "d", "<<", 34, c, |i| {
        if i < 32 {
            (c as u32).wrapping_shl(i as u32).into()
        } else {
            0
        }
    });
}

#[test]
fn test_shl_const_qword() {
    let c: u64 = 0x87654321ab125aa5;
    test_shift_common_const("unsigned long long", "q", "<<", 66, c, |i| {
        if i < 64 {
            (c as u64).wrapping_shl(i as u32).into()
        } else {
            0
        }
    });
}

#[test]
fn test_shl_var_byte() {
    let c: u64 = 0xa5;
    test_shift_common_var("char", "b", "<<", 10, c, |i| {
        if i < 8 {
            (c as u8).wrapping_shl(i as u32).into()
        } else {
            0
        }
    });
}

#[test]
fn test_shl_var_word() {
    let c: u64 = 0x5aa5;
    test_shift_common_var("unsigned int", "w", "<<", 18, c, |i| {
        if i < 16 {
            (c as u16).wrapping_shl(i as u32).into()
        } else {
            0
        }
    });
}

#[test]
fn test_shl_var_dword() {
    let c: u64 = 0xab125aa5;
    test_shift_common_var("unsigned long", "d", "<<", 34, c, |i| {
        if i < 32 {
            (c as u32).wrapping_shl(i as u32).into()
        } else {
            0
        }
    });
}

#[test]
fn test_sar_const_byte_neg() {
    let c: u64 = 0xa5;
    test_shift_common_const("signed char", "b", ">>", 10, c, |i| {
        if i < 8 {
            ((c as i8).wrapping_shr(i as u32) as u64) & 0xff
        } else {
            0xff
        }
    });
}

#[test]
fn test_sar_const_byte_pos() {
    let c: u64 = 0x12;
    test_shift_common_const("signed char", "b", ">>", 10, c, |i| {
        if i < 8 {
            ((c as i8).wrapping_shr(i as u32) as u64) & 0xff
        } else {
            0
        }
    });
}

#[test]
fn test_sar_const_word_neg() {
    let c: u64 = 0x8aa5;
    test_shift_common_const("int", "w", ">>", 18, c, |i| {
        if i < 16 {
            ((c as i16).wrapping_shr(i as u32) as u64) & 0xffff
        } else {
            0xffff
        }
    });
}

#[test]
fn test_sar_const_word_pos() {
    let c: u64 = 0x1aa5;
    test_shift_common_const("int", "w", ">>", 18, c, |i| {
        if i < 16 {
            ((c as i16).wrapping_shr(i as u32) as u64) & 0xffff
        } else {
            0
        }
    });
}

#[test]
fn test_sar_const_dword_neg() {
    let c: u64 = 0xab125aa5;
    test_shift_common_const("long", "d", ">>", 34, c, |i| {
        if i < 32 {
            ((c as i32).wrapping_shr(i as u32) as u64) & 0xffffffff
        } else {
            0xffffffff
        }
    });
}

#[test]
fn test_sar_const_dword_pos() {
    let c: u64 = 0x4b125aa5;
    test_shift_common_const("long", "d", ">>", 34, c, |i| {
        if i < 32 {
            ((c as i32).wrapping_shr(i as u32) as u64) & 0xffffffff
        } else {
            0
        }
    });
}

#[test]
fn test_sar_const_qword_neg() {
    let c: u64 = 0x87654321ab125aa5;
    test_shift_common_const("long long", "q", ">>", 66, c, |i| {
        if i < 64 {
            (c as i64).wrapping_shr(i as u32) as u64
        } else {
            0xffff_ffff_ffff_ffff
        }
    });
}

#[test]
fn test_sar_const_qword_pos() {
    let c: u64 = 0x17654321ab125aa5;
    test_shift_common_const("long long", "q", ">>", 66, c, |i| {
        if i < 64 {
            (c as i64).wrapping_shr(i as u32) as u64
        } else {
            0
        }
    });
}

#[test]
fn test_sar_var_byte_neg() {
    let c: u64 = 0xa5;
    test_shift_common_var("signed char", "b", ">>", 10, c, |i| {
        if i < 8 {
            ((c as i8).wrapping_shr(i as u32) as u64) & 0xff
        } else {
            0xff
        }
    });
}

#[test]
fn test_sar_var_byte_pos() {
    let c: u64 = 0x12;
    test_shift_common_var("signed char", "b", ">>", 10, c, |i| {
        if i < 8 {
            ((c as i8).wrapping_shr(i as u32) as u64) & 0xff
        } else {
            0
        }
    });
}

#[test]
fn test_sar_var_word_neg() {
    let c: u64 = 0x8aa5;
    test_shift_common_var("int", "w", ">>", 18, c, |i| {
        if i < 16 {
            ((c as i16).wrapping_shr(i as u32) as u64) & 0xffff
        } else {
            0xffff
        }
    });
}

#[test]
fn test_sar_var_word_pos() {
    let c: u64 = 0x1aa5;
    test_shift_common_var("int", "w", ">>", 18, c, |i| {
        if i < 16 {
            ((c as i16).wrapping_shr(i as u32) as u64) & 0xffff
        } else {
            0
        }
    });
}
