
use crate::build::{run, build};

mod build;

#[macro_use]
extern crate lazy_static;

#[test]
fn test_shl_const_char() {
    let mut code = "char fooa; char ".to_string();
    for i in 0..10 {
        if i != 0 {
            code += ",";
        }
        code += &format!(" foob{}", i);
    }
    code += "; void main(void) {";
    for i in 0..10 {
        code += &format!("foob{} = fooa << {};", i, i);
    }
    code += "}";
    let (bin, map) = build(&code);

    let mut script = vec!["u main".to_string(), "poke b fooa 0xa5".to_string(), "u main_exit".to_string()];
    for i in 0..8 {
        script.push(format!("check b foob{} {}", i, (0xa5 as u8).wrapping_shl(i)));
    }
    script.push("check b foob8 0".to_string());
    script.push("check b foob9 0".to_string());

    let script_refs: Vec<&str> = script.iter().map(|s| s.as_str()).collect();

    run(&bin, &map, &script_refs);
}
