#![allow(dead_code)]
#![feature(assert_matches)]


mod ctype;
mod error;
mod ir;
mod machine;
mod translation_unit;
mod type_builder;
mod constant;
mod initializer;
mod string;
mod function;
mod compile;
mod name_scope;
mod block_emitter;

#[macro_use]
extern crate static_assertions;

use lang_c::driver::{parse_preprocessed, Config, Flavor};

fn main() {
    let mut cfg = Config::default();
    cfg.flavor = Flavor::StdC11;
    let p = parse_preprocessed(
        &cfg,
        "
        int x = \"hello\" \"world\";
    "
        .to_string(),
    )
    .unwrap();
    println!("{:#?}", p);
}
