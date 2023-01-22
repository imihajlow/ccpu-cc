#![allow(dead_code)]
#![feature(assert_matches)]


mod ctype;
mod error;
mod ir;
mod machine;
mod translation_unit;
mod type_builder;
mod type_registry;
mod constant;
mod initializer;
mod string;

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
