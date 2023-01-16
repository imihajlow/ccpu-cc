#![allow(dead_code)]

mod ctype;
mod error;
mod ir;
mod machine;
mod translation_unit;
mod type_builder;
mod type_registry;

#[macro_use]
extern crate static_assertions;

use lang_c::driver::{parse_preprocessed, Config, Flavor};

fn main() {
    let mut cfg = Config::default();
    cfg.flavor = Flavor::StdC11;
    let p = parse_preprocessed(
        &cfg,
        "
        typedef int t;
        const t x;
    "
        .to_string(),
    )
    .unwrap();
    println!("{:#?}", p);
}

#[cfg(test)]
mod tests {

    #[test]
    fn test() {}
}
