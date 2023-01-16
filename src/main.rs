#![allow(dead_code)]

mod ctype;
mod machine;
mod type_registry;
mod translation_unit;
mod error;
mod ir;
mod type_builder;

#[macro_use]
extern crate static_assertions;

use lang_c::driver::{Config, Flavor, parse_preprocessed};

fn main() {
    let mut cfg = Config::default();
    cfg.flavor = Flavor::StdC11;
    let p = parse_preprocessed(&cfg, "
        typedef int t;
        const t x;
    ".to_string()).unwrap();
    println!("{:#?}", p);
}

#[cfg(test)]
mod tests {

    #[test]
    fn test() {

    }

}
