#![allow(dead_code)]
#![feature(assert_matches)]
#![feature(iter_intersperse)]


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
mod lvalue;

#[macro_use]
extern crate static_assertions;

use lang_c::{driver::{parse_preprocessed, Config, Flavor}};
use crate::{translation_unit::TranslationUnit, error::ErrorCollector};

fn main() {
    let mut cfg = Config::default();
    cfg.flavor = Flavor::StdC11;
    let p = parse_preprocessed(
        &cfg,
        "
        int x = 2;

        void foo(void) {
            x = 5;
            char y;
            y = x;
        }
    "
        .to_string(),
    )
    .unwrap();
    println!("{:#?}", p);
    let mut ec = ErrorCollector::new();
    let tu = TranslationUnit::translate(p.unit, &mut ec);
    ec.print_issues();
    if let Ok(tu) = tu {
        println!("{}", tu);
    }
}
