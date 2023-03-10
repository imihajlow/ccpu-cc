#![allow(dead_code)]
#![feature(assert_matches)]
#![feature(iter_intersperse)]

mod block_emitter;
mod compile;
mod constant;
mod ctype;
mod enums;
mod error;
mod flush;
mod function;
mod graph;
mod initializer;
mod ir;
mod lvalue;
mod machine;
mod name_scope;
mod object_location;
mod opt;
mod rvalue;
mod ssa;
mod string;
mod struct_union;
mod translation_unit;
mod type_builder;
mod utils;

#[cfg(test)]
mod test;

#[macro_use]
extern crate static_assertions;

use crate::{error::ErrorCollector, translation_unit::TranslationUnit};
use lang_c::driver::{parse_preprocessed, Config, Flavor};

fn main() {
    let mut cfg = Config::default();
    cfg.flavor = Flavor::StdC11;
    let p = parse_preprocessed(
        &cfg,
        "
int send(int a)
{
    if (a) {
        return a + 1;
    } else {
        return a - 1;
    }
}
    "
        .to_string(),
    )
    .unwrap();
    // println!("{:#?}", p);
    let mut ec = ErrorCollector::new();
    let tu = TranslationUnit::translate(p.unit, &mut ec);
    ec.print_issues();
    if let Ok(mut tu) = tu {
        println!("<{}>", tu);
        tu.enforce_ssa();
        tu.optimize();
        println!("<{}>", tu);
    }
}
