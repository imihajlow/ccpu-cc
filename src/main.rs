#![allow(dead_code)]
#![feature(assert_matches)]
#![feature(iter_intersperse)]
#![feature(map_try_insert)]

mod block_emitter;
mod compile;
mod constant;
mod ctype;
mod deconstruct;
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
mod regalloc;
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
        int x = a, y = x ? a + 1 : x - 2;
        return x + y;
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
        tu.deconstruct_ssa();
        println!("<{}>", tu);
    }
}
