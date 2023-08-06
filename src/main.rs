#![allow(dead_code)]
#![feature(assert_matches)]
#![feature(iter_intersperse)]
#![feature(map_try_insert)]

mod block_emitter;
mod ccpu;
mod compile;
mod constant;
mod ctype;
mod deconstruct;
mod enums;
mod error;
mod flush;
mod function;
mod generic_ir;
mod graph;
mod initializer;
mod ir;
mod lvalue;
mod machine;
mod name_scope;
mod object_location;
mod opt;
mod regalloc;
mod register;
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
        void bar(int x, int y);
        int foo(int x, int y) {
            int z = x + y;
            bar(x, y);
            bar(z, x - y);
            return (x < y);
        }
    "
        .to_string(),
    )
    .unwrap();
    // println!("{:#?}", p);
    let mut ec = ErrorCollector::new();
    println!("========== TRANSLATE ===========");
    let tu = TranslationUnit::translate(p.unit, &mut ec);
    ec.print_issues();
    if let Ok(mut tu) = tu {
        println!("{}", tu);
        println!("========== ENFORCE SSA ===========");
        tu.enforce_ssa();
        println!("{}", tu);
        println!("========== OPTIMIZE SSA ===========");
        tu.optimize_ssa();
        println!("{}", tu);
        println!("========== UTILISE INTRINSIC CALLS ===========");
        tu.utilise_intrin_calls();
        println!("{}", tu);
        println!("========== ENFORCE CALL REGS ===========");
        tu.enforce_special_regs();
        println!("{}", tu);
        println!("========== DECONSTRUCT SSA ===========");
        let mut tu = tu.deconstruct_ssa();
        println!("{}", tu);
        println!("========== OPTIMIZE DECONSTRUCTED ===========");
        tu.optimize_deconstructed();
        println!("{}", tu);
        println!("========== GENERATE ===========");
        let w = ccpu::gen::gen_tu(tu);
        println!("{}", w);
    }
}
