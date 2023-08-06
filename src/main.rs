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
mod preprocess;
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

use std::fs::File;
use std::io::Write;
use std::{fs, path::PathBuf, process::exit};

use crate::{error::ErrorCollector, translation_unit::TranslationUnit};

use lang_c::driver::{parse, Config, Flavor};

use clap::Parser;

#[derive(Parser)]
#[command(name = "CCPU compiler")]
struct Cli {
    /// Be verbose
    #[arg(short, long)]
    verbose: bool,

    /// Output file name
    #[arg(short)]
    output: Option<PathBuf>,

    /// Define macro
    #[arg(short = 'D')]
    define: Vec<String>,

    /// Input file name
    input: PathBuf,
}

fn main() {
    let cli = Cli::parse();

    println!("{:?}", cli.define);

    let cfg = preprocess::get_config(cli.define);

    let output_path = if let Some(output) = cli.output {
        output
    } else {
        let mut output = cli.input.clone();
        output.set_extension("asm");
        output
    };

    let p = match parse(&cfg, cli.input) {
        Ok(p) => p,
        Err(e) => {
            println!("{}", e);
            exit(1);
        }
    };

    let mut ec = ErrorCollector::new();
    if cli.verbose {
        println!("========== TRANSLATE ===========");
    }
    let tu = TranslationUnit::translate(p.unit, &mut ec);
    ec.print_issues();
    if let Ok(mut tu) = tu {
        if cli.verbose {
            println!("{}", tu);
            println!("========== ENFORCE SSA ===========");
        }
        tu.enforce_ssa();
        if cli.verbose {
            println!("{}", tu);
            println!("========== OPTIMIZE SSA ===========");
        }
        tu.optimize_ssa();
        if cli.verbose {
            println!("{}", tu);
            println!("========== UTILISE INTRINSIC CALLS ===========");
        }
        tu.utilise_intrin_calls();
        if cli.verbose {
            println!("{}", tu);
            println!("========== ENFORCE CALL REGS ===========");
        }
        tu.enforce_special_regs();
        if cli.verbose {
            println!("{}", tu);
            println!("========== DECONSTRUCT SSA ===========");
        }
        let mut tu = tu.deconstruct_ssa();
        if cli.verbose {
            println!("{}", tu);
            println!("========== OPTIMIZE DECONSTRUCTED ===========");
        }
        tu.optimize_deconstructed();
        if cli.verbose {
            println!("{}", tu);
            println!("========== GENERATE ===========");
        }
        let w = ccpu::gen::gen_tu(tu);
        if cli.verbose {
            println!("{}", w);
        }

        if let Err(e) = File::create(output_path.clone()).and_then(|mut f| write!(f, "{}", w)) {
            println!("Cannot open {} for writing: {}", output_path.display(), e);
            exit(1);
        }
    } else {
        exit(1);
    }
}
