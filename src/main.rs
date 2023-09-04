#![allow(dead_code)]
#![feature(assert_matches)]
#![feature(iter_intersperse)]
#![feature(map_try_insert)]

mod block_emitter;
mod builtin;
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
mod offsetof;
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
use std::{path::PathBuf, process::exit};

use crate::{error::ErrorCollector, translation_unit::TranslationUnit};

use lang_c::driver::{parse, Flavor};

use clap::{Parser, ValueEnum};

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

    /// Add directory to the end of the list of include search paths
    #[arg(short = 'I')]
    include: Vec<String>,

    /// Add directory to SYSTEM include search path
    #[arg(long = "isystem")]
    isystem: Vec<String>,

    /// Add directory to QUOTE include search path
    #[arg(long = "iquote")]
    iquote: Vec<String>,

    /// C dialect
    #[arg(long = "std")]
    std: Option<Standard>,

    /// Input file name
    input: PathBuf,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Standard {
    C11,
    Gnu11,
    Clang11,
}

impl From<Standard> for Flavor {
    fn from(s: Standard) -> Self {
        match s {
            Standard::C11 => Flavor::StdC11,
            Standard::Gnu11 => Flavor::GnuC11,
            Standard::Clang11 => Flavor::ClangC11,
        }
    }
}

fn main() {
    let cli = Cli::parse();

    let cfg = preprocess::get_config(
        cli.std.unwrap_or(Standard::Gnu11).into(),
        cli.define,
        cli.include,
        cli.isystem,
        cli.iquote,
    );

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
    ec.print_issues_src(&p.source);
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
        let mut ec = ErrorCollector::new();
        let w = ccpu::gen::gen_tu(tu, &mut ec);
        ec.print_issues_src(&p.source);
        let w = if let Ok(w) = w {
            w
        } else {
            exit(1);
        };
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
