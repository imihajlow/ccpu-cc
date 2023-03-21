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
mod rvalue;
mod ssa;
mod string;
mod struct_union;
mod temp_reg;
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
void swap(int* a, int* b) {
    int t = *a;
    *a = *b;
    *b = t;
}

int partition(int arr[], int low, int high) {
    int pivot = arr[high];
    int i = (low - 1);

    for (int j = low; j <= high - 1; j++) {
        if (arr[j] < pivot) {
            i++;
            swap(&arr[i], &arr[j]);
        }
    }
    swap(&arr[i + 1], &arr[high]);
    return (i + 1);
}

void quickSort(int arr[], int low, int high) {
    if (low < high) {
        int pi = partition(arr, low, high);
        quickSort(arr, low, pi - 1);
        quickSort(arr, pi + 1, high);
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
        tu.optimize_ssa();
        println!("<{}>", tu);
        let mut tu = tu.deconstruct_ssa();
        tu.optimize_deconstructed();
        println!("<{}>", tu);
    }
}
