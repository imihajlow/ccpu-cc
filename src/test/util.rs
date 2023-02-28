use crate::error::ErrorCollector;
use crate::ir;
use crate::translation_unit::TranslationUnit;

pub fn compile(code: &str) -> (TranslationUnit, ErrorCollector) {
    use lang_c::driver::{parse_preprocessed, Config, Flavor};
    let mut cfg = Config::default();
    cfg.flavor = Flavor::StdC11;
    let p = parse_preprocessed(&cfg, code.to_string()).unwrap();
    let mut ec = ErrorCollector::new();
    let tu = TranslationUnit::translate(p.unit, &mut ec).unwrap();
    assert_eq!(ec.get_error_count(), 0);
    (tu, ec)
}

pub fn assert_compile_error(code: &str) {
    use lang_c::driver::{parse_preprocessed, Config, Flavor};
    let mut cfg = Config::default();
    cfg.flavor = Flavor::StdC11;
    let p = parse_preprocessed(&cfg, code.to_string()).unwrap();
    let mut ec = ErrorCollector::new();
    assert!(TranslationUnit::translate(p.unit, &mut ec).is_err());
}

pub fn get_first_body(tu: &TranslationUnit) -> &Vec<ir::Block> {
    tu.functions.first().unwrap().get_body()
}


pub fn translate(code: &str) -> (Result<TranslationUnit, ()>, ErrorCollector) {
    use lang_c::driver::{parse_preprocessed, Config, Flavor};
    let mut cfg = Config::default();
    cfg.flavor = Flavor::StdC11;
    let p = parse_preprocessed(&cfg, code.to_string()).unwrap();
    let mut ec = ErrorCollector::new();
    (TranslationUnit::translate(p.unit, &mut ec), ec)
}
