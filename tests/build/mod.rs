use assert_cmd::assert::OutputAssertExt;
use assert_cmd::Command;
use rand;
use std::{
    env::temp_dir,
    path::{Path, PathBuf},
};

lazy_static! {
    static ref RUNTIME_OBJ: PathBuf = assemble(&PathBuf::from("ccpu-runtime/runtime.asm"));
    static ref STARTUP_OBJ: PathBuf = assemble(&PathBuf::from("ccpu-runtime/rom_startup.asm"));
    static ref MEMCPY_OBJ: PathBuf = assemble(&PathBuf::from("ccpu-runtime/memcpy.asm"));
    static ref DIVIDE32_OBJ: PathBuf = assemble(&PathBuf::from("ccpu-runtime/divide32.asm"));
}

pub fn run(bin: &Path, map: &Path, script: &[&str]) {
    let mut cmd = Command::new("rsim");
    for s in script {
        cmd.arg("-c").arg(s);
    }
    cmd.arg("-c").arg("q");
    cmd.arg("--config").arg("tests/rsim-stack.yaml");
    cmd.arg(&bin).arg(&map).unwrap().assert().success();
}

pub fn build(code: &str) -> (PathBuf, PathBuf) {
    let obj = compile(code);
    link(
        &obj.with_extension("bin"),
        &[&obj, &RUNTIME_OBJ, &STARTUP_OBJ, &MEMCPY_OBJ, &DIVIDE32_OBJ],
        &PathBuf::from("tests/rom.yaml"),
    )
}

fn compile(code: &str) -> PathBuf {
    let mut filename = temp_dir();
    filename.push(format!("{:016X}.c", rand::random::<u64>()));
    std::fs::write(&filename, code).unwrap();
    let obj_filename = filename.with_extension("o");
    Command::cargo_bin("ccpu-cc")
        .unwrap()
        .arg("-std=gnu11")
        .arg("-o")
        .arg(&obj_filename)
        .arg(filename)
        .unwrap()
        .assert()
        .success();
    obj_filename
}

fn assemble(file: &Path) -> PathBuf {
    let assembler = PathBuf::from("../ccpu/tools/asm.py");
    let output = file.with_extension("o");
    Command::new(&assembler)
        .arg("-o")
        .arg(&output)
        .arg(&file)
        .unwrap()
        .assert()
        .success();
    output
}

fn link(output: &Path, objects: &[&Path], layout: &Path) -> (PathBuf, PathBuf) {
    let linker = PathBuf::from("../ccpu/tools/link.py");
    let map_path = output.with_extension("map");
    Command::new(&linker)
        .arg("-o")
        .arg(output)
        .arg("--slim")
        .arg("--layout")
        .arg(&layout)
        .arg("-m")
        .arg(&map_path)
        .args(objects)
        .unwrap()
        .assert()
        .success();
    (output.to_path_buf(), map_path)
}
