use std::process::Command;

const NASM: &str = "nasm";

fn compile_test(name: &str) {
    let source = format!("tests/{name}.asm");
    let binary = format!("tests/{name}.bin");

    let result = Command::new(NASM)
        .arg("-o")
        .arg(binary)
        .arg(source)
        .status()
        .expect("Could not run nasm");
    assert_eq!(result.code().unwrap(), 0);
}

const FILES: &[&str] = &[
    "calljmp", "curpos", "ea", "each", "group1", "imul", "incdec",
];

fn main() {
    for file in FILES {
        println!("cargo:rerun-if-changed=tests/{}.asm", file);
    }

    for file in FILES {
        compile_test(file);
    }
}
