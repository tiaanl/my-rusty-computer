use std::process::Command;

fn compile_test(name: &str) {
    let source = format!("tests/{name}.asm");
    let binary = format!("tests/{name}.bin");

    let result = Command::new("/home/tilo/code/nasm/nasm")
        .arg("-o")
        .arg(binary)
        .arg(source)
        .status()
        .expect("Could not run nasm");
    assert_eq!(result.code().unwrap(), 0);
}

fn main() {
    println!("cargo:rerun-if-changed=tests/calljmp.asm");
    println!("cargo:rerun-if-changed=tests/ea.asm");
    println!("cargo:rerun-if-changed=tests/each.asm");
    println!("cargo:rerun-if-changed=tests/group1.asm");
    println!("cargo:rerun-if-changed=tests/imul.asm");
    println!("cargo:rerun-if-changed=tests/incdec.asm");

    compile_test("calljmp");
    compile_test("ea");
    compile_test("each");
    compile_test("group1");
    compile_test("imul");
    compile_test("incdec");
}
