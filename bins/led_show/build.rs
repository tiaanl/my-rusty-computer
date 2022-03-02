use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=assets/main.asm");

    let success = Command::new("nasm")
        .args([
            "-f",
            "bin",
            "-o",
            "assets/main.bin",
            "assets/main.asm",
        ])
        .status()
        .unwrap()
        .success();

    if !success {
        std::process::exit(1);
    }
}
