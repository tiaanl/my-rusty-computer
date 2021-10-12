use std::process::Command;

// Example custom build script.
fn main() {
    // Tell Cargo that if the given file changes, to rerun this build script.
    println!("cargo:rerun-if-changed=ext/mrc_bios/bios.asm");
    println!("cargo:rerun-if-changed=ext/mrc_bios/cpu_test.asm");

    let success = Command::new("nasm")
        .args(&[
            "-f",
            "bin",
            "-Iext/mrc_bios/",
            "-o",
            "ext/mrc_bios/bios.bin",
            "ext/mrc_bios/bios.asm",
        ])
        .status()
        .unwrap()
        .success();

    if !success {
        std::process::exit(1);
    }
}
