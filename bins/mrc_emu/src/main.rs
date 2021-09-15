mod cpu;
mod memory;

use crate::cpu::{segment_and_offset, SegmentAndOffset};
use crate::memory::{MemoryManager, PhysicalMemory};
use clap::{App, Arg};
use cpu::Cpu;
use std::io::Read;

fn main() {
    let matches = App::new("mrc-emu")
        .version("0.0.1")
        .arg(
            Arg::with_name("binary")
                .value_name("BINARY")
                .help("The binary file to emulate.")
                .takes_value(true),
        )
        .get_matches();

    let mut buffer = vec![];

    if let Some(binary) = matches.value_of("binary") {
        if let Ok(mut file) = std::fs::File::open(binary) {
            file.read_to_end(&mut buffer).unwrap();
        }
    }

    if buffer.is_empty() {
        println!("Could not read binary file.");
        return;
    }

    let mut mm = MemoryManager::new();
    mm.map(
        segment_and_offset(0x0000, 0x0000),
        0xFFFFF,
        Box::new(PhysicalMemory::new()),
    );

    let mut cpu = Cpu::new(&mut mm);

    cpu.start();
}
