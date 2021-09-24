mod cpu;
mod memory;

#[cfg(feature = "dos")]
mod dos;

use crate::cpu::segment_and_offset;
use crate::memory::{MemoryManager, PhysicalMemory};
use clap::{App, Arg};
use cpu::Cpu;
use std::cell::RefCell;
use std::rc::Rc;

#[cfg(feature = "dos")]
use dos::load_binary;

#[cfg(not(feature = "dos"))]
mod no_dos {
    use super::cpu::segment_and_offset;
    use crate::memory::PhysicalMemory;
    use std::io::Read;

    pub fn load_binary(
        physical_memory: &mut PhysicalMemory,
        file: &mut std::fs::File,
    ) -> std::io::Result<()> {
        // Load 512 bytes from the beginning of the binary image to the last 515 bytes at the top of
        // physical memory.
        let start_position = segment_and_offset(0x0000, 0x7C00) as usize;
        let mut destination = &mut physical_memory.data[start_position..start_position + 512];
        if let Err(err) = file.read_exact(&mut destination) {
            panic!("{}", err);
        }

        Ok(())
    }
}

#[cfg(not(feature = "dos"))]
use no_dos::load_binary;

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

    let mut physical_memory = PhysicalMemory::with_capacity(0xFFFFF);

    let path = match matches.value_of("binary") {
        None => {
            eprintln!("No binary file specified.");
            return;
        }
        Some(path) => path,
    };

    let mut file = match std::fs::File::open(path) {
        Err(err) => {
            eprintln!("Could not open binary file. {}", err);
            return;
        }
        Ok(file) => file,
    };

    load_binary(&mut physical_memory, &mut file).unwrap();

    let mut memory_manager = MemoryManager::new();
    memory_manager.map(
        segment_and_offset(0x0000, 0x0000),
        0xFFFFF,
        Rc::new(RefCell::new(physical_memory)),
    );

    let memory_manager = Rc::new(RefCell::new(memory_manager));

    Cpu::new(memory_manager).start();
}
