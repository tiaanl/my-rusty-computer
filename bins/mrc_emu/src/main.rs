mod cpu;
mod memory;

#[cfg(feature = "sdl")]
mod monitor;

#[cfg(feature = "dos")]
mod dos;

use crate::cpu::segment_and_offset;
use crate::memory::{MemoryMapper, PhysicalMemory, ReadOnlyMemory};
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
        // Load 512 bytes from the beginning of the binary image to 0000:7C00 in physical memory,
        // because that is where the cs:ip is pointing.  (0000:7C00)
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
use std::io::Read;

fn load_bios<P: AsRef<std::path::Path>>(path: P) -> Result<Vec<u8>, std::io::Error> {
    let metadata = std::fs::metadata(&path)?;
    let file_size = metadata.len();

    let mut file = std::fs::File::open(&path)?;

    let mut data: Vec<u8> = Vec::with_capacity(file_size as usize);
    data.resize(file_size as usize, 0);

    file.read_exact(&mut data.as_mut_slice())?;

    Ok(data)
}

fn main() {
    let matches = App::new("mrc-emu")
        .version("0.0.1")
        .arg(
            Arg::with_name("binary")
                .value_name("binary")
                .help("The binary file to emulate.")
                .takes_value(true)
                .required(true),
        )
        .arg(
            Arg::with_name("bios")
                .value_name("bios")
                .help("A binary containing a BIOS.")
                .takes_value(true),
        )
        .get_matches();

    #[cfg(feature = "sdl")]
    {
        let monitor_handle = std::thread::spawn(monitor::run_loop);
    }

    let mut memory_manager = MemoryMapper::new();

    // Physical memory is 640KiB.
    let mut physical_memory = PhysicalMemory::with_capacity(0xA0000);

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

    memory_manager.map(
        segment_and_offset(0x0000, 0x0000),
        0xA0000,
        Rc::new(RefCell::new(physical_memory)),
    );

    if let Some(path) = matches.value_of("bios") {
        println!("Loading BIOS from: {}", path);
        let data = match load_bios(path) {
            Ok(r) => r,
            Err(err) => {
                eprintln!("Could not read BIOS file. ({}) ({})", &path, err);
                return;
            }
        };
        let data_size = data.len() as u16;
        let bios_start_addr = segment_and_offset(0xF000, 0xFFFF - data_size) + 1;

        println!("BIOS start address: {:05X}", bios_start_addr);

        let rom = ReadOnlyMemory::from_vec(data);
        memory_manager.map(
            bios_start_addr,
            data_size as u32,
            Rc::new(RefCell::new(rom)),
        );
    }

    Cpu::new(memory_manager).start();

    #[cfg(feature = "sdl")]
    monitor_handle.join().unwrap();
}
