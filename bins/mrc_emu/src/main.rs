mod cpu;
mod memory;

use crate::cpu::segment_and_offset;
use crate::memory::{MemoryManager, PhysicalMemory};
use clap::{App, Arg};
use cpu::Cpu;
use std::cell::RefCell;
use std::rc::Rc;

#[cfg(feature = "dos")]
mod dos {
    use super::memory::PhysicalMemory;
    use std::io::{Read, SeekFrom};

    #[repr(C, packed)]
    struct ExeHeader {
        id: u16,
        // EXE Signature MZ or ZM
        extra_bytes: u16,
        // Bytes on the last page
        pages: u16,
        // Pages in file
        relocation_items: u16,
        // Relocations in file
        header_size: u16,
        // Paragraphs in header
        minimum_allocation: u16,
        // Minimum amount of memory
        maximum_allocation: u16,
        // Maximum amount of memory
        initial_ss: u16,
        initial_sp: u16,
        checksum: u16,
        initial_ip: u16,
        initial_cs: u16,
        relocation_table: u16,
        overlay: u16,
        // overlay_information: u16,
    }

    impl ExeHeader {
        fn header_size(&self) -> u16 {
            let header_size = self.header_size * 16;
            //let extra_start = self.pages * 512 - (512 - self.extra_bytes);
            header_size
        }
    }

    pub fn load_binary(
        physical_memory: &mut PhysicalMemory,
        file: &mut std::fs::File,
    ) -> std::io::Result<()> {
        let mut header: ExeHeader = unsafe { std::mem::zeroed() };

        // Make the config_slice point to the header struct.
        let config_slice = unsafe {
            std::slice::from_raw_parts_mut(
                &mut header as *mut _ as *mut u8,
                std::mem::size_of::<ExeHeader>(),
            )
        };

        let mut is_mz = false;
        if let Err(err) = file.read_exact(config_slice) {
            // It is OK If we could not read enough bytes from the file to fill physical memory.
            if err.kind() != std::io::ErrorKind::UnexpectedEof {
                return Err(err);
            }
        } else if header.id == 0x4D5A {
            // 0x4D5A == MZ
            is_mz = true
        }

        // Set the pointer in the file to where we want to continue reading the code.
        if is_mz {
            file.seek(SeekFrom::Start(header.header_size().into()))?;
        } else {
            file.seek(SeekFrom::Start(0))?;
        }

        // Read the code from the file into the physical memory.
        if let Err(err) = file.read_exact(&mut physical_memory.data[0x100..]) {
            if err.kind() != std::io::ErrorKind::UnexpectedEof {
                return Err(err);
            }
        }

        Ok(())
    }
}

#[cfg(feature = "dos")]
use dos::load_binary;

#[cfg(not(feature = "dos"))]
mod no_dos {
    use super::cpu::segment_and_offset;
    use super::memory::PhysicalMemory;
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
        Box::new(physical_memory),
    );

    let memory_manager = Rc::new(RefCell::new(memory_manager));

    Cpu::new(memory_manager).start();
}
