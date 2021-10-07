use std::cell::RefCell;
use std::convert::TryFrom;
use std::io::Read;
use std::rc::Rc;

use clap::{App, Arg};

use mrc_emulator::{Emulator, segment_and_offset};
use mrc_emulator::ram::RandomAccessMemory;
use mrc_emulator::rom::ReadOnlyMemory;

mod cpu;
mod memory;
mod monitor;
mod video;

fn load_bios<P: AsRef<std::path::Path>>(path: P) -> std::io::Result<Vec<u8>> {
    let metadata = std::fs::metadata(&path)?;
    let file_size = metadata.len();

    // TODO: This should be returned as an Err from the function.
    assert!(file_size.is_power_of_two());

    let mut data = vec![0; file_size as usize];

    std::fs::File::open(&path)?.read_exact(data.as_mut_slice())?;

    Ok(data)
}

fn install_memory(emulator: &Emulator) {
    // 640KiB RAM
    let ram = RandomAccessMemory::with_capacity(0xA0000);
    emulator.bus().borrow_mut().map(0x00000, 0xA0000, Rc::new(RefCell::new(ram)));
}

fn install_bios(emulator: &Emulator, path: &str) {
    log::info!("Loading BIOS from: {}", path);

    let data = match load_bios(path) {
        Ok(r) => r,
        Err(err) => {
            log::error!("Could not read BIOS file. ({}) ({})", &path, err);
            return;
        }
    };

    let data_size = u32::try_from(data.len()).unwrap();
    let bios_start_addr = segment_and_offset(0xF000, 0x0000);

    log::info!(
            "Loading BIOS to: {:#05X}..{:#05X} ({:05X}/{} bytes)",
            bios_start_addr,
            bios_start_addr + data_size as u32,
            data_size,
            data_size
        );

    let rom = ReadOnlyMemory::from_vec(data);
    emulator.bus().borrow_mut().map(
        bios_start_addr,
        data_size as u32,
        Rc::new(RefCell::new(rom)),
    );
}

fn main() {
    pretty_env_logger::init();

    let matches = App::new("mrc-emu")
        .version("0.0.1")
        .arg(
            Arg::with_name("bios")
                .help("Specify a file to use as the BIOS")
                .required(true),
        )
        .get_matches();

    let mut emulator = Emulator::new();

    install_memory(&emulator);

    if let Some(path) = matches.value_of("bios") {
        install_bios(&emulator, path);
        emulator.set_reset_vector(0xF000, 0xFFF0);
    }

    /*
    // Video

    let video_memory = VideoMemory::default();
    memory_mapper.map(0xC0000, 0x8000, Rc::new(RefCell::new(video_memory)));

    let video_expansion = PhysicalMemory::with_capacity(0x28000);
    memory_mapper.map(0xC8000, 0x28000, Rc::new(RefCell::new(video_expansion)));

    let mut monitor = monitor::Monitor {};
    monitor.start();

    // Cpu::new(memory_mapper).start();
    */

    emulator.boot();
}
