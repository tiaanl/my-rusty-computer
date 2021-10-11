use std::cell::RefCell;
use std::convert::TryFrom;
use std::io::Read;
use std::rc::Rc;
use std::time::Instant;

use clap::{App, Arg};
use glutin::event_loop::ControlFlow;

use mrc_display::Monitor;
use mrc_emulator::Emulator;
use mrc_emulator::ram::RandomAccessMemory;
use mrc_emulator::rom::ReadOnlyMemory;

mod cpu;
mod memory;
mod video;

fn load_rom<P: AsRef<std::path::Path>>(path: P) -> std::io::Result<Vec<u8>> {
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
    emulator
        .bus()
        .borrow_mut()
        .map(0x00000, 0xA0000, Rc::new(RefCell::new(ram)));
}

fn install_bios(emulator: &Emulator, path: &str) {
    log::info!("Loading BIOS from: {}", path);

    let data = match load_rom(path) {
        Ok(r) => r,
        Err(err) => {
            log::error!("Could not read BIOS file. ({}) ({})", &path, err);
            return;
        }
    };

    let data_size = u32::try_from(data.len()).unwrap();
    let bios_start_addr = 0x100000 - data_size;

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
                .required(false),
        )
        .get_matches();

    let mut emulator = Emulator::new();

    install_memory(&emulator);

    // Basic ROM just below BIOS.
    [
        (
            r#"C:\Code\my-rusty-computer\bins\mrc_emu\ext\bios\BASICF6.ROM"#,
            0xF6000,
        ),
        (
            r#"C:\Code\my-rusty-computer\bins\mrc_emu\ext\bios\BASICF8.ROM"#,
            0xF8000,
        ),
        (
            r#"C:\Code\my-rusty-computer\bins\mrc_emu\ext\bios\BASICFA.ROM"#,
            0xFA000,
        ),
        (
            r#"C:\Code\my-rusty-computer\bins\mrc_emu\ext\bios\BASICFC.ROM"#,
            0xFC000,
        ),
    ]
        .map(|(path, address)| {
            let data = load_rom(path).unwrap();
            let data_len = data.len() as u32;
            let rom = ReadOnlyMemory::from_vec(data);
            emulator
                .bus()
                .borrow_mut()
                .map(address, data_len, Rc::new(RefCell::new(rom)));
            log::info!(
            "Loading ROM \"{}\" to [{:05X}..{:05X}]",
            path,
            address,
            address + data_len
        );
        });

    if let Some(path) = matches.value_of("bios") {
        install_bios(&emulator, path);
    } else {
        let data = include_bytes!("../ext/mrc_bios/bios.bin");
        println!("size: {}", data.len());
        let data = Vec::from(*data);
        let data_size = data.len() as u32;
        let bios_start_addr = 0x100000 - data_size;

        emulator.bus().borrow_mut().map(
            bios_start_addr,
            data_size,
            Rc::new(RefCell::new(ReadOnlyMemory::from_vec(data))),
        );
    }

    emulator.set_reset_vector(0xF000, 0xFFF0);

    let event_loop = glutin::event_loop::EventLoop::new();

    let monitor = Rc::new(RefCell::new(Monitor::new(&event_loop)));
    emulator
        .bus()
        .borrow_mut()
        .map(0xB8000, 0x4000, monitor.clone());
    emulator
        .interrupt_controller()
        .borrow_mut()
        .map(0x10, monitor.clone());

    let last_monitor_update = Instant::now();

    let mut cpu_stopped = false;

    event_loop.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Poll;

        if let Some(cf) = monitor.borrow().handle_events(&event) {
            *control_flow = cf;
            return;
        }

        let now = Instant::now();

        if now >= last_monitor_update + std::time::Duration::from_nanos(16_666_667) {
            monitor.borrow_mut().tick();
        }

        if !cpu_stopped {
            for _ in 0..10 {
                if !emulator.tick() {
                    cpu_stopped = true;
                    return;
                    // *control_flow = ControlFlow::Exit;
                    // return;
                }
            }
        }
    });
}
