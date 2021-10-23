mod config;
mod debugger;

use std::cell::RefCell;
use std::convert::TryFrom;
use std::io::Read;
use std::rc::Rc;
use std::sync::Arc;
use std::time::Instant;

use config::Config;
use debugger::Debugger;
use glutin::event_loop::ControlFlow;
use mrc_emulator::builder::EmulatorBuilder;
use mrc_emulator::pic::ProgrammableInterruptController8259;

use crate::debugger::DebuggerAction;
use mrc_emulator::ram::RandomAccessMemory;
use mrc_emulator::rom::ReadOnlyMemory;
use mrc_emulator::shared::Shared;
use mrc_emulator::timer::ProgrammableIntervalTimer8253;
use mrc_screen::{Screen, TextMode, TextModeInterface};

fn load_rom<P: AsRef<std::path::Path>>(path: P) -> std::io::Result<Vec<u8>> {
    let metadata = std::fs::metadata(&path)?;
    let file_size = metadata.len();

    // TODO: This should be returned as an Err from the function.
    assert!(file_size.is_power_of_two());

    let mut data = vec![0; file_size as usize];

    std::fs::File::open(&path)?.read_exact(data.as_mut_slice())?;

    Ok(data)
}

fn install_bios(builder: &mut EmulatorBuilder, path: &str) {
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

    builder.map_address(
        bios_start_addr,
        data_size as u32,
        ReadOnlyMemory::from_vec(data),
    );
}

fn create_emulator(
    builder: &mut EmulatorBuilder,
    config: &Config,
    text_mode: Rc<RefCell<TextMode>>,
) {
    // 640KiB RAM
    builder.map_address(0x00000, 0xA0000, RandomAccessMemory::with_capacity(0xA0000));

    // Install 2 programmable interrupt controllers.
    builder.map_io_range(0x20, 0x0F, ProgrammableInterruptController8259::default());
    builder.map_io_range(0xA0, 0x0F, ProgrammableInterruptController8259::default());

    // Programmable Interval Timer
    builder.map_io_range(0x40, 0x04, ProgrammableIntervalTimer8253::default());

    /*
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
    */

    if let Some(bios_path) = &config.bios_path {
        install_bios(builder, bios_path);
    } else {
        let data = include_bytes!("../ext/mrc_bios/bios.bin");
        let data = Vec::from(*data);
        let data_size = data.len() as u32;
        let bios_start_addr = 0x100000 - data_size;

        builder.map_address(bios_start_addr, data_size, ReadOnlyMemory::from_vec(data));
    }

    builder.map_address(0xB8000, 0x4000, TextModeInterface::new(text_mode.clone()));
    builder.map_interrupt(0x10, TextModeInterface::new(text_mode.clone()));

    builder.reset_vector(0xF000, 0xFFF0);
}

fn main() {
    pretty_env_logger::init();

    let config = Config::default();

    let event_loop = glutin::event_loop::EventLoop::new();

    let mut debugger = Debugger::new(&event_loop);

    let text_mode = Rc::new(RefCell::new(TextMode::default()));
    let screen = Rc::new(RefCell::new(Screen::new(&event_loop, text_mode.clone())));

    let mut builder = EmulatorBuilder::new();

    create_emulator(&mut builder, &config, text_mode.clone());

    let emulator = Arc::new(Shared::new(builder.build()));

    let mut last_monitor_update = Instant::now();

    event_loop.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Poll;

        if let Some(cf) = screen.borrow().handle_events(&event) {
            *control_flow = cf;
            return;
        }

        if let Some(debugger_action) = debugger.handle_events(&event) {
            match debugger_action {
                DebuggerAction::Step => {
                    emulator.write().tick();
                }
            }
            return;
        }

        // 60 fps
        let now = Instant::now();
        if now >= last_monitor_update + std::time::Duration::from_nanos(16_666_667) {
            last_monitor_update = now;
            screen.borrow_mut().tick();
            debugger.update(emulator.read());
            debugger.tick();
        }
    });
}
