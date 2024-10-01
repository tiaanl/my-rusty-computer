mod components;
mod config;
mod emulator;
mod interrupts;

use crate::config::Config;
use crate::emulator::BIOS_SIZE;
use crate::interrupts::InterruptManager;
// use glium::{glutin, Surface};
use mrc_emulator::components::rom::ReadOnlyMemory;
use tracing::{error, info, warn};
// use mrc_emulator::debugger::{Debugger, DebuggerState};
use std::io::Read;

fn load_rom(path: impl AsRef<std::path::Path>) -> std::io::Result<Vec<u8>> {
    let metadata = std::fs::metadata(&path)?;
    let file_size = metadata.len();

    let mut data = vec![0; file_size as usize];

    std::fs::File::open(&path)?.read_exact(data.as_mut_slice())?;

    Ok(data)
}

/// Create a [ReadOnlyMemory] instance that has a fixed `size`.  If the path is valid and could
/// be loaded from disk, then it will be mapped to the beginning of the block.
fn create_bios(path: Option<String>) -> ReadOnlyMemory {
    if let Some(path) = path {
        info!("Loading BIOS from: {path}");
        if let Ok(mut data) = load_rom(&path) {
            if data.len() > BIOS_SIZE {
                warn!(
                    "BIOS file truncated from {} to {}. ({})",
                    data.len(),
                    BIOS_SIZE,
                    &path
                );
            }
            data.resize(BIOS_SIZE, 0);
            return ReadOnlyMemory::from_vec(data);
        }
    }

    info!("Using internal BIOS.");
    let bytes = include_bytes!("../ext/mrc_bios/bios.bin");
    ReadOnlyMemory::from_vec(Vec::from(&bytes[..]))
}

fn main() {
    tracing_subscriber::fmt().init();

    let config = match Config::load() {
        Ok(cfg) => cfg,
        Err(err) => {
            error!("Could not load configuration! {:?}", err);
            std::process::exit(1);
        }
    };

    let bios = create_bios(config.bios);

    emulator::Emulator::new(bios).run();
}
