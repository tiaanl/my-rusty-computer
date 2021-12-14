pub mod dma;
pub mod pic;
pub mod pit;
pub mod ram;
pub mod rom;

use mrc_emulator::io::IOInterface;
use mrc_emulator::error::Result;

/// Temporary implementation for the NMI register
#[allow(clippy::upper_case_acronyms)]
pub struct NMI {
    pub value: u8,
}

impl IOInterface for NMI {
    fn read(&self, port: u16) -> Result<u8> {
        log::info!("Reading NMI register from port {:#06X}", port);
        Ok(self.value)
    }

    fn write(&mut self, port: u16, value: u8) -> Result<()> {
        log::info!("Writing to NMI port {:#06X} with value {:#04X}", port, value);
        Ok(())
    }
}
