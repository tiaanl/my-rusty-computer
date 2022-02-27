pub mod cga;
pub mod dma;
pub mod pic;
pub mod pit;
pub mod ppi;

use mrc_emulator::error::Result;
use mrc_emulator::{Bus, Port};

/// Temporary implementation for the NMI register
#[allow(clippy::upper_case_acronyms)]
pub struct NMI {
    pub value: u8,
}

impl Bus<Port> for NMI {
    fn read(&self, port: u16) -> Result<u8> {
        log::info!("Reading NMI register from port {:#06X}", port);
        Ok(self.value)
    }

    fn write(&mut self, port: u16, value: u8) -> Result<()> {
        log::info!(
            "Writing to NMI port {:#06X} with value {:#04X}",
            port,
            value
        );
        Ok(())
    }
}
