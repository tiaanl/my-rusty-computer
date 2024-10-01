use mrc_emulator::{Address, Bus};
use tracing::info;

/// Temporary implementation for the NMI register
#[allow(clippy::upper_case_acronyms)]
pub struct NMI {
    pub value: u8,
}

impl Bus for NMI {
    fn read(&self, port: Address) -> u8 {
        info!("Reading NMI register from port {:#06X}", port);
        self.value
    }

    fn write(&mut self, port: Address, value: u8) {
        info!(
            "Writing to NMI port {:#06X} with value {:#04X}",
            port, value
        );
    }
}
