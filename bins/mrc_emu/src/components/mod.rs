use mrc_emulator::{Bus, Port};

/// Temporary implementation for the NMI register
#[allow(clippy::upper_case_acronyms)]
pub struct NMI {
    pub value: u8,
}

impl Bus<Port> for NMI {
    fn read(&self, port: u16) -> u8 {
        log::info!("Reading NMI register from port {:#06X}", port);
        self.value
    }

    fn write(&mut self, port: u16, value: u8) {
        log::info!(
            "Writing to NMI port {:#06X} with value {:#04X}",
            port,
            value
        );
    }
}
