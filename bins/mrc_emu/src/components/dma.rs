use mrc_emulator::error::Result;
use mrc_emulator::{Bus, Port};

#[derive(Default)]
pub struct DMAController;

impl Bus<Port> for DMAController {
    fn read(&self, port: u16) -> Result<u8> {
        log::info!("Reading from DMA controller on port {:#06x}.", port);
        Ok(0)
    }

    fn write(&mut self, port: u16, value: u8) -> Result<()> {
        log::info!(
            "Writing {:#04x} to DMA controller on port {:#06x}.",
            value,
            port
        );
        Ok(())
    }
}
