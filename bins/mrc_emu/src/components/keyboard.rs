use mrc_emulator::{Bus, Port};

#[derive(Default)]
pub struct KeyboardController;

impl Bus<Port> for KeyboardController {
    fn read(&self, _address: Port) -> mrc_emulator::error::Result<u8> {
        todo!()
    }

    fn write(&mut self, address: Port, value: u8) -> mrc_emulator::error::Result<()> {
        log::info!(
            "Writing {:#04x} to keyboard controller on {:#06x}",
            value,
            address
        );
        Ok(())
        // todo!()
    }
}
