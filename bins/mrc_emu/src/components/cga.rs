use mrc_emulator::{Bus, Port};

#[derive(Default)]
pub struct Cga;

impl Bus<Port> for Cga {
    fn read(&self, _address: Port) -> mrc_emulator::error::Result<u8> {
        todo!()
    }

    fn write(&mut self, address: Port, value: u8) -> mrc_emulator::error::Result<()> {
        log::info!("Writing {:#04x} to CGA adapter on {:#06x}", value, address);
        Ok(())
    }
}
