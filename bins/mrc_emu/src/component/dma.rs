use mrc_emulator::{Bus, Port};
use mrc_emulator::error::Result;

#[derive(Default)]
pub struct DMAController;

impl Bus<Port> for DMAController {
    fn read(&self, _port: u16) -> Result<u8> {
        todo!()
    }

    fn write(&mut self, _port: u16, _value: u8) -> Result<()> {
        todo!()
    }
}
