use crate::{Bus, Port};

#[derive(Default)]
pub struct Cga;

impl Bus<Port> for Cga {
    fn read(&self, _address: Port) -> u8 {
        todo!()
    }

    fn write(&mut self, address: Port, value: u8) {
        log::info!("Writing {:#04x} to CGA adapter on {:#06x}", value, address);
    }
}
