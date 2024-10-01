use crate::{Address, Bus};

#[derive(Default)]
pub struct Cga;

impl Bus for Cga {
    fn read(&self, _address: Address) -> u8 {
        todo!()
    }

    fn write(&mut self, address: Address, value: u8) {
        log::info!("Writing {:#04x} to CGA adapter on {:#06x}", value, address);
    }
}
