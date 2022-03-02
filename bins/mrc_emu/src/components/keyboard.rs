use mrc_emulator::{error::Result, Bus, Port};

#[derive(Default)]
pub struct Keyboard {
    data: u8,
}

impl Bus<Port> for Keyboard {
    fn read(&self, _address: Port) -> Result<u8> {
        log::info!("Writing to keyboard port: {:#04X}", self.data);

        Ok(self.data)
    }

    fn write(&mut self, _address: Port, value: u8) -> Result<()> {
        log::info!("Writing to keyboard port: {:#04X}", value);
        self.data = value;
        Ok(())
    }
}
