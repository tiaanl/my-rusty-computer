use mrc_emulator::error::Error;
use mrc_emulator::error::Result;
use mrc_emulator::{Bus, Port};

#[derive(Default)]
pub struct Panel {
    colors: [u8; 16],
}

impl Panel {
    pub fn colors(&self) -> &[u8; 16] {
        &self.colors
    }
}

impl Bus<Port> for Panel {
    fn read(&self, address: Port) -> Result<u8> {
        Err(Error::InvalidPort(address))
    }

    fn write(&mut self, _address: Port, value: u8) -> Result<()> {
        let index = value >> 4;
        let color = value & 0x0F;

        self.colors[index as usize] = color;

        Ok(())
    }
}
