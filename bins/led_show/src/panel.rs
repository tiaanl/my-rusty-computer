use mrc_emulator::{Address, Bus};

#[derive(Default)]
pub struct Panel {
    colors: [u8; 16],
}

impl Panel {
    pub fn colors(&self) -> &[u8; 16] {
        &self.colors
    }
}

impl Bus for Panel {
    fn read(&self, _address: Address) -> u8 {
        //Err(Error::InvalidPort(address))
        0
    }

    fn write(&mut self, _address: Address, value: u8) {
        let index = value >> 4;
        let color = value & 0x0F;

        self.colors[index as usize] = color;
    }
}
