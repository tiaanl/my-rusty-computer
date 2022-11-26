use crate::{Address, Bus};

pub struct ReadOnlyMemory {
    data: Vec<u8>,
}

impl ReadOnlyMemory {
    pub fn from_vec(vec: Vec<u8>) -> Self {
        Self { data: vec }
    }
}

impl Bus<Address> for ReadOnlyMemory {
    fn read(&self, address: Address) -> u8 {
        let address = address as usize;
        if address >= self.data.len() {
            log::warn!("Reading outside of bounds! ({:05X})", address);
            0
        } else {
            self.data[address]
        }
    }

    fn write(&mut self, address: Address, _value: u8) {
        log::warn!("Writing to read only memory at {:#05X}", address);
    }
}
