use crate::error::{Error, Result};
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
    fn read(&self, address: Address) -> Result<u8> {
        if address as usize >= self.data.len() {
            Err(Error::AddressNotMapped(address))
        } else {
            Ok(self.data[address as usize])
        }
    }

    fn write(&mut self, address: Address, _value: u8) -> Result<()> {
        Err(Error::AddressOutOfRange(address))
    }
}
