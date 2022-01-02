use crate::error::{Error, Result};
use crate::{Address, Bus};

pub struct RandomAccessMemory {
    data: Vec<u8>,
}

impl RandomAccessMemory {
    pub fn _from_vec(vec: Vec<u8>) -> Self {
        Self { data: vec }
    }

    pub fn with_capacity(capacity: u32) -> Self {
        Self {
            data: vec![0; capacity as usize],
        }
    }
}

impl Bus<Address> for RandomAccessMemory {
    fn read(&self, address: Address) -> Result<u8> {
        if address as usize >= self.data.len() {
            Err(Error::AddressNotMapped(address))
        } else {
            Ok(self.data[address as usize])
        }
    }

    fn write(&mut self, address: Address, value: u8) -> Result<()> {
        if address as usize >= self.data.len() {
            Err(Error::AddressNotMapped(address))
        } else {
            self.data[address as usize] = value;
            Ok(())
        }
    }
}
