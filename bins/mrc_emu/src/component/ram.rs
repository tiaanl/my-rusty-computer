use mrc_emulator::bus::{Address, BusInterface};
use mrc_emulator::error::{Error, Result};

pub struct RandomAccessMemory {
    data: Vec<u8>,
}

impl RandomAccessMemory {
    pub fn _from_vec(vec: Vec<u8>) -> Self {
        Self { data: vec }
    }

    pub fn _with_capacity(capacity: u32) -> Self {
        Self {
            data: vec![0; capacity as usize],
        }
    }
}

impl BusInterface for RandomAccessMemory {
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
