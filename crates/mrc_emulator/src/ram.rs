use crate::bus::{Address, BusError, BusInterface};

pub struct RandomAccessMemory {
    data: Vec<u8>,
}

impl RandomAccessMemory {
    pub fn with_capacity(capacity: u32) -> Self {
        Self {
            data: vec!(0; capacity as usize),
        }
    }
}

impl BusInterface for RandomAccessMemory {
    fn read(&self, address: Address) -> Result<u8, BusError> {
        if address as usize >= self.data.len() {
            Err(BusError::AddressNotMapped(address))
        } else {
            Ok(self.data[address as usize])
        }
    }

    fn write(&mut self, address: Address, value: u8) -> Result<(), BusError> {
        if address as usize >= self.data.len() {
            Err(BusError::AddressNotMapped(address))
        } else {
            self.data[address as usize] = value;
            Ok(())
        }
    }
}
