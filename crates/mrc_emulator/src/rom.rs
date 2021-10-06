use crate::bus::{Address, BusError, BusInterface};

pub struct ReadOnlyMemory {
    data: Vec<u8>,
}

impl ReadOnlyMemory {
    pub fn from_vec(vec: Vec<u8>) -> Self {
        Self { data: vec }
    }
}

impl BusInterface for ReadOnlyMemory {
    fn read(&self, address: Address) -> Result<u8, BusError> {
        if address as usize >= self.data.len() {
            Err(BusError::AddressNotMapped(address))
        } else {
            Ok(self.data[address as usize])
        }
    }

    fn write(&mut self, address: Address, value: u8) -> Result<(), BusError> {
        log::warn!("Writing {:#02x} to ROM at {:05x}", value, address);
        Ok(())
    }
}
