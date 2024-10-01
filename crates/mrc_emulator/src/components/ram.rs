use tracing::warn;

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

impl Bus for RandomAccessMemory {
    fn read(&self, address: Address) -> u8 {
        let address = address as usize;
        if address >= self.data.len() {
            warn!("Reading outside of bounds! ({:05X})", address);
            0
        } else {
            self.data[address]
        }
    }

    fn write(&mut self, address: Address, value: u8) {
        let address = address as usize;
        if address >= self.data.len() {
            warn!("Writing outside of bounds! ({:05X})", address);
        } else {
            self.data[address as usize] = value;
        }
    }
}

// impl Bus for RandomAccessMemory {
//     fn read(&self, address: Address) -> u8 {
//         let address = address as usize;
//         if address >= self.data.len() {
//             warn!("Reading outside of bounds! ({:05X})", address);
//             0
//         } else {
//             self.data[address]
//         }
//     }

//     fn write(&mut self, address: Address, value: u8) {
//         let address = address as usize;
//         if address >= self.data.len() {
//             warn!("Writing outside of bounds! ({:05X})", address);
//         } else {
//             self.data[address as usize] = value;
//         }
//     }
// }
