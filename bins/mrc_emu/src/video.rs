/*
use crate::memory::MemoryInterface;

pub struct VideoMemory {
    data: Vec<u8>,
}

impl Default for VideoMemory {
    fn default() -> Self {
        // 32KiB memory for VGA ROM.
        Self {
            data: vec![0; 0x8000],
        }
    }
}

impl MemoryInterface for VideoMemory {
    fn read(&self, so: SegmentAndOffset) -> u8 {
        self.data[so as usize]
    }

    fn write(&mut self, _so: SegmentAndOffset, _value: u8) {
        todo!()
    }
}
*/