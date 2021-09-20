use crate::cpu::SegmentAndOffset;

#[derive(Debug)]
pub enum MemoryError {
    //NoInterface(u32),
}

pub trait MemoryInterface {
    fn read_u8(&self, so: SegmentAndOffset) -> u8;
    fn write_u8(&mut self, so: SegmentAndOffset, value: u8);
}

pub struct PhysicalMemory {
    pub data: Vec<u8>,
}

impl PhysicalMemory {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: vec![0; capacity],
        }
    }
}

impl MemoryInterface for PhysicalMemory {
    fn read_u8(&self, so: SegmentAndOffset) -> u8 {
        self.data[so as usize]
    }

    fn write_u8(&mut self, so: SegmentAndOffset, value: u8) {
        self.data[so as usize] = value;
    }
}

struct InterfaceContainer {
    start: SegmentAndOffset,
    size: u32,
    interface: Box<dyn MemoryInterface>,
}

pub struct MemoryManager {
    interfaces: Vec<InterfaceContainer>,
}

impl MemoryManager {
    pub fn new() -> Self {
        MemoryManager {
            interfaces: Vec::new(),
        }
    }

    pub fn map(&mut self, start: SegmentAndOffset, size: u32, interface: Box<dyn MemoryInterface>) {
        self.interfaces.push(InterfaceContainer {
            start: start.clone(),
            size,
            interface,
        })
    }

    pub fn read_u16(&self, so: SegmentAndOffset) -> u16 {
        let hi = self.read_u8(so) as u16;
        let lo = self.read_u8(so + 1) as u16;
        (hi << 8) | lo
    }

    pub fn write_u16(&mut self, so: SegmentAndOffset, value: u16) {
        self.write_u8(so, (value >> 8) as u8);
        self.write_u8(so + 1, value as u8);
    }
}

impl MemoryInterface for MemoryManager {
    fn read_u8(&self, so: SegmentAndOffset) -> u8 {
        for container in &self.interfaces {
            if so >= container.start && so < container.start + container.size {
                return container.interface.read_u8(so);
            }
        }
        panic!("No interface found for address {}", so);
    }

    fn write_u8(&mut self, so: SegmentAndOffset, value: u8) {
        for container in &mut self.interfaces {
            if so >= container.start && so < container.start + container.size {
                return container.interface.write_u8(so, value);
            }
        }
        panic!("No interface found for address {}", so);
    }
}
