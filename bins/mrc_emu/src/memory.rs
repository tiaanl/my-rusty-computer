use crate::cpu::SegmentAndOffset;

#[derive(Debug)]
pub enum MemoryError {
    NoInterface(usize),
}

pub trait MemoryInterface {
    fn read_u8(&self, so: SegmentAndOffset) -> Result<u8, MemoryError>;
    fn write_u8(&mut self, so: SegmentAndOffset, value: u8) -> Result<(), MemoryError>;
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
    fn read_u8(&self, so: SegmentAndOffset) -> Result<u8, MemoryError> {
        Ok(self.data[so])
    }

    fn write_u8(&mut self, _: SegmentAndOffset, _: u8) -> Result<(), MemoryError> {
        todo!()
    }
}

struct InterfaceContainer {
    start: SegmentAndOffset,
    size: usize,
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

    pub fn map(
        &mut self,
        start: SegmentAndOffset,
        size: usize,
        interface: Box<dyn MemoryInterface>,
    ) {
        self.interfaces.push(InterfaceContainer {
            start: start.clone(),
            size,
            interface,
        })
    }
}

impl MemoryInterface for MemoryManager {
    fn read_u8(&self, so: SegmentAndOffset) -> Result<u8, MemoryError> {
        for container in &self.interfaces {
            if so >= container.start && so < container.start + container.size {
                return container.interface.read_u8(so);
            }
        }
        Err(MemoryError::NoInterface(so))
    }

    fn write_u8(&mut self, _: SegmentAndOffset, _: u8) -> Result<(), MemoryError> {
        todo!()
    }
}
