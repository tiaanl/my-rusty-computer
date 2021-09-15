use crate::cpu::SegmentAndOffset;

enum MemoryError {
    OutsideBounds,
    ReadError(std::io::Error),
    NoInterface,
}

pub trait MemoryInterface {
    fn read_u8(&self, so: SegmentAndOffset) -> Result<u8, MemoryError>;
    fn read_u16(&self, so: SegmentAndOffset) -> Result<u16, MemoryError>;
    fn write_u8(&mut self, so: SegmentAndOffset, value: u8) -> Result<(), MemoryError>;
    fn write_u16(&mut self, so: SegmentAndOffset, value: u16) -> Result<(), MemoryError>;
}

pub struct PhysicalMemory {
    data: Vec<u8>,
}

impl PhysicalMemory {
    pub fn new() -> Self {
        Self {
            data: Vec::with_capacity(0xFFFFF),
        }
    }
}

impl MemoryInterface for PhysicalMemory {
    fn read_u8(&self, so: SegmentAndOffset) -> Result<u8, MemoryError> {
        let (_, buf) = self.data.split_at(so.into());
        Ok(buf[0])
    }

    fn read_u16(&self, so: SegmentAndOffset) -> Result<u16, MemoryError> {
        let mut buf = [0u8; 2];
        if let Some(byte) = self.data.get(so) {
            buf[0] = *byte;
        }
        if let Some(byte) = self.data.get(so + 1) {
            buf[0] = *byte;
        }
        Ok(u16::from_be_bytes(buf))
    }

    fn write_u8(&mut self, so: SegmentAndOffset, value: u8) -> Result<(), MemoryError> {
        todo!()
    }

    fn write_u16(&mut self, so: SegmentAndOffset, value: u16) -> Result<(), MemoryError> {
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
            if so >= container.start && so < container.start + container.start {
                return container.interface.read_u8(so);
            }
        }
        Err(MemoryError::NoInterface)
    }

    fn read_u16(&self, so: SegmentAndOffset) -> Result<u16, MemoryError> {
        todo!()
    }

    fn write_u8(&mut self, so: SegmentAndOffset, value: u8) -> Result<(), MemoryError> {
        todo!()
    }

    fn write_u16(&mut self, so: SegmentAndOffset, value: u16) -> Result<(), MemoryError> {
        todo!()
    }
}
