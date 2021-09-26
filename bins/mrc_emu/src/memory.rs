use crate::cpu::SegmentAndOffset;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub enum MemoryError {
    //NoInterface(u32),
}

pub trait MemoryInterface {
    fn read(&self, so: SegmentAndOffset) -> u8;
    fn write(&mut self, so: SegmentAndOffset, value: u8);
}

pub type MemoryInterfaceRef = Rc<RefCell<dyn MemoryInterface>>;

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
    fn read(&self, so: SegmentAndOffset) -> u8 {
        self.data[so as usize]
    }

    fn write(&mut self, so: SegmentAndOffset, value: u8) {
        self.data[so as usize] = value;
    }
}

struct InterfaceContainer {
    start: SegmentAndOffset,
    size: u32,
    interface: MemoryInterfaceRef,
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

    pub fn map(&mut self, start: SegmentAndOffset, size: u32, interface: MemoryInterfaceRef) {
        self.interfaces.push(InterfaceContainer {
            start,
            size,
            interface,
        })
    }

    pub fn read_u8(&self, so: SegmentAndOffset) -> u8 {
        for container in &self.interfaces {
            if so >= container.start && so < container.start + container.size {
                return container.interface.borrow().read(so);
            }
        }
        panic!("No interface found for address {}", so);
    }

    pub fn read_u16(&self, so: SegmentAndOffset) -> u16 {
        let hi = self.read_u8(so);
        let lo = self.read_u8(so + 1);
        u16::from_le_bytes([hi, lo])
    }

    pub fn write_u8(&mut self, so: SegmentAndOffset, value: u8) {
        for container in &mut self.interfaces {
            if so >= container.start && so < container.start + container.size {
                return container.interface.borrow_mut().write(so, value);
            }
        }
        panic!("No interface found for address {}", so);
    }

    pub fn write_u16(&mut self, so: SegmentAndOffset, value: u16) {
        let bytes: [u8; 2] = value.to_le_bytes();
        self.write_u8(so, bytes[0]);
        self.write_u8(so + 1, bytes[1]);
    }
}
