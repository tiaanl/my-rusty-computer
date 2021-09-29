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

pub struct MemoryReader<M: MemoryInterface> {
    interface: Rc<RefCell<M>>,
}

impl<M: MemoryInterface> MemoryReader<M> {
    pub fn new(interface: Rc<RefCell<M>>) -> Self {
        Self { interface }
    }

    pub fn interface(&self) -> Rc<RefCell<M>> {
        Rc::clone(&self.interface)
    }

    pub fn read_u8(&self, so: SegmentAndOffset) -> u8 {
        self.interface.borrow().read(so)
    }

    pub fn read_u16(&self, so: SegmentAndOffset) -> u16 {
        u16::from_le_bytes([
            self.interface.borrow().read(so),
            self.interface.borrow().read(so + 1),
        ])
    }

    pub fn write_u8(&mut self, so: SegmentAndOffset, value: u8) {
        self.interface.borrow_mut().write(so, value);
    }

    pub fn write_u16(&mut self, so: SegmentAndOffset, value: u16) {
        let bytes: [u8; 2] = value.to_le_bytes();
        self.interface.borrow_mut().write(so, bytes[0]);
        self.interface.borrow_mut().write(so + 1, bytes[1]);
    }
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

pub struct ReadOnlyMemory {
    data: Vec<u8>,
}

impl ReadOnlyMemory {
    pub fn from_vec(data: Vec<u8>) -> Self {
        Self { data }
    }
}

impl MemoryInterface for ReadOnlyMemory {
    fn read(&self, so: SegmentAndOffset) -> u8 {
        match self.data.get(so as usize) {
            None => panic!("Address out of range. ({})", so),
            Some(&byte) => byte,
        }
    }

    fn write(&mut self, _: SegmentAndOffset, _: u8) {
        panic!("Can't write to ROM's");
    }
}

struct InterfaceContainer {
    start: SegmentAndOffset,
    size: u32,
    interface: MemoryInterfaceRef,
}

pub struct MemoryMapper {
    interfaces: Vec<InterfaceContainer>,
}

impl MemoryMapper {
    pub fn new() -> Self {
        MemoryMapper {
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
}

impl MemoryInterface for MemoryMapper {
    fn read(&self, so: SegmentAndOffset) -> u8 {
        for container in &self.interfaces {
            if so >= container.start && so < container.start + container.size {
                return container.interface.borrow().read(so - container.start);
            }
        }
        panic!("No interface found for address {:05X}", so);
    }

    fn write(&mut self, so: SegmentAndOffset, value: u8) {
        for container in &mut self.interfaces {
            if so >= container.start && so < container.start + container.size {
                return container.interface.borrow_mut().write(so, value);
            }
        }
        panic!("No interface found for address {}", so);
    }
}
