/*
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub enum MemoryError {
    //NoInterface(u32),
}

pub trait MemoryInterface {
    fn read(&self, so: Address) -> u8;
    fn write(&mut self, so: Address, value: u8);
}

pub struct Bus<M: MemoryInterface> {
    interface: Rc<RefCell<M>>,
}

impl<M: MemoryInterface> Bus<M> {
    pub fn new(interface: Rc<RefCell<M>>) -> Self {
        Self { interface }
    }

    pub fn interface(&self) -> Rc<RefCell<M>> {
        Rc::clone(&self.interface)
    }

    pub fn read_u8(&self, so: Address) -> u8 {
        self.interface.borrow().read(so)
    }

    pub fn read_u16(&self, so: Address) -> u16 {
        u16::from_le_bytes([
            self.interface.borrow().read(so),
            self.interface.borrow().read(so + 1),
        ])
    }

    pub fn write_u8(&mut self, so: Address, value: u8) {
        self.interface.borrow_mut().write(so, value);
    }

    pub fn write_u16(&mut self, so: Address, value: u16) {
        let bytes: [u8; 2] = value.to_le_bytes();
        self.interface.borrow_mut().write(so, bytes[0]);
        self.interface.borrow_mut().write(so + 1, bytes[1]);
    }
}

pub type MemoryInterfaceRef = Rc<RefCell<dyn MemoryInterface>>;

struct InterfaceContainer {
    start: Address,
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

    pub fn map(&mut self, start: Address, size: u32, interface: MemoryInterfaceRef) {
        self.interfaces.push(InterfaceContainer {
            start,
            size,
            interface,
        })
    }
}

impl MemoryInterface for MemoryMapper {
    fn read(&self, so: Address) -> u8 {
        for container in &self.interfaces {
            if so >= container.start && so < container.start + container.size {
                return container.interface.borrow().read(so - container.start);
            }
        }
        panic!("No interface found for address {:05X}", so);
    }

    fn write(&mut self, so: Address, value: u8) {
        for container in &mut self.interfaces {
            if so >= container.start && so < container.start + container.size {
                return container
                    .interface
                    .borrow_mut()
                    .write(so - container.start, value);
            }
        }
        panic!("No interface found for address {:05X}", so);
    }
}

#[cfg(test)]
mod test {
    use crate::cpu::segment_and_offset;

    use super::*;

    #[test]
    fn test_memory_mapping() {
        let mut pm = PhysicalMemory::with_capacity(0x100); // 256 bytes
        for i in 0..0x100 {
            pm.data[i as usize] = i as u8;
        }

        let mut mm = MemoryMapper::new();

        mm.map(
            segment_and_offset(0x000, 0x000),
            0x100,
            Rc::new(RefCell::new(pm)),
        );

        assert_eq!(0x00, mm.read(segment_and_offset(0x0000, 0x0000)));
        assert_eq!(0x7F, mm.read(segment_and_offset(0x0000, 0x007F)));
        assert_eq!(0xFF, mm.read(segment_and_offset(0x0000, 0x00FF)));
    }
}
*/
