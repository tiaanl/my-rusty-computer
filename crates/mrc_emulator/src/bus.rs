use crate::error::Result;
use std::cell::RefCell;
use std::rc::Rc;

pub type Address = u32;

pub fn segment_and_offset(segment: u16, offset: u16) -> Address {
    ((segment as Address) << 4) + (offset as Address)
}

/// An object where bytes can be read from or written to.
pub trait BusInterface {
    fn read(&self, address: Address) -> Result<u8>;
    fn write(&mut self, address: Address, value: u8) -> Result<()>;
}

pub struct InterfaceContainer {
    pub start_address: Address,
    pub size: u32,
    pub interface: Box<dyn BusInterface>,
}

impl InterfaceContainer {
    fn contains(&self, address: Address) -> bool {
        address >= self.start_address && address < self.start_address + self.size
    }
}

/// A bus where a CPU can read or write data to/from. Other components can map their address space
/// to the bus.
#[derive(Default)]
pub struct Bus {
    interfaces: Vec<InterfaceContainer>,
}

impl Bus {
    pub fn with_interfaces(interfaces: Vec<InterfaceContainer>) -> Self {
        Self { interfaces }
    }

    pub fn map(&mut self, start_address: Address, size: u32, interface: Box<dyn BusInterface>) {
        // TODO: Check for overlapping ranges.
        self.interfaces.push(InterfaceContainer {
            start_address,
            size,
            interface,
        });

        self.interfaces.sort_by(|left, right| {
            left.start_address
                .partial_cmp(&right.start_address)
                .unwrap()
        });
    }
}

impl BusInterface for Bus {
    fn read(&self, address: Address) -> Result<u8> {
        if let Some(container) = self
            .interfaces
            .iter()
            .find(|interface| interface.contains(address))
        {
            container.interface.read(address - container.start_address)
        } else {
            // Err(Error::AddressNotMapped(address))
            // log::warn!("Reading from unmapped memory: [{:05X}]", address);
            Ok(0)
        }
    }

    fn write(&mut self, address: Address, value: u8) -> Result<()> {
        if let Some(container) = self
            .interfaces
            .iter_mut()
            .find(|interface| interface.contains(address))
        {
            container
                .interface
                .as_mut()
                .write(address - container.start_address, value)
        } else {
            // Err(Error::AddressNotMapped(address))
            // log::warn!(
            //     "Writing to unmapped memory: {:02X} -> [{:05X}]",
            //     value,
            //     address
            // );
            Ok(())
        }
    }
}

pub struct WrappingBusInterface<T: BusInterface> {
    inner: Rc<RefCell<T>>,
}

impl<T: BusInterface> WrappingBusInterface<T> {
    pub fn new(inner: Rc<RefCell<T>>) -> Self {
        Self { inner }
    }
}

impl<T: BusInterface> BusInterface for WrappingBusInterface<T> {
    fn read(&self, address: Address) -> Result<u8> {
        self.inner.borrow().read(address)
    }

    fn write(&mut self, address: Address, value: u8) -> Result<()> {
        self.inner.borrow_mut().write(address, value)
    }
}

#[cfg(test)]
mod test {
    use super::super::error::Result;
    use super::*;
    use crate::ram::RandomAccessMemory;
    use std::cell::Cell;

    #[test]
    fn can_read_write_through_bus() {
        struct TestInterface {
            reads: Cell<isize>,
            writes: Cell<isize>,
        }

        impl TestInterface {
            pub fn new() -> Self {
                Self {
                    reads: Cell::new(0),
                    writes: Cell::new(0),
                }
            }
        }

        impl BusInterface for TestInterface {
            fn read(&self, _address: Address) -> Result<u8> {
                self.reads.set(self.reads.get() + 1);
                Ok(0)
            }

            fn write(&mut self, _address: Address, _value: u8) -> Result<()> {
                self.writes.set(self.writes.get() + 1);
                Ok(())
            }
        }

        let test_interface = Rc::new(RefCell::new(TestInterface::new()));

        let mut bus = Bus::default();
        bus.map(
            0x00000,
            0xFF,
            Box::new(WrappingBusInterface::new(test_interface.clone())),
        );

        bus.read(0x0).unwrap();
        bus.write(0x0, 0x0).unwrap();
        bus.read(0x1000).unwrap();
        bus.write(0x1000, 0x0).unwrap();

        assert_eq!(1, test_interface.borrow().reads.get());
        assert_eq!(1, test_interface.borrow().writes.get());
    }

    #[test]
    fn with_interfaces() {
        let interfaces = vec![InterfaceContainer {
            start_address: 0x0,
            size: 0x1,
            interface: Box::new(RandomAccessMemory::from_vec(vec![0x01])),
        }];
        let bus = Bus::with_interfaces(interfaces);

        assert_eq!(0x01, bus.read(0x0).unwrap());
    }
}
