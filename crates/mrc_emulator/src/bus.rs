use crate::error::Result;
use std::cell::RefCell;
use std::ops::Range;
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
    pub range: Range<Address>,
    pub interface: Box<dyn BusInterface>,
}

impl InterfaceContainer {
    fn contains(&self, address: Address) -> bool {
        self.range.contains(&address)
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

    pub fn map(&mut self, range: Range<Address>, interface: impl BusInterface + 'static) {
        self.interfaces.push(InterfaceContainer {
            range,
            interface: Box::new(interface),
        })
    }
}

impl BusInterface for Bus {
    fn read(&self, address: Address) -> Result<u8> {
        if let Some(container) = self
            .interfaces
            .iter()
            .find(|interface| interface.contains(address))
        {
            container.interface.read(address - container.range.start)
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
                .write(address - container.range.start, value)
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
    use crate::error::Result;
    use super::*;

    #[test]
    fn can_read_write_through_bus() {
        #[derive(Default)]
        struct TestState {
            reads: isize,
            writes: isize,
        }

        struct TestInterface {
            state: Rc<RefCell<TestState>>,
        }

        impl TestInterface {
            pub fn new(state: Rc<RefCell<TestState>>) -> Self {
                Self { state }
            }
        }

        impl BusInterface for TestInterface {
            fn read(&self, _address: Address) -> Result<u8> {
                self.state.borrow_mut().reads += 1;
                Ok(0)
            }

            fn write(&mut self, _address: Address, _value: u8) -> Result<()> {
                self.state.borrow_mut().writes += 1;
                Ok(())
            }
        }

        let mut bus = Bus::default();

        let state = Rc::new(RefCell::new(TestState::default()));
        let test_interface = TestInterface::new(state.clone());
        bus.map(0x00..0xFF, test_interface);

        bus.read(0x0).unwrap();
        bus.write(0x0, 0x0).unwrap();
        bus.read(0x1000).unwrap();
        bus.write(0x1000, 0x0).unwrap();

        assert_eq!(1, state.borrow().reads);
        assert_eq!(1, state.borrow().writes);
    }

    /*
    #[test]
    fn with_interfaces() {
        let interfaces = vec![InterfaceContainer {
            range: 0x0..0x1,
            interface: Box::new(RandomAccessMemory::from_vec(vec![0x01])),
        }];
        let bus = Bus::with_interfaces(interfaces);

        assert_eq!(0x01, bus.read(0x0).unwrap());
    }
    */
}
