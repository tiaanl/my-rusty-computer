use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub type Address = u32;

pub fn segment_and_offset(segment: u16, offset: u16) -> Address {
    ((segment as Address) << 4) + (offset as Address)
}

pub enum BusError {
    AddressNotMapped(Address),
    AddressOutOfRange(Address),
}

impl Display for BusError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BusError::AddressNotMapped(address) => {
                write!(f, "Address not mapped. ({:02x})", address)
            }
            BusError::AddressOutOfRange(address) => {
                write!(f, "Address out of range. ({:02x})", address)
            }
        }
    }
}

/// An object where bytes can be read from or written to.
pub trait BusInterface {
    fn read(&self, address: Address) -> Result<u8, BusError>;
    fn write(&mut self, address: Address, value: u8) -> Result<(), BusError>;
}

struct InterfaceContainer {
    start_address: Address,
    size: u32,
    interface: Rc<RefCell<dyn BusInterface>>,
}

impl InterfaceContainer {
    fn contains(&self, address: Address) -> bool {
        address >= self.start_address && address < self.start_address + self.size
    }
}

/// A bus where a CPU can read or write data to/from. Other components can map their address space
/// to the bus.
pub struct Bus {
    interfaces: Vec<InterfaceContainer>,
}

impl Default for Bus {
    fn default() -> Self {
        Self {
            interfaces: vec!(),
        }
    }
}

impl Bus {
    pub fn map(&mut self, start_address: Address, size: u32, interface: Rc<RefCell<dyn BusInterface>>) {
        // TODO: Check for overlapping ranges.
        self.interfaces.push(InterfaceContainer {
            start_address,
            size,
            interface,
        });

        self.interfaces.sort_by(|left, right| left.start_address.partial_cmp(&right.start_address).unwrap());
    }
}

impl BusInterface for Bus {
    fn read(&self, address: Address) -> Result<u8, BusError> {
        if let Some(container) = self.interfaces.iter().find(|interface| interface.contains(address)) {
            container.interface.borrow().read(address)
        } else {
            Err(BusError::AddressNotMapped(address))
        }
    }

    fn write(&mut self, address: Address, value: u8) -> Result<(), BusError> {
        if let Some(container) = self.interfaces.iter().find(|interface| interface.contains(address)) {
            container.interface.borrow_mut().write(address, value)
        } else {
            Err(BusError::AddressNotMapped(address))
        }
    }
}
