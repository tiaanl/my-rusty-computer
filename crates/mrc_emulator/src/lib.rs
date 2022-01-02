pub mod components;
pub mod cpu;
pub mod error;

/// An object where bytes can be read from or written to.
pub trait Bus<Size> {
    fn read(&self, address: Size) -> error::Result<u8>;
    fn write(&mut self, address: Size, value: u8) -> error::Result<()>;
}

/// The type used for reading from a data bus.
pub type Address = u32;

pub fn segment_and_offset(segment: u16, offset: u16) -> Address {
    ((segment as Address) << 4) + (offset as Address)
}

/// The type used for reading from an I/O bus.
pub type Port = u16;
