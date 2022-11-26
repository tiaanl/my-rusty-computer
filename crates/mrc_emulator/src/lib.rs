pub mod components;
pub mod cpu;
pub mod cpu2;
pub mod error;

/// An object where bytes can be read from or written to.
pub trait Bus<Size> {
    fn read(&self, address: Size) -> u8;
    fn write(&mut self, address: Size, value: u8);
}

/// The type used for reading from a data bus.
pub type Address = u32;

pub fn segment_and_offset(segment: u16, offset: u16) -> Address {
    ((segment as Address) << 4) + (offset as Address)
}

/// The type used for reading from an I/O bus.
pub type Port = u16;

/// Interface for a central processing unit.
pub trait Cpu {
    /// Reset the CPU to it's initial state.
    fn reset(&mut self);

    /// Cycle the CPU by the amount of clock cycles given.
    fn cycle(&mut self, cycles: usize);
}
