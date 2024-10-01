pub mod components;
pub mod cpu;
pub mod cpu2;
pub mod error;

/// Specified the address used to read/write to/from a bus.
pub type Address = u32;

/// An object where bytes can be read from or written to.
pub trait Bus {
    fn read(&self, address: Address) -> u8;
    fn write(&mut self, address: Address, value: u8);
}

pub fn segment_and_offset(segment: u16, offset: u16) -> Address {
    ((segment as Address) << 4) + (offset as Address)
}

/// Interface for a central processing unit.
pub trait Cpu {
    /// Reset the CPU to it's initial state.
    fn reset(&mut self);

    /// Cycle the CPU by the amount of clock cycles given.
    fn cycle(&mut self, cycles: usize);
}
