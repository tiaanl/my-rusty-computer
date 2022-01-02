/// Represents a location in a segmented memory model.  The address stores the values in 16-bit
/// segment and offset values, but can also be represented as a 20-bit flat address.
///
/// ```rust
/// let reset_vector = Address::new(0xFFFF, 0x0000);
/// assert_eq!(address.flat(), 0xFFFF0);
/// ```
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Address {
    /// 16-bit segment part of the address.
    pub segment: u16,
    /// 16-bit offset part of the address.
    pub offset: u16,
}

impl Address {
    /// Create a new segment/offset pair.
    pub fn new(segment: u16, offset: u16) -> Self {
        Self { segment, offset }
    }

    /// Return the 20-bit address value for flat memory access.
    pub fn flat(&self) -> u32 {
        ((self.segment as u32) << 4) + (self.offset as u32)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flat() {
        let a = Address::new(0x1000, 0x0010);
        assert_eq!(a.flat(), 0x10010);
    }
}
