/// Represents a location in a segmented memory model.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Address {
    segment: u16,
    offset: u16,
}

impl Address {
    /// Create a new segment/offset pair.
    pub fn new(segment: u16, offset: u16) -> Self {
        Self { segment, offset }
    }

    /// Return the 20-bit address value for linear memory access.
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
