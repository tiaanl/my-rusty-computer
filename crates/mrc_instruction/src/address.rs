use crate::Displacement;
use std::fmt::{Display, Formatter};

/// Represents a location in a segmented memory model.  The address stores the values in 16-bit
/// segment and offset values, but can also be represented as a 20-bit flat address.
///
/// ```rust
/// use mrc_instruction::Address;
/// let reset_vector = Address::new(0xFFFF, 0x0000);
/// assert_eq!(reset_vector.flat(), 0xFFFF0);
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

    /// Displace the offset within the same segment.
    pub fn displace(&self, displacement: Displacement) -> Self {
        let displacement = match displacement {
            Displacement::None => 0,
            Displacement::Byte(displacement) => displacement.into(),
            Displacement::Word(displacement) => displacement,
        };

        let offset = if displacement < 0 {
            if let Some(neg) = displacement.checked_neg() {
                self.offset.wrapping_sub(neg as u16)
            } else {
                self.offset.wrapping_sub(128)
            }
        } else {
            self.offset.wrapping_add(displacement as u16)
        };

        Self {
            segment: self.segment,
            offset,
        }
    }
}

impl Display for Address {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04X}:{:04X}", self.segment, self.offset)
    }
}

pub trait RelativeToAddress {
    fn relative_to(&self, addr: &Address) -> Address;
}

impl RelativeToAddress for u32 {
    fn relative_to(&self, addr: &Address) -> Address {
        let offset = (self & !((addr.segment as u32) << 4)) + addr.offset as u32;
        Address::new(addr.segment, offset as u16)
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
