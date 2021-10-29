mod arithmetic;
mod logic;

pub trait SignificantBit {
    fn least_significant_bit(&self) -> bool;
    fn most_significant_bit(&self) -> bool;
}

macro_rules! significant_bit {
    ($t:ident) => {
        impl SignificantBit for $t {
            fn least_significant_bit(&self) -> bool {
                self & 0x1 != 0
            }

            fn most_significant_bit(&self) -> bool {
                self & (0x1 << (std::mem::size_of::<$t>() * 8) - 1) != 0
            }
        }
    };
}

significant_bit!(u8);
significant_bit!(u16);

#[cfg(test)]
mod test {
    use crate::cpu::Flags;

    pub fn test_flags(
        flags: &Flags,
        carry: bool,
        parity: bool,
        aux_carry: bool,
        zero: bool,
        sign: bool,
        overflow: bool,
    ) {
        assert_eq!(carry, flags.contains(Flags::CARRY), "CARRY");
        assert_eq!(parity, flags.contains(Flags::PARITY), "PARITY");
        assert_eq!(aux_carry, flags.contains(Flags::AUX_CARRY), "AUX_CARRY");
        assert_eq!(zero, flags.contains(Flags::ZERO), "ZERO");
        assert_eq!(sign, flags.contains(Flags::SIGN), "SIGN");
        assert_eq!(overflow, flags.contains(Flags::OVERFLOW), "OVERFLOW");
    }
}

const PARITY_TABLE: [u8; 0x100] = [
    1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
    0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
    0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
    1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
    0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
    1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
    1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
    0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
];

pub(crate) fn parity_for(value: u8) -> bool {
    PARITY_TABLE[value as usize] == 1
}

pub mod byte {
    use super::SignificantBit;
    use crate::cpu::Flags;

    pub fn flags_from_result(flags: &mut Flags, result: u8) {
        flags.set(Flags::ZERO, result == 0);
        flags.set(Flags::SIGN, result.most_significant_bit());
        flags.set(Flags::PARITY, super::parity_for(result));
    }

    pub use super::arithmetic::byte::{add, add_with_carry, compare, multiply, subtract};
    pub use super::logic::byte::{
        and, exclusive_or, not, or, rotate_left, rotate_right, shift_left, shift_right, test,
    };
}

pub mod word {
    use super::SignificantBit;
    use crate::cpu::Flags;

    pub fn flags_from_result(flags: &mut Flags, result: u16) {
        flags.set(Flags::ZERO, result == 0);
        flags.set(Flags::SIGN, result.most_significant_bit());
        flags.set(Flags::PARITY, super::parity_for(result as u8));
    }

    pub use super::arithmetic::word::{add, add_with_carry, compare, multiply, subtract};
    pub use super::logic::word::{
        and, exclusive_or, not, or, rotate_left, rotate_right, shift_left, shift_right, test,
    };
}
