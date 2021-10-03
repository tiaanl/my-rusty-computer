use super::{flags_from_byte_result, flags_from_word_result};
use crate::cpu::{Flags, SignificantBit};

pub fn shift_right_byte(value: u8, by: u8, flags: &mut Flags) -> u8 {
    flags.set(Flags::OVERFLOW, by == 1 && value.most_significant_bit());

    let mut result = value;
    for _ in 0..by {
        flags.set(Flags::CARRY, result.least_significant_bit());
        result >>= 1;
    }

    flags_from_byte_result(flags, result);

    result
}

pub fn shift_right_word(value: u16, by: u16, flags: &mut Flags) -> u16 {
    flags.set(Flags::OVERFLOW, by == 1 && value.most_significant_bit());

    let mut result = value;
    for _ in 0..by {
        flags.set(Flags::CARRY, result.least_significant_bit());
        result >>= 1;
    }

    flags_from_word_result(flags, result);

    result
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_shift_right_byte() {
        let mut flags = Flags::empty();
        let result = shift_right_byte(0xFF, 0x01, &mut flags);
        assert_eq!(0x7F, result);
        assert!(flags.contains(Flags::CARRY));
        assert!(flags.contains(Flags::OVERFLOW));
        assert!(!flags.contains(Flags::ZERO));
        assert!(!flags.contains(Flags::SIGN));
        assert!(!flags.contains(Flags::PARITY));
    }

    #[test]
    fn test_shift_right_word() {
        let mut flags = Flags::empty();
        let result = shift_right_word(0xFFFF, 0x0001, &mut flags);
        assert_eq!(0x7FFF, result);
        assert!(flags.contains(Flags::CARRY));
        assert!(flags.contains(Flags::OVERFLOW));
        assert!(!flags.contains(Flags::ZERO));
        assert!(!flags.contains(Flags::SIGN));
        assert!(flags.contains(Flags::PARITY));
    }
}
