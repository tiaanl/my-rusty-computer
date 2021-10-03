use crate::cpu::operations::{flags_from_byte_result, flags_from_word_result};
use crate::cpu::{Flags, SignificantBit};

pub fn shift_left_byte(value: u8, by: u8, flags: &mut Flags) -> u8 {
    let mut result = value;

    let mut count = by;
    while count != 0 {
        flags.set(Flags::CARRY, result.most_significant_bit());
        result <<= 1;
        count -= 1;
    }

    if by == 1 {
        flags.set(
            Flags::OVERFLOW,
            result.most_significant_bit() ^ flags.contains(Flags::CARRY),
        );
    }

    flags_from_byte_result(flags, result);

    result
}

pub fn shift_left_word(value: u16, by: u16, flags: &mut Flags) -> u16 {
    let mut result = value;

    let mut count = by;
    while count != 0 {
        flags.set(Flags::CARRY, result.most_significant_bit());
        result <<= 1;
        count -= 1;
    }

    if by == 1 {
        flags.set(
            Flags::OVERFLOW,
            result.most_significant_bit() ^ flags.contains(Flags::CARRY),
        );
    }

    flags_from_word_result(flags, result);

    result
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_shift_left_byte() {
        let mut flags = Flags::empty();
        let result = shift_left_byte(0x01, 0x01, &mut flags);
        assert_eq!(0x02, result);
        assert!(!flags.contains(Flags::CARRY));
        assert!(!flags.contains(Flags::SIGN));
        assert!(!flags.contains(Flags::OVERFLOW));
        assert!(!flags.contains(Flags::PARITY));
    }

    #[test]
    fn test_shift_left_with_carry_byte() {
        let mut flags = Flags::empty();
        let result = shift_left_byte(0xFF, 0x01, &mut flags);
        assert_eq!(0xFE, result);
        assert!(flags.contains(Flags::CARRY));
        assert!(!flags.contains(Flags::OVERFLOW));
    }

    #[test]
    fn test_shift_left_with_overflow_byte() {
        let mut flags = Flags::empty();
        let result = shift_left_byte(0x7F, 0x01, &mut flags);
        assert_eq!(0xFE, result);
        assert!(flags.contains(Flags::CARRY));
        assert!(flags.contains(Flags::OVERFLOW));
    }

    #[test]
    fn test_shift_left_word() {
        let mut flags = Flags::empty();
        let result = shift_left_word(0x0001, 0x0001, &mut flags);
        assert_eq!(0x0002, result);
        assert!(!flags.contains(Flags::CARRY));
        assert!(!flags.contains(Flags::OVERFLOW));
    }

    #[test]
    fn test_shift_left_with_carry_word() {
        let mut flags = Flags::empty();
        let result = shift_left_word(0xFFFF, 0x0001, &mut flags);
        assert_eq!(0xFFFE, result);
        assert!(flags.contains(Flags::CARRY));
        assert!(!flags.contains(Flags::OVERFLOW));
    }

    #[test]
    fn test_shift_left_with_sign_word() {
        let mut flags = Flags::empty();
        let result = shift_left_word(0x7FFF, 0x0001, &mut flags);
        assert_eq!(0xFFFE, result);
        assert!(!flags.contains(Flags::CARRY));
        assert!(flags.contains(Flags::OVERFLOW));
    }

    #[test]
    fn test_shift_left_with_overflow_word() {
        let mut flags = Flags::empty();
        let result = shift_left_word(0x7FFF, 0x01, &mut flags);
        assert_eq!(0xFFFE, result);
        assert!(!flags.contains(Flags::CARRY));
        assert!(flags.contains(Flags::OVERFLOW));
    }
}
