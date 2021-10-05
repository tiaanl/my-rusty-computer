use crate::cpu::operations::{flags_from_byte_result, flags_from_word_result};
use crate::cpu::{Flags, SignificantBit};

pub fn shift_left_byte(value: u8, by: u8, flags: &mut Flags) -> Option<u8> {
    let result = value.wrapping_shl(by.into());

    flags.set(
        Flags::CARRY,
        (value as u16).wrapping_shl(by.into()) & 0xFF00 > 0,
    );
    flags.set(
        Flags::OVERFLOW,
        result.most_significant_bit() != value.most_significant_bit(),
    );

    flags_from_byte_result(flags, result);

    Some(result)
}

pub fn shift_left_word(value: u16, by: u16, flags: &mut Flags) -> Option<u16> {
    let result = value.wrapping_shl(by.into());

    flags.set(
        Flags::CARRY,
        (value as u32).wrapping_shl(by.into()) & 0xFFFF0000 > 0,
    );
    flags.set(
        Flags::OVERFLOW,
        result.most_significant_bit() != value.most_significant_bit(),
    );

    flags_from_word_result(flags, result);

    Some(result)
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_flags(
        flags: &Flags,
        carry: bool,
        parity: bool,
        aux_carry: bool,
        zero: bool,
        sign: bool,
        overflow: bool,
    ) {
        assert_eq!(carry, flags.contains(Flags::CARRY));
        assert_eq!(parity, flags.contains(Flags::PARITY));
        assert_eq!(aux_carry, flags.contains(Flags::AUX_CARRY));
        assert_eq!(zero, flags.contains(Flags::ZERO));
        assert_eq!(sign, flags.contains(Flags::SIGN));
        assert_eq!(overflow, flags.contains(Flags::OVERFLOW));
    }

    #[test]
    fn test_shift_left_byte() {
        let mut flags = Flags::empty();
        let result = shift_left_byte(0x01, 0x01, &mut flags);
        assert_eq!(0x02, result);
        test_flags(&flags, false, false, false, false, false, false);
    }

    #[test]
    fn test_shift_left_byte_with_carry() {
        let mut flags = Flags::empty();
        let result = shift_left_byte(0xFF, 0x01, &mut flags);
        assert_eq!(0xFE, result);
        test_flags(&flags, true, false, false, false, true, false);
    }

    #[test]
    fn test_shift_left_byte_with_overflow() {
        let mut flags = Flags::empty();
        let result = shift_left_byte(0x40, 0x01, &mut flags);
        assert_eq!(0x80, result);
        test_flags(&flags, false, false, false, false, true, true);
    }

    #[test]
    fn test_shift_left_word() {
        let mut flags = Flags::empty();
        let result = shift_left_word(0x0001, 0x0001, &mut flags);
        assert_eq!(0x0002, result);
        test_flags(&flags, false, false, false, false, false, false);
    }

    #[test]
    fn test_shift_left_word_with_carry() {
        let mut flags = Flags::empty();
        let result = shift_left_word(0xFFFF, 0x0001, &mut flags);
        assert_eq!(0xFFFE, result);
        test_flags(&flags, true, false, false, false, true, false);
    }

    #[test]
    fn test_shift_left_word_with_overflow() {
        let mut flags = Flags::empty();
        let result = shift_left_word(0x7FFF, 0x01, &mut flags);
        assert_eq!(0xFFFE, result);
        test_flags(&flags, false, false, false, false, true, true);
    }
}
