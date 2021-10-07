use crate::cpu::Flags;

use super::{flags_from_byte_result, flags_from_word_result, SignificantBit};

pub fn and_byte(destination: u8, source: u8, flags: &mut Flags) -> Option<u8> {
    flags.remove(Flags::OVERFLOW | Flags::CARRY);
    let result = destination & source;
    flags_from_byte_result(flags, result);
    Some(result)
}

pub fn and_word(destination: u16, source: u16, flags: &mut Flags) -> Option<u16> {
    flags.remove(Flags::OVERFLOW | Flags::CARRY);
    let result = destination & source;
    flags_from_word_result(flags, result);
    Some(result)
}

pub fn or_byte(destination: u8, source: u8, flags: &mut Flags) -> Option<u8> {
    flags.remove(Flags::OVERFLOW | Flags::CARRY);
    let result = destination | source;
    flags_from_byte_result(flags, result);
    Some(result)
}

pub fn or_word(destination: u16, source: u16, flags: &mut Flags) -> Option<u16> {
    flags.remove(Flags::OVERFLOW | Flags::CARRY);
    let result = destination | source;
    flags_from_word_result(flags, result);
    Some(result)
}

pub fn rol_byte(left: u8, right: u8, flags: &mut Flags) -> Option<u8> {
    let right = right % 8;
    let res = left << right | left >> (8 - right);

    flags.set(Flags::CARRY, res & 1 == 1);
    flags.set(Flags::OVERFLOW, left & 1 << 7 != res & 1 << 7);

    Some(res)
}

pub fn rol_word(left: u16, right: u16, flags: &mut Flags) -> Option<u16> {
    let right = right % 16;
    let res = left << right | left >> (16 - right);

    flags.set(Flags::CARRY, res & 1 == 1);
    flags.set(Flags::OVERFLOW, left & 1 << 15 != res & 1 << 15);

    Some(res)
}

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

pub fn shift_right_byte(value: u8, by: u8, flags: &mut Flags) -> Option<u8> {
    flags.set(Flags::OVERFLOW, by == 1 && value.most_significant_bit());

    let mut result = value;
    for _ in 0..by {
        flags.set(Flags::CARRY, result.least_significant_bit());
        result >>= 1;
    }

    flags_from_byte_result(flags, result);

    Some(result)
}

pub fn shift_right_word(value: u16, by: u16, flags: &mut Flags) -> Option<u16> {
    flags.set(Flags::OVERFLOW, by == 1 && value.most_significant_bit());

    let mut result = value;
    for _ in 0..by {
        flags.set(Flags::CARRY, result.least_significant_bit());
        result >>= 1;
    }

    flags_from_word_result(flags, result);

    Some(result)
}

pub fn test_byte(left: u8, right: u8, flags: &mut Flags) -> Option<u8> {
    let result = left & right;

    flags.set(Flags::SIGN, result.most_significant_bit());
    flags.remove(Flags::CARRY | Flags::OVERFLOW);

    super::flags_from_byte_result(flags, result);

    None
}

pub fn test_word(left: u16, right: u16, flags: &mut Flags) -> Option<u16> {
    let result = left & right;

    flags.set(Flags::SIGN, result.most_significant_bit());
    flags.remove(Flags::CARRY | Flags::OVERFLOW);

    super::flags_from_word_result(flags, result);

    None
}

pub fn xor_byte(destination: u8, source: u8, flags: &mut Flags) -> Option<u8> {
    flags.remove(Flags::OVERFLOW | Flags::CARRY);
    let result = destination ^ source;
    flags_from_byte_result(flags, result);
    Some(result)
}

pub fn xor_word(destination: u16, source: u16, flags: &mut Flags) -> Option<u16> {
    flags.remove(Flags::OVERFLOW | Flags::CARRY);
    let result = destination ^ source;
    flags_from_word_result(flags, result);
    Some(result)
}

#[cfg(test)]
mod test {
    use super::*;
    use super::super::test_flags;

    #[test]
    fn test_shift_left_byte() {
        let mut flags = Flags::empty();
        let result = shift_left_byte(0x01, 0x01, &mut flags).unwrap();
        assert_eq!(0x02, result);
        test_flags(&flags, false, false, false, false, false, false);
    }

    #[test]
    fn test_shift_left_byte_with_carry() {
        let mut flags = Flags::empty();
        let result = shift_left_byte(0xFF, 0x01, &mut flags).unwrap();
        assert_eq!(0xFE, result);
        test_flags(&flags, true, false, false, false, true, false);
    }

    #[test]
    fn test_shift_left_byte_with_overflow() {
        let mut flags = Flags::empty();
        let result = shift_left_byte(0x40, 0x01, &mut flags).unwrap();
        assert_eq!(0x80, result);
        test_flags(&flags, false, false, false, false, true, true);
    }

    #[test]
    fn test_shift_left_word() {
        let mut flags = Flags::empty();
        let result = shift_left_word(0x0001, 0x0001, &mut flags).unwrap();
        assert_eq!(0x0002, result);
        test_flags(&flags, false, false, false, false, false, false);
    }

    #[test]
    fn test_shift_left_word_with_carry() {
        let mut flags = Flags::empty();
        let result = shift_left_word(0xFFFF, 0x0001, &mut flags).unwrap();
        assert_eq!(0xFFFE, result);
        test_flags(&flags, true, false, false, false, true, false);
    }

    #[test]
    fn test_shift_left_word_with_overflow() {
        let mut flags = Flags::empty();
        let result = shift_left_word(0x7FFF, 0x01, &mut flags).unwrap();
        assert_eq!(0xFFFE, result);
        test_flags(&flags, false, false, false, false, true, true);
    }

    #[test]
    fn test_shift_right_byte() {
        let mut flags = Flags::empty();
        let result = shift_right_byte(0xFF, 0x01, &mut flags).unwrap();
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
        let result = shift_right_word(0xFFFF, 0x0001, &mut flags).unwrap();
        assert_eq!(0x7FFF, result);
        assert!(flags.contains(Flags::CARRY));
        assert!(flags.contains(Flags::OVERFLOW));
        assert!(!flags.contains(Flags::ZERO));
        assert!(!flags.contains(Flags::SIGN));
        assert!(flags.contains(Flags::PARITY));
    }
}
