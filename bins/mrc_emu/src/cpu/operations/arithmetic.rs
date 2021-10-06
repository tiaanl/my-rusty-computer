use crate::cpu::operations::{flags_from_byte_result, flags_from_word_result};
use crate::cpu::{Flags, SignificantBit};

pub fn add_byte(destination: u8, source: u8, flags: &mut Flags) -> Option<u8> {
    let (result, carry) = destination.overflowing_add(source);

    flags.set(Flags::CARRY, carry);
    flags.set(
        Flags::OVERFLOW,
        destination.most_significant_bit() != result.most_significant_bit(),
    );

    flags_from_byte_result(flags, result);

    Some(result)
}

pub fn add_word(destination: u16, source: u16, flags: &mut Flags) -> Option<u16> {
    let (result, carry) = destination.overflowing_add(source);

    flags.set(Flags::CARRY, carry);
    flags.set(
        Flags::OVERFLOW,
        destination.most_significant_bit() != result.most_significant_bit(),
    );

    flags_from_word_result(flags, result);

    Some(result)
}

pub fn add_with_carry_byte(destination: u8, source: u8, flags: &mut Flags) -> Option<u8> {
    let mut result = destination.wrapping_add(source);
    if flags.contains(Flags::CARRY) {
        result = result.wrapping_add(1);
    }

    flags.set(
        Flags::CARRY,
        (destination as u16 + source as u16) & 0xFF00 > 0,
    );
    flags.set(
        Flags::OVERFLOW,
        destination.most_significant_bit() != result.most_significant_bit(),
    );

    flags_from_byte_result(flags, result);

    Some(result)
}

pub fn add_with_carry_word(destination: u16, source: u16, flags: &mut Flags) -> Option<u16> {
    let mut result = destination.wrapping_add(source);
    if flags.contains(Flags::CARRY) {
        result = result.wrapping_add(1);
    }

    flags.set(
        Flags::CARRY,
        (destination as u32 + source as u32) & 0xFFFF0000 > 0,
    );
    flags.set(
        Flags::OVERFLOW,
        destination.most_significant_bit() != result.most_significant_bit(),
    );

    flags_from_word_result(flags, result);

    Some(result)
}

pub fn compare_byte(destination: u8, source: u8, flags: &mut Flags) -> Option<u8> {
    sub_byte(destination, source, flags);
    None
}

pub fn compare_word(destination: u16, source: u16, flags: &mut Flags) -> Option<u16> {
    sub_word(destination, source, flags);
    None
}

pub fn multiply_byte(destination: u8, source: u8, flags: &mut Flags) -> Option<u8> {
    let (result, carry) = destination.overflowing_mul(source);

    flags.set(Flags::CARRY, carry);
    flags.set(
        Flags::OVERFLOW,
        destination.most_significant_bit() != result.most_significant_bit(),
    );

    flags_from_byte_result(flags, result);

    Some(result)
}

pub fn multiply_word(destination: u16, source: u16, flags: &mut Flags) -> Option<u16> {
    let (result, carry) = destination.overflowing_mul(source);

    flags.set(Flags::CARRY, carry);
    flags.set(
        Flags::OVERFLOW,
        destination.most_significant_bit() != result.most_significant_bit(),
    );

    flags_from_word_result(flags, result);

    Some(result)
}

pub fn sub_byte(destination: u8, source: u8, flags: &mut Flags) -> Option<u8> {
    let (result, carry) = destination.overflowing_sub(source);

    flags.set(Flags::CARRY, carry);
    flags.set(
        Flags::OVERFLOW,
        destination.most_significant_bit() != result.most_significant_bit(),
    );

    flags_from_byte_result(flags, result);

    Some(result)
}

pub fn sub_word(destination: u16, source: u16, flags: &mut Flags) -> Option<u16> {
    let (result, carry) = destination.overflowing_sub(source);

    flags.set(Flags::CARRY, carry);
    flags.set(
        Flags::OVERFLOW,
        destination.most_significant_bit() != result.most_significant_bit(),
    );

    flags_from_word_result(flags, result);

    Some(result)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_sub_byte() {
        let mut flags = Flags::empty();
        let result = sub_byte(0x10, 0x09, &mut flags).unwrap();
        assert_eq!(0x07, result);
        assert!(!flags.contains(Flags::CARRY | Flags::ZERO | Flags::SIGN));
    }

    #[test]
    fn test_sub_word() {
        let mut flags = Flags::empty();
        let result = sub_word(0x1000, 0x0900, &mut flags).unwrap();
        assert_eq!(0x0700, result);
        assert!(!flags.contains(Flags::CARRY | Flags::ZERO | Flags::SIGN));
    }
}