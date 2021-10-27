use crate::cpu::Flags;

use super::{flags_from_byte_result, flags_from_word_result, SignificantBit};

pub mod byte {
    use super::*;

    pub fn add(destination: u8, source: u8, flags: &mut Flags) -> Option<u8> {
        let (result, carry) = destination.overflowing_add(source);

        flags.set(Flags::CARRY, carry);
        flags.set(
            Flags::OVERFLOW,
            destination.most_significant_bit() != result.most_significant_bit(),
        );

        flags_from_byte_result(flags, result);

        Some(result)
    }

    pub fn add_with_carry(destination: u8, source: u8, flags: &mut Flags) -> Option<u8> {
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

    pub fn compare(destination: u8, source: u8, flags: &mut Flags) -> Option<u8> {
        subtract(destination, source, flags);
        None
    }

    pub fn multiply(destination: u8, source: u8, flags: &mut Flags) -> Option<u8> {
        let (result, carry) = destination.overflowing_mul(source);

        flags.set(Flags::CARRY, carry);
        flags.set(
            Flags::OVERFLOW,
            destination.most_significant_bit() != result.most_significant_bit(),
        );

        flags_from_byte_result(flags, result);

        Some(result)
    }

    pub fn subtract(destination: u8, source: u8, flags: &mut Flags) -> Option<u8> {
        let (result, carry) = destination.overflowing_sub(source);

        flags.set(Flags::CARRY, carry);
        flags.set(
            Flags::OVERFLOW,
            destination.most_significant_bit() != result.most_significant_bit(),
        );

        flags_from_byte_result(flags, result);

        Some(result)
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_sub_byte() {
            let mut flags = Flags::empty();
            let result = subtract(0x10, 0x09, &mut flags).unwrap();
            assert_eq!(0x07, result);
            assert!(!flags.contains(Flags::CARRY | Flags::ZERO | Flags::SIGN));
        }
    }
}

pub mod word {
    use super::*;

    pub fn add(destination: u16, source: u16, flags: &mut Flags) -> Option<u16> {
        let (result, carry) = destination.overflowing_add(source);

        flags.set(Flags::CARRY, carry);
        flags.set(
            Flags::OVERFLOW,
            destination.most_significant_bit() != result.most_significant_bit(),
        );

        flags_from_word_result(flags, result);

        Some(result)
    }

    pub fn add_with_carry(destination: u16, source: u16, flags: &mut Flags) -> Option<u16> {
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

    pub fn compare(destination: u16, source: u16, flags: &mut Flags) -> Option<u16> {
        subtract(destination, source, flags);
        None
    }

    pub fn multiply(destination: u16, source: u16, flags: &mut Flags) -> Option<u16> {
        let (result, carry) = destination.overflowing_mul(source);

        flags.set(Flags::CARRY, carry);
        flags.set(
            Flags::OVERFLOW,
            destination.most_significant_bit() != result.most_significant_bit(),
        );

        flags_from_word_result(flags, result);

        Some(result)
    }

    pub fn subtract(destination: u16, source: u16, flags: &mut Flags) -> Option<u16> {
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
            let result = subtract_byte(0x10, 0x09, &mut flags).unwrap();
            assert_eq!(0x07, result);
            assert!(!flags.contains(Flags::CARRY | Flags::ZERO | Flags::SIGN));
        }

        #[test]
        fn test_sub_word() {
            let mut flags = Flags::empty();
            let result = subtract(0x1000, 0x0900, &mut flags).unwrap();
            assert_eq!(0x0700, result);
            assert!(!flags.contains(Flags::CARRY | Flags::ZERO | Flags::SIGN));
        }
    }
}

