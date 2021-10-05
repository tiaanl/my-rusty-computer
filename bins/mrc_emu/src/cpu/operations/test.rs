use crate::cpu::{Flags, SignificantBit};

pub fn test_byte(left: u8, right: u8, flags: &mut Flags) {
    let result = left & right;

    flags.set(Flags::SIGN, result.most_significant_bit());
    flags.remove(Flags::CARRY | Flags::OVERFLOW);

    super::flags_from_byte_result(flags, result);
}

pub fn test_word(left: u16, right: u16, flags: &mut Flags) {
    let result = left & right;

    flags.set(Flags::SIGN, result.most_significant_bit());
    flags.remove(Flags::CARRY | Flags::OVERFLOW);

    super::flags_from_word_result(flags, result);
}
