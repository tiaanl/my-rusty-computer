use crate::cpu2::Flags;

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

pub fn flags_from_value_byte(value: u8, flags: &mut Flags) {
    flags.set(Flags::ZERO, value == 0);
    flags.set(Flags::SIGN, (value & 0x80) != 0);
    flags.set(Flags::PARITY, PARITY_TABLE[value as usize] == 1);
}

pub fn flags_from_value_word(value: u16, flags: &mut Flags) {
    flags.set(Flags::ZERO, value == 0);
    flags.set(Flags::SIGN, (value & 0x8000) != 0);
    flags.set(Flags::PARITY, PARITY_TABLE[(value & 0xFF) as usize] == 1);
}

pub fn rol(value: u8, count: u8, flags: &mut Flags) -> u8 {
    let result = (value << (count & 7)) | (value >> (8 - (count & 7)));
    flags.set(Flags::CARRY, (result & 1) == 1);
    result
}

pub fn ror(_value: u8, _count: u8, _flags: &mut Flags) -> u8 {
    todo!()
}

pub fn rcl(_value: u8, _count: u8, _flags: &mut Flags) -> u8 {
    todo!()
}

pub fn rcr(_value: u8, _count: u8, _flags: &mut Flags) -> u8 {
    todo!()
}

pub fn shl(value: u8, count: u8, flags: &mut Flags) -> u8 {
    if count > 0 {
        let (result, carry) = if count > 8 {
            (0, 0)
        } else {
            let carry = value.wrapping_shl(count as u32 - 1);
            (carry.wrapping_shl(1), carry)
        };

        flags.set(Flags::CARRY, carry & 0x80 != 0);
        flags.set(Flags::OVERFLOW, (result ^ carry) & 0x80 != 0);
        flags_from_value_byte(result, flags);

        result
    } else {
        value
    }
}

pub fn shr(value: u8, count: u8, flags: &mut Flags) -> u8 {
    let mut result = if count > 8 { 0 } else { value >> (count - 1) };

    flags.set(Flags::CARRY, result & 1 != 0);

    result >>= 1;

    flags_from_value_byte(result, flags);

    result
}

pub fn sar(_value: u8, _count: u8, _flags: &mut Flags) -> u8 {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shl() {
        let mut flags = Flags::empty();
        let result = shl(1, 1, &mut flags);
        assert_eq!(2, result);
        assert_eq!(flags.contains(Flags::ZERO), false);
        assert_eq!(flags.contains(Flags::CARRY), false);
        assert_eq!(flags.contains(Flags::OVERFLOW), false);
        assert_eq!(flags.contains(Flags::PARITY), false);
        assert_eq!(flags.contains(Flags::SIGN), false);

        let mut flags = Flags::empty();
        let result = shl(0x40, 1, &mut flags);
        assert_eq!(result, 0x80);
        assert_eq!(flags.contains(Flags::ZERO), false);
        assert_eq!(flags.contains(Flags::CARRY), false);
        assert_eq!(flags.contains(Flags::OVERFLOW), true);
        assert_eq!(flags.contains(Flags::PARITY), false);
        assert_eq!(flags.contains(Flags::SIGN), true);
    }
}
