use crate::cpu::Flags;

pub fn rol_byte(left: u8, right: u8, flags: &mut Flags) -> u8 {
    let right = right % 8;
    let res = left << right | left >> (8 - right);

    flags.set(Flags::CARRY, res & 1 == 1);
    flags.set(Flags::OVERFLOW, left & 1 << 7 != res & 1 << 7);

    res
}

pub fn rol_word(left: u16, right: u16, flags: &mut Flags) -> u16 {
    let right = right % 16;
    let res = left << right | left >> (16 - right);

    flags.set(Flags::CARRY, res & 1 == 1);
    flags.set(Flags::OVERFLOW, left & 1 << 15 != res & 1 << 15);

    res
}
