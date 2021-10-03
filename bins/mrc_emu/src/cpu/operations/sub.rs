use super::flags_from_byte_result;
use crate::cpu::Flags;

pub fn sub_byte(destination: u8, source: u8, flags: &mut Flags) -> u8 {
    let destination = destination as u16;
    let source = source as u16;

    let result = destination.wrapping_sub(source);

    flags_from_byte_result(flags, result as u8);

    let wide = destination as u16 - source as u16;
    flags.set(Flags::CARRY, wide & 0xFF00 != 0);
    flags.set(
        Flags::OVERFLOW,
        (wide ^ destination) & (destination ^ source) & 0x80 != 0,
    );
    flags.set(Flags::AUX_CARRY, (destination ^ source ^ wide) & 0x10 != 0);

    result as u8
}

pub fn sub_word(destination: u16, source: u16, flags: &mut Flags) -> u16 {
    let destination = destination as u32;
    let source = source as u32;

    let result = destination.wrapping_sub(source);

    super::flags_from_word_result(flags, result as u16);

    let wide = destination as u32 - source as u32;
    flags.set(Flags::CARRY, wide & 0xFFFF0000 != 0);
    flags.set(
        Flags::OVERFLOW,
        (wide ^ destination as u32) & (destination as u32 ^ source as u32) & 0x8000 != 0,
    );
    flags.set(
        Flags::AUX_CARRY,
        (destination as u32 ^ source as u32 ^ wide) & 0x10 != 0,
    );

    result as u16
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_sub_byte() {
        let mut flags = Flags::empty();
        let result = sub_byte(0x10, 0x09, &mut flags);
        assert_eq!(0x07, result);
        assert!(!flags.contains(Flags::CARRY | Flags::ZERO | Flags::SIGN));
    }

    #[test]
    fn test_sub_word() {
        let mut flags = Flags::empty();
        let result = sub_word(0x1000, 0x0900, &mut flags);
        assert_eq!(0x0700, result);
        assert!(!flags.contains(Flags::CARRY | Flags::ZERO | Flags::SIGN));
    }
}
