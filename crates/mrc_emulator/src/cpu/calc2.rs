#![allow(unused)]

use crate::cpu::Flags;

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

macro_rules! ops {
    () => {
        use std::ops::BitAnd;

        pub fn flags_from_value(value: Type, flags: &mut Flags) {
            flags.set(Flags::ZERO, value == 0);
            flags.set(Flags::SIGN, (value & SignedType::MIN as Type) != 0);
            flags.set(
                Flags::PARITY,
                PARITY_TABLE[(value & u8::MAX as Type) as usize] == 1,
            );
        }

        pub fn add(dst: Type, src: Type, flags: &mut Flags) -> Type {
            let (result, carry) = dst.overflowing_add(src);

            flags_from_value(result, flags);
            flags.set(Flags::CARRY, carry);
            flags.set(
                Flags::OVERFLOW,
                ((result ^ dst) & (result ^ src) & SignedType::MIN as Type) != 0,
            );

            result
        }

        pub fn and(dst: Type, src: Type, flags: &mut Flags) -> Type {
            let result = dst.bitand(src);

            flags_from_value(result, flags);
            flags.set(Flags::CARRY, false);
            flags.set(Flags::OVERFLOW, false);

            result
        }

        pub fn cmp(dst: Type, src: Type, flags: &mut Flags) -> Type {
            let result = dst;

            flags_from_value(dst.wrapping_sub(src), flags);

            result
        }

        pub fn or(dst: Type, src: Type, flags: &mut Flags) -> Type {
            let result = dst | src;

            flags_from_value(result, flags);

            result
        }

        pub fn sub(dst: Type, src: Type, flags: &mut Flags) -> Type {
            let (result, carry) = dst.overflowing_sub(src);

            flags_from_value(result, flags);

            flags.set(Flags::CARRY, carry);

            flags.set(
                Flags::OVERFLOW,
                ((dst ^ result) & (dst & src) & SignedType::MIN as Type) != 0,
            );

            result
        }

        pub fn test(dst: Type, src: Type, flags: &mut Flags) -> Type {
            let result = dst;

            flags.set(Flags::CARRY, false);
            flags.set(Flags::OVERFLOW, false);
            flags_from_value(dst & src, flags);

            result
        }

        pub fn xor(dst: Type, src: Type, flags: &mut Flags) -> Type {
            let result = dst ^ src;

            flags.set(Flags::CARRY, false);
            flags.set(Flags::OVERFLOW, false);
            flags_from_value(result, flags);

            result
        }

        pub fn rol(dst: Type, count: u16, flags: &mut Flags) -> Type {
            let result = dst << count;

            flags.set(Flags::CARRY, false);
            flags.set(Flags::OVERFLOW, false);
            flags_from_value(result, flags);

            result
        }

        pub fn ror(dst: Type, count: u16, flags: &mut Flags) -> Type {
            todo!()
        }

        pub fn rcl(dst: Type, count: u16, flags: &mut Flags) -> Type {
            todo!()
        }

        pub fn rcr(dst: Type, count: u16, flags: &mut Flags) -> Type {
            todo!()
        }

        pub fn shl(dst: Type, count: u16, flags: &mut Flags) -> Type {
            if count > 0 {
                let (result, carry) = if count > 8 {
                    (0, 0)
                } else {
                    let carry = dst.wrapping_shl(count as u32 - 1);
                    (carry.wrapping_shl(1), carry)
                };

                flags.set(Flags::CARRY, carry & SignedType::MIN as Type != 0);
                flags.set(
                    Flags::OVERFLOW,
                    (result ^ carry) & SignedType::MIN as Type != 0,
                );
                flags_from_value(result, flags);

                result
            } else {
                dst
            }
        }

        pub fn shr(dst: Type, count: u16, flags: &mut Flags) -> Type {
            let mut result = if count > 8 { 0 } else { dst >> (count - 1) };

            flags.set(Flags::CARRY, result & 1 != 0);
            result >>= 1;
            flags_from_value(result, flags);

            result
        }

        pub fn sar(dst: Type, count: u16, flags: &mut Flags) -> Type {
            todo!()
        }
    };
}

pub mod byte {
    use super::*;

    type Type = u8;
    type SignedType = i8;

    ops!();
}

pub mod word {
    use super::*;

    type Type = u16;
    type SignedType = i16;

    ops!();
}
