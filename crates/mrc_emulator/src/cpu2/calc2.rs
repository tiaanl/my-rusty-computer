#![allow(unused)]

use crate::cpu2::Flags;
use crate::{Address, Bus};

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
    flags.set(Flags::SIGN, (value & 0x80) != 0x80);
    flags.set(Flags::PARITY, PARITY_TABLE[value as usize] == 1);
}

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
            todo!()
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

// pub fn rol<S, T: SizedValue<S>>(value: T, count: u32, flags: &mut Flags) -> T {
//     let result = value.rol(count);
//     flags.set(Flags::CARRY, result.least_significant_bit());
//     result
// }
//
// pub fn ror(value: u8, count: u8, flags: &mut Flags) -> u8 {
//     todo!()
// }
//
// pub fn rcl(value: u8, count: u8, flags: &mut Flags) -> u8 {
//     todo!()
// }
//
// pub fn rcr(value: u8, count: u8, flags: &mut Flags) -> u8 {
//     todo!()
// }
//
// pub fn shl(value: u8, count: u8, flags: &mut Flags) -> u8 {
//     if count > 0 {
//         let (result, carry) = if count > 8 {
//             (0, 0)
//         } else {
//             let carry = value.wrapping_shl(count as u32 - 1);
//             (carry.wrapping_shl(1), carry)
//         };
//
//         flags.set(Flags::CARRY, carry & 0x80 != 0);
//         flags.set(Flags::OVERFLOW, (result ^ carry) & 0x80 != 0);
//         flags_from_value_byte(result, flags);
//
//         result
//     } else {
//         value
//     }
// }
//
// pub fn shr(value: u8, count: u8, flags: &mut Flags) -> u8 {
//     let mut result = if count > 8 { 0 } else { value >> (count - 1) };
//
//     flags.set(Flags::CARRY, result & 1 != 0);
//
//     result >>= 1;
//
//     flags_from_value_byte(result, flags);
//
//     result
// }
//
// pub fn sar(_value: u8, _count: u8, _flags: &mut Flags) -> u8 {
//     todo!()
// }
//
// pub trait SizedValue<S: Sized> {
//     fn read_from_bus(bus: &impl Bus<Address>, addr: Address) -> Self;
//     fn write_to_bus(bus: &mut impl Bus<Address>, addr: Address, value: Self);
//
//     fn read_from_register(registers: &[u16; 8], encoding: usize) -> Self;
//     fn write_to_register(registers: &mut [u16; 8], encoding: usize, value: Self);
//
//     fn get(&self) -> S;
//
//     fn is_zero(&self) -> bool;
//     fn is_signed(&self) -> bool;
//     fn is_parity(&self) -> bool;
//     fn least_significant_bit(&self) -> bool;
//
//     fn or(&self, other: &Self) -> Self;
//     fn and(&self, other: &Self) -> Self;
//     fn rol(&self, count: u32) -> Self;
// }
//
// impl SizedValue<u8> for u8 {
//     fn read_from_bus(bus: &impl Bus<Address>, addr: Address) -> Self {
//         bus.read(addr).unwrap()
//     }
//
//     fn write_to_bus(bus: &mut impl Bus<Address>, addr: Address, value: Self) {
//         bus.write(addr, value.get()).unwrap();
//     }
//
//     fn read_from_register(registers: &[u16; 8], encoding: usize) -> Self {
//         let value = registers[encoding & 0b11];
//         if encoding & 0b100 == 0b100 {
//             (value >> 8) as u8
//         } else {
//             value as u8
//         }
//     }
//
//     fn write_to_register(registers: &mut [u16; 8], encoding: usize, value: Self) {
//         let orig = registers[encoding & 0b11];
//         registers[encoding & 0b11] = if encoding & 0b100 == 0b100 {
//             (orig & 0xFF00) + (value.get() as u16) << 8
//         } else {
//             (orig & 0x00FF) + value.get() as u16
//         }
//     }
//
//     fn get(&self) -> u8 {
//         *self
//     }
//
//     fn is_zero(&self) -> bool {
//         *self == 0
//     }
//
//     fn is_signed(&self) -> bool {
//         *self & 0x80 == 0x80
//     }
//
//     fn is_parity(&self) -> bool {
//         PARITY_TABLE[*self as usize] == 1
//     }
//
//     fn least_significant_bit(&self) -> bool {
//         self & 1 == 1
//     }
//
//     fn or(&self, other: &Self) -> u8 {
//         self | other
//     }
//
//     fn and(&self, other: &Self) -> Self {
//         self & other
//     }
//
//     fn rol(&self, count: u32) -> Self {
//         (*self << (count & 7)) | (*self >> (8 - (count & 7)))
//     }
// }
//
// impl SizedValue<u16> for u16 {
//     fn read_from_bus(bus: &impl Bus<Address>, addr: Address) -> Self {
//         let lo = bus.read(addr).unwrap();
//         let hi = bus.read(addr + 1).unwrap();
//
//         u16::from_le_bytes([lo, hi])
//     }
//
//     fn write_to_bus(bus: &mut impl Bus<Address>, addr: Address, value: Self) {
//         let [lo, hi] = value.get().to_le_bytes();
//         bus.write(addr, lo).unwrap();
//         bus.write(addr, hi).unwrap();
//     }
//
//     fn read_from_register(registers: &[u16; 8], encoding: usize) -> Self {
//         registers[encoding]
//     }
//
//     fn write_to_register(registers: &mut [u16; 8], encoding: usize, value: Self) {
//         registers[encoding] = value.get();
//     }
//
//     fn get(&self) -> u16 {
//         *self
//     }
//
//     fn is_zero(&self) -> bool {
//         *self == 0
//     }
//
//     fn is_signed(&self) -> bool {
//         *self & 0x8000 == 0x8000
//     }
//
//     fn is_parity(&self) -> bool {
//         PARITY_TABLE[(*self & 0xFF) as usize] == 1
//     }
//
//     fn least_significant_bit(&self) -> bool {
//         self & 1 == 1
//     }
//
//     fn or(&self, other: &Self) -> u16 {
//         self.get() | other.get()
//     }
//
//     fn and(&self, other: &Self) -> Self {
//         self & other
//     }
//
//     fn rol(&self, count: u32) -> Self {
//         (*self << (count & 15)) | (*self >> (16 - (count & 15)))
//     }
// }
//
// fn flags_from_result<S, T: SizedValue<S>>(value: &T, flags: &mut Flags) {
//     flags.set(Flags::ZERO, value.is_zero());
//     flags.set(Flags::SIGN, value.is_signed());
//     flags.set(Flags::PARITY, value.is_parity());
// }
//
// pub fn or<S, T: SizedValue<S>>(dst: T, src: T, flags: &mut Flags) -> S {
//     let result = dst.or(&src);
//
//     flags_from_result(&result, flags);
//
//     result.get()
// }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cpu2::{calc2, SizedValue, AX, SP};

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

    #[test]
    fn test_generics() {
        let dst = 10_u8;
        let src = 20_u8;
        let mut flags = Flags::empty();

        let result = or(dst, src, &mut flags);

        assert_eq!(result, 10 | 20);
        assert_eq!(flags, Flags::empty());

        let dst = 0_u16;
        let src = 0_u16;
        let mut flags = Flags::empty();

        let result = or(dst, src, &mut flags);

        assert_eq!(result, 0);
        assert_eq!(flags, Flags::ZERO);
    }

    #[test]
    fn test_registers() {
        let mut registers = [0_u16; 8];
        registers[0] = u16::from_le_bytes([0x01, 0x02]);
        assert_eq!(u8::read_from_register(&registers, 0), 0x01);
        assert_eq!(u8::read_from_register(&registers, 4), 0x02);

        let mut flags = Flags::empty();
        let result = calc2::byte::or(0x01, 0x01, &mut flags);
        assert_eq!(result, 0x01);
    }
}
