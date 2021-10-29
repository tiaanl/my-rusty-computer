use crate::cpu::Flags;

macro_rules! logic {
    ($t:ident,$tt:ident) => {
        const BITS: $t = (std::mem::size_of::<$t>() as $t) * 8;

        pub fn and(destination: $t, source: $t, flags: &mut Flags) -> Option<$t> {
            flags.remove(Flags::OVERFLOW | Flags::CARRY);
            let result = destination & source;
            flags_from_result(flags, result);
            Some(result)
        }

        pub fn not(destination: $t) -> Option<$t> {
            let result = !destination;
            Some(result)
        }

        pub fn or(destination: $t, source: $t, flags: &mut Flags) -> Option<$t> {
            flags.remove(Flags::OVERFLOW | Flags::CARRY);
            let result = destination | source;
            flags_from_result(flags, result);
            Some(result)
        }

        pub fn rotate_left(destination: $t, source: $t, flags: &mut Flags) -> Option<$t> {
            let source = source % BITS;
            let result = destination << source | destination >> (BITS - source);

            flags.set(Flags::CARRY, result & 1 == 1);
            flags.set(
                Flags::OVERFLOW,
                destination & 1 << (BITS - 1) != result & 1 << (BITS - 1),
            );

            Some(result)
        }

        pub fn shift_left(value: $t, by: $t, flags: &mut Flags) -> Option<$t> {
            let result = value.wrapping_shl(by.into());

            flags.set(
                Flags::CARRY,
                (value as $tt).wrapping_shl(by.into()) & (($t::MAX as $tt) << BITS) > 0,
            );
            flags.set(
                Flags::OVERFLOW,
                result.most_significant_bit() != value.most_significant_bit(),
            );

            flags_from_result(flags, result);

            Some(result)
        }

        pub fn shift_right(value: $t, by: $t, flags: &mut Flags) -> Option<$t> {
            flags.set(Flags::OVERFLOW, by == 1 && value.most_significant_bit());

            let mut result = value;
            for _ in 0..by {
                flags.set(Flags::CARRY, result.least_significant_bit());
                result >>= 1;
            }

            flags_from_result(flags, result);

            Some(result)
        }

        pub fn rotate_right(value: $t, by: $t, flags: &mut Flags) -> Option<$t> {
            let by = by % BITS;
            let result = value >> by | value << (BITS - by);

            flags.set(Flags::CARRY, result & 1 << (BITS - 1) != 0);
            flags.set(
                Flags::OVERFLOW,
                value & 1 << (BITS - 1) != result & 1 << (BITS - 1),
            );

            flags_from_result(flags, result);

            Some(result)
        }

        pub fn test(left: $t, right: $t, flags: &mut Flags) -> Option<$t> {
            let _ = and(left, right, flags);
            None
        }

        pub fn exclusive_or(destination: $t, source: $t, flags: &mut Flags) -> Option<$t> {
            flags.remove(Flags::OVERFLOW | Flags::CARRY);
            let result = destination ^ source;
            flags_from_result(flags, result);
            Some(result)
        }

        #[cfg(test)]
        mod test {
            use super::*;
            use crate::cpu::executor::operations::test::test_flags;

            #[test]
            fn test_shift_left() {
                let mut flags = Flags::empty();
                let result = shift_left(0x1, 0x1, &mut flags).unwrap();
                assert_eq!(0x2, result);
                test_flags(&flags, false, false, false, false, false, false);
            }

            #[test]
            fn test_shift_left_with_carry() {
                let mut flags = Flags::empty();
                let result = shift_left($t::MAX, 0x1, &mut flags).unwrap();
                assert_eq!($t::MAX - 1, result);
                test_flags(&flags, true, false, false, false, true, false);
            }

            #[test]
            fn test_shift_left_with_overflow() {
                let mut flags = Flags::empty();
                let result = shift_left(1 << (BITS - 2), 0x1, &mut flags).unwrap();
                assert_eq!(1 << (BITS - 1), result);
                if BITS == 8 {
                    test_flags(&flags, false, false, false, false, true, true);
                } else {
                    test_flags(&flags, false, true, false, false, true, true);
                }
            }

            #[test]
            fn test_shift_right() {
                let mut flags = Flags::empty();
                let result = shift_right($t::MAX, 1, &mut flags).unwrap();
                assert_eq!($t::MAX.wrapping_shr(1), result);
                if BITS == 8 {
                    test_flags(&flags, true, false, false, false, false, true);
                } else {
                    test_flags(&flags, true, true, false, false, false, true);
                }
            }
        }
    };
}

pub mod byte {
    use super::*;
    use crate::cpu::executor::operations::{byte::flags_from_result, SignificantBit};

    logic!(u8, u16);
}

pub mod word {
    use super::*;
    use crate::cpu::executor::operations::{word::flags_from_result, SignificantBit};

    logic!(u16, u32);
}
