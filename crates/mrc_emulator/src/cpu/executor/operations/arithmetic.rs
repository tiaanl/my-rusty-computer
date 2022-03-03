use crate::cpu::Flags;

macro_rules! arithmetic {
    ($t:ident) => {
        fn set_flags_all(flags: &mut Flags, carry: bool, destination: $t, result: $t) {
            flags.set(Flags::CARRY, carry);
            flags.set(
                Flags::OVERFLOW,
                destination.most_significant_bit() != result.most_significant_bit(),
            );
            flags_from_result(flags, result);
        }

        pub fn add(destination: $t, source: $t, flags: &mut Flags) -> Option<$t> {
            let (result, carry) = destination.overflowing_add(source);

            set_flags_all(flags, carry, destination, result);

            Some(result)
        }

        pub fn add_with_carry(destination: $t, source: $t, flags: &mut Flags) -> Option<$t> {
            let (mut result, carry) = destination.overflowing_add(source);
            if flags.contains(Flags::CARRY) {
                result = result.wrapping_add(1);
            }

            set_flags_all(flags, carry, destination, result);

            Some(result)
        }

        pub fn compare(destination: $t, source: $t, flags: &mut Flags) -> Option<$t> {
            let _ = subtract(destination, source, flags);
            None
        }

        pub fn multiply(destination: $t, source: $t, flags: &mut Flags) -> Option<$t> {
            let (result, carry) = destination.overflowing_mul(source);

            set_flags_all(flags, carry, destination, result);

            Some(result)
        }

        pub fn subtract(destination: $t, source: $t, flags: &mut Flags) -> Option<$t> {
            let (result, carry) = destination.overflowing_sub(source);

            set_flags_all(flags, carry, destination, result);

            Some(result)
        }

        pub fn increment(destination: $t, flags: &mut Flags) -> Option<$t> {
            let result = destination.wrapping_add(1);

            flags_from_result(flags, result);

            Some(result)
        }

        pub fn decrement(destination: $t, flags: &mut Flags) -> Option<$t> {
            let result = destination.wrapping_sub(1);

            flags_from_result(flags, result);

            Some(result)
        }

        // TODO: Tests
    };
}

pub mod byte {
    use super::*;
    use crate::cpu::executor::operations::{byte::flags_from_result, SignificantBit};

    arithmetic!(u8);
}

pub mod word {
    use super::*;
    use crate::cpu::executor::operations::{word::flags_from_result, SignificantBit};

    arithmetic!(u16);
}
