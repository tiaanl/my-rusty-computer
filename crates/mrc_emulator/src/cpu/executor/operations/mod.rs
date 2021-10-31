use crate::cpu::{Flags, State};
use mrc_x86::Register;

mod arithmetic;
mod logic;

pub trait SignificantBit {
    fn least_significant_bit(&self) -> bool;
    fn most_significant_bit(&self) -> bool;
}

macro_rules! significant_bit {
    ($t:ident) => {
        impl SignificantBit for $t {
            fn least_significant_bit(&self) -> bool {
                self & 0x1 != 0
            }

            fn most_significant_bit(&self) -> bool {
                self & (0x1 << (std::mem::size_of::<$t>() * 8) - 1) != 0
            }
        }
    };
}

significant_bit!(u8);
significant_bit!(u16);

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

pub(crate) fn parity_for(value: u8) -> bool {
    PARITY_TABLE[value as usize] == 1
}

pub mod byte {
    use super::SignificantBit;
    use crate::cpu::Flags;

    pub fn flags_from_result(flags: &mut Flags, result: u8) {
        flags.set(Flags::ZERO, result == 0);
        flags.set(Flags::SIGN, result.most_significant_bit());
        flags.set(Flags::PARITY, super::parity_for(result));
    }

    pub use super::arithmetic::byte::{add, add_with_carry, compare, multiply, subtract};
    pub use super::logic::byte::{
        and, exclusive_or, not, or, rotate_left, rotate_right, shift_left, shift_right, test,
    };
}

pub mod word {
    use super::SignificantBit;
    use crate::cpu::Flags;

    pub fn flags_from_result(flags: &mut Flags, result: u16) {
        flags.set(Flags::ZERO, result == 0);
        flags.set(Flags::SIGN, result.most_significant_bit());
        flags.set(Flags::PARITY, super::parity_for(result as u8));
    }

    pub use super::arithmetic::word::{add, add_with_carry, compare, multiply, subtract};
    pub use super::logic::word::{
        and, exclusive_or, not, or, rotate_left, rotate_right, shift_left, shift_right, test,
    };
}

pub trait StateExt {
    fn store_ah_into_flags(&mut self);
    fn load_ah_from_flags(&mut self);
}

impl StateExt for State {
    /// Store `AH` into the `FLAGS` register. Only affects `SIGN`, `ZERO`, `AUX_CARRY`, `PARITY`
    /// and `CARRY`. Takes the state of reserved flags into account.
    fn store_ah_into_flags(&mut self) {
        // EFLAGS(SF:ZF:0:AF:0:PF:1:CF) = AH;

        let ah = self.get_byte_register_value(Register::AhSp);
        let mut new_flags = Flags::from_bits(u16::from(ah)).unwrap();

        // Make sure we set/unset the fixed values:
        new_flags.insert(Flags::RESERVED_1);
        new_flags.remove(Flags::RESERVED_3 | Flags::RESERVED_5);

        self.flags = new_flags;
    }

    /// Set `AH` to the low byte of the `FLAGS` register. Takes the state of reserved flags into
    /// account.
    fn load_ah_from_flags(&mut self) {
        // AH = EFLAGS(SF:ZF:0:AF:0:PF:1:CF);

        let mut ah = self.flags;
        ah.insert(Flags::RESERVED_1);
        ah.remove(Flags::RESERVED_3 | Flags::RESERVED_5);

        self.set_byte_register_value(Register::AhSp, ah.bits.to_le_bytes()[0]);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    pub fn test_flags(
        flags: &Flags,
        carry: bool,
        parity: bool,
        aux_carry: bool,
        zero: bool,
        sign: bool,
        overflow: bool,
    ) {
        assert_eq!(carry, flags.contains(Flags::CARRY), "CARRY");
        assert_eq!(parity, flags.contains(Flags::PARITY), "PARITY");
        assert_eq!(aux_carry, flags.contains(Flags::AUX_CARRY), "AUX_CARRY");
        assert_eq!(zero, flags.contains(Flags::ZERO), "ZERO");
        assert_eq!(sign, flags.contains(Flags::SIGN), "SIGN");
        assert_eq!(overflow, flags.contains(Flags::OVERFLOW), "OVERFLOW");
    }

    #[test]
    fn test_store_ah_into_flags() {
        let mut state = State::default();

        state.set_byte_register_value(Register::AhSp, 0xD5);
        state.store_ah_into_flags();
        assert!(state.flags.contains(Flags::SIGN));
        assert!(state.flags.contains(Flags::ZERO));
        assert!(state.flags.contains(Flags::AUX_CARRY));
        assert!(state.flags.contains(Flags::PARITY));
        assert!(state.flags.contains(Flags::CARRY));
    }

    #[test]
    fn test_load_ah_from_flags() {
        let mut state = State::with_flags(
            Flags::SIGN | Flags::ZERO | Flags::AUX_CARRY | Flags::PARITY | Flags::CARRY,
        );

        state.load_ah_from_flags();

        assert_eq!(0xD7, state.get_byte_register_value(Register::AhSp));
    }
}
