use super::super::Flags;

pub trait Operation<T> {
    fn op(self, left: T, right: T, flags: &mut Flags) -> T;
}

pub fn arithmetic<T>(operation: impl Operation<T>, left: T, right: T, flags: &mut Flags) -> T {
    operation.op(left, right, flags)
}

pub struct Add;
pub struct And;
pub struct ExclusiveOr;
pub struct Or;
pub struct RotateLeft;
pub struct RotateLeftWithCarry;
pub struct RotateRight;
pub struct RotateRightWithCarry;
pub struct ShiftArithmeticRight;
pub struct ShiftLeft;
pub struct ShiftRight;
pub struct Subtract;

macro_rules! dummy {
    ($name:ident) => {
        impl Operation<u8> for $name {
            fn op(self, _left: u8, _right: u8, _flags: &mut Flags) -> u8 {
                todo!()
            }
        }

        impl Operation<u16> for $name {
            fn op(self, _left: u16, _right: u16, _flags: &mut Flags) -> u16 {
                todo!()
            }
        }
    };
}

impl Operation<u8> for Add {
    fn op(self, left: u8, right: u8, flags: &mut Flags) -> u8 {
        let (result, carry) = left.overflowing_add(right);

        flags.set_from_result(result);
        flags.set(Flags::CARRY, carry);
        flags.set(
            Flags::OVERFLOW,
            ((result ^ left) & (result ^ right) & i8::MIN as u8) != 0,
        );

        result
    }
}

impl Operation<u16> for Add {
    fn op(self, left: u16, right: u16, flags: &mut Flags) -> u16 {
        let (result, carry) = left.overflowing_add(right);

        flags.set_from_result(result);
        flags.set(Flags::CARRY, carry);
        flags.set(
            Flags::OVERFLOW,
            ((result ^ left) & (result ^ right) & i16::MIN as u16) != 0,
        );

        result
    }
}

impl Operation<u8> for And {
    fn op(self, left: u8, right: u8, flags: &mut Flags) -> u8 {
        let result = left & right;

        flags.set_from_result(result);
        flags.set(Flags::CARRY, false);
        flags.set(Flags::OVERFLOW, false);

        result
    }
}

impl Operation<u16> for And {
    fn op(self, left: u16, right: u16, flags: &mut Flags) -> u16 {
        let result = left & right;

        flags.set_from_result(result);
        flags.set(Flags::CARRY, false);
        flags.set(Flags::OVERFLOW, false);

        result
    }
}

impl Operation<u8> for ExclusiveOr {
    fn op(self, left: u8, right: u8, flags: &mut Flags) -> u8 {
        let result = left ^ right;

        flags.set_from_result(result);
        flags.set(Flags::CARRY, false);
        flags.set(Flags::OVERFLOW, false);

        result
    }
}

impl Operation<u16> for ExclusiveOr {
    fn op(self, left: u16, right: u16, flags: &mut Flags) -> u16 {
        let result = left ^ right;

        flags.set_from_result(result);
        flags.set(Flags::CARRY, false);
        flags.set(Flags::OVERFLOW, false);

        result
    }
}

impl Operation<u8> for Or {
    fn op(self, left: u8, right: u8, flags: &mut Flags) -> u8 {
        let result = left | right;

        flags.set_from_result(result);
        flags.set(Flags::CARRY, false);
        flags.set(Flags::OVERFLOW, false);

        result
    }
}

impl Operation<u16> for Or {
    fn op(self, left: u16, right: u16, flags: &mut Flags) -> u16 {
        let result = left | right;

        flags.set_from_result(result);
        flags.set(Flags::CARRY, false);
        flags.set(Flags::OVERFLOW, false);

        result
    }
}

dummy!(RotateLeft);
dummy!(RotateLeftWithCarry);
dummy!(RotateRight);
dummy!(RotateRightWithCarry);
dummy!(ShiftArithmeticRight);

impl Operation<u8> for ShiftLeft {
    fn op(self, left: u8, right: u8, flags: &mut Flags) -> u8 {
        if right > 0 {
            let (result, carry) = if right > 8 {
                (0, 0)
            } else {
                let carry = left.wrapping_shl(right as u32 - 1);
                (carry.wrapping_shl(1), carry)
            };

            flags.set(Flags::CARRY, carry & i8::MIN as u8 != 0);
            flags.set(Flags::OVERFLOW, (result ^ carry) & i8::MIN as u8 != 0);
            flags.set_from_result(result);

            result
        } else {
            left
        }
    }
}

impl Operation<u16> for ShiftLeft {
    fn op(self, left: u16, right: u16, flags: &mut Flags) -> u16 {
        if right > 0 {
            let (result, carry) = if right > 8 {
                (0, 0)
            } else {
                let carry = left.wrapping_shl(right as u32 - 1);
                (carry.wrapping_shl(1), carry)
            };

            flags.set(Flags::CARRY, carry & i16::MIN as u16 != 0);
            flags.set(Flags::OVERFLOW, (result ^ carry) & i16::MIN as u16 != 0);
            flags.set_from_result(result);

            result
        } else {
            left
        }
    }
}

impl Operation<u8> for ShiftRight {
    fn op(self, left: u8, right: u8, flags: &mut Flags) -> u8 {
        let mut result = if right > 8 { 0 } else { left >> (right - 1) };

        flags.set(Flags::CARRY, result & 1 != 0);
        result >>= 1;
        flags.set_from_result(result);

        result
    }
}

impl Operation<u16> for ShiftRight {
    fn op(self, left: u16, right: u16, flags: &mut Flags) -> u16 {
        let mut result = if right > 8 { 0 } else { left >> (right - 1) };

        flags.set(Flags::CARRY, result & 1 != 0);
        result >>= 1;
        flags.set_from_result(result);

        result
    }
}

impl Operation<u8> for Subtract {
    fn op(self, left: u8, right: u8, flags: &mut Flags) -> u8 {
        let (result, carry) = left.overflowing_sub(right);

        flags.set_from_result(result);
        flags.set(Flags::CARRY, carry);
        flags.set(
            Flags::OVERFLOW,
            ((left ^ result) & (left & right) & i8::MIN as u8) != 0,
        );

        result
    }
}

impl Operation<u16> for Subtract {
    fn op(self, left: u16, right: u16, flags: &mut Flags) -> u16 {
        let (result, carry) = left.overflowing_sub(right);

        flags.set_from_result(result);
        flags.set(Flags::CARRY, carry);
        flags.set(
            Flags::OVERFLOW,
            ((left ^ result) & (left & right) & i16::MIN as u16) != 0,
        );

        result
    }
}
