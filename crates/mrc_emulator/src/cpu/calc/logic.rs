use super::super::Flags;

pub trait Operation<T> {
    fn op(self, left: T, right: T, flags: &mut Flags);
}

pub fn logic<T>(op: impl Operation<T>, left: T, right: T, flags: &mut Flags) {
    op.op(left, right, flags);
}

pub struct Compare;
pub struct Test;

impl Operation<u8> for Compare {
    fn op(self, left: u8, right: u8, flags: &mut Flags) {
        let result = left.wrapping_sub(right);

        flags.set_from_result(result);
    }
}

impl Operation<u16> for Compare {
    fn op(self, left: u16, right: u16, flags: &mut Flags) {
        let result = left.wrapping_sub(right);

        flags.set_from_result(result);
    }
}

impl Operation<u8> for Test {
    fn op(self, left: u8, right: u8, flags: &mut Flags) {
        let result = left & right;

        flags.set_from_result(result);
    }
}

impl Operation<u16> for Test {
    fn op(self, left: u16, right: u16, flags: &mut Flags) {
        let result = left & right;

        flags.set_from_result(result);
    }
}
