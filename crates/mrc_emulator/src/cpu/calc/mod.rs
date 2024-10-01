use super::Flags;

pub mod arithmetic;
pub mod logic;

trait SizedValueTraits {
    fn is_zero(&self) -> bool;
    fn is_signed(&self) -> bool;
    fn is_parity(&self) -> bool;
}

impl Flags {
    fn set_from_result<T: SizedValueTraits>(&mut self, result: T) {
        self.set(Flags::ZERO, result.is_zero());
        self.set(Flags::SIGN, result.is_signed());
        self.set(Flags::PARITY, result.is_parity());
    }
}

impl SizedValueTraits for u8 {
    fn is_zero(&self) -> bool {
        *self == 0
    }

    fn is_signed(&self) -> bool {
        *self & 0x80 != 0
    }

    fn is_parity(&self) -> bool {
        PARITY_TABLE[*self as usize] == 1
    }
}

impl SizedValueTraits for u16 {
    fn is_zero(&self) -> bool {
        *self == 0
    }

    fn is_signed(&self) -> bool {
        *self & 0x8000 != 0
    }

    fn is_parity(&self) -> bool {
        PARITY_TABLE[*self as usize & 0xFF] == 1
    }
}

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
