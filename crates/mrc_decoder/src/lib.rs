mod common;
mod decode;
mod errors;
mod mrrm;
mod traits;

pub use decode::decode_instruction;
pub use errors::{DecodeError, Result};
pub use mrrm::{ModRegRM, RegisterOrMemory};

use mrc_instruction::{Register, Segment};

trait TryFromEncoding<T> {
    fn try_from_encoding(encoding: u8) -> Result<T>;
}

impl TryFromEncoding<Register> for Register {
    fn try_from_encoding(encoding: u8) -> Result<Self> {
        debug_assert!(encoding <= 0b111);

        match encoding {
            0b000 => Ok(Register::AlAx),
            0b001 => Ok(Register::ClCx),
            0b010 => Ok(Register::DlDx),
            0b011 => Ok(Register::BlBx),
            0b100 => Ok(Register::AhSp),
            0b101 => Ok(Register::ChBp),
            0b110 => Ok(Register::DhSi),
            0b111 => Ok(Register::BhDi),
            _ => Err(DecodeError::InvalidRegisterEncoding(encoding)),
        }
    }
}

impl TryFromEncoding<Self> for Segment {
    fn try_from_encoding(encoding: u8) -> Result<Self> {
        match encoding {
            0b00 => Ok(Self::ES),
            0b01 => Ok(Self::CS),
            0b10 => Ok(Self::SS),
            0b11 => Ok(Self::DS),
            _ => Err(DecodeError::InvalidSegmentEncoding(encoding)),
        }
    }
}
