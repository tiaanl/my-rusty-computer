mod decode;
mod errors;
mod modrm;

pub use decode::{decode_instruction, DecodeResult};
pub use errors::{Error, Result};
pub use modrm::Modrm;

use crate::instructions::*;

impl Register {
    fn try_from_low_bits(byte: u8) -> Result<Self> {
        assert!(byte <= 0b111);

        match byte {
            0b000 => Ok(Register::AlAx),
            0b001 => Ok(Register::ClCx),
            0b010 => Ok(Register::DlDx),
            0b011 => Ok(Register::BlBx),
            0b100 => Ok(Register::AhSp),
            0b101 => Ok(Register::ChBp),
            0b110 => Ok(Register::DhSi),
            0b111 => Ok(Register::BhDi),
            _ => Err(Error::InvalidRegisterEncoding(byte)),
        }
    }
}

impl Segment {
    fn try_from_low_bits(byte: u8) -> Result<Self> {
        assert!(byte <= 0b11);

        match byte {
            0b00 => Ok(Self::Es),
            0b01 => Ok(Self::Cs),
            0b10 => Ok(Self::Ss),
            0b11 => Ok(Self::Ds),
            _ => Err(Error::InvalidSegmentEncoding(byte)),
        }
    }
}

pub trait ByteReader {
    fn read_u8(&self) -> Result<u8>;
    fn read_u16(&self) -> Result<u16>;
}

impl ByteReader for &[u8] {
    fn read_u8(&self) -> Result<u8> {
        if !self.is_empty() {
            Ok(self[0])
        } else {
            Err(Error::CouldNotReadExtraBytes)
        }
    }

    fn read_u16(&self) -> Result<u16> {
        if self.len() >= 2 {
            Ok(((self[1] as u16) << 8) + self[0] as u16)
        } else {
            Err(Error::CouldNotReadExtraBytes)
        }
    }
}

impl OperandSize {
    fn try_from_low_bits(encoding: u8) -> Result<OperandSize> {
        match encoding {
            0b0 => Ok(OperandSize::Byte),
            0b1 => Ok(OperandSize::Word),
            _ => Err(Error::InvalidDataSizeEncoding(encoding)),
        }
    }

    fn in_bytes(&self) -> usize {
        match self {
            OperandSize::Byte => 1,
            OperandSize::Word => 2,
        }
    }
}
