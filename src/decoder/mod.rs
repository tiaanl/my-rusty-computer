mod decode;
mod errors;
mod mod_rm;

pub use decode::{decode_instruction, DecodeResult};
pub use errors::{Error, Result};
pub use mod_rm::ModRM;

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

    fn try_from_mod_rm_byte(mod_rm_byte: u8) -> Result<Self> {
        if mod_rm_byte >> 6 == 0b11 {
            Register::try_from_low_bits(mod_rm_byte >> 3 & 0b111)
        } else {
            Err(Error::InvalidModRMMode(mod_rm_byte))
        }
    }
}

impl Segment {
    fn try_from_encoding(encoding: u8) -> Result<Self> {
        match encoding {
            0b00 => Ok(Self::Es),
            0b01 => Ok(Self::Cs),
            0b10 => Ok(Self::Ss),
            0b11 => Ok(Self::Ds),
            _ => Err(Error::InvalidSegmentEncoding(encoding)),
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

impl DataSize {
    fn try_from_encoding(encoding: u8) -> Result<DataSize> {
        match encoding {
            0b0 => Ok(DataSize::Byte),
            0b1 => Ok(DataSize::Word),
            _ => Err(Error::InvalidDataSizeEncoding(encoding)),
        }
    }

    fn in_bytes(&self) -> usize {
        match self {
            DataSize::Byte => 1,
            DataSize::Word => 2,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_encoding_from_byte() {
        assert_eq!(
            RegisterEncoding::try_from_mod_rm_byte(0b11_000_000).unwrap(),
            RegisterEncoding::AlAx
        );
        assert_eq!(
            RegisterEncoding::try_from_mod_rm_byte(0b11_001_000).unwrap(),
            RegisterEncoding::ClCx
        );
        assert_eq!(
            RegisterEncoding::try_from_mod_rm_byte(0b11_010_000).unwrap(),
            RegisterEncoding::DlDx
        );
        assert_eq!(
            RegisterEncoding::try_from_mod_rm_byte(0b11_011_000).unwrap(),
            RegisterEncoding::BlBx
        );
        assert_eq!(
            RegisterEncoding::try_from_mod_rm_byte(0b11_100_000).unwrap(),
            RegisterEncoding::AhSp
        );
        assert_eq!(
            RegisterEncoding::try_from_mod_rm_byte(0b11_101_000).unwrap(),
            RegisterEncoding::ChBp
        );
        assert_eq!(
            RegisterEncoding::try_from_mod_rm_byte(0b11_110_000).unwrap(),
            RegisterEncoding::DhSi
        );
        assert_eq!(
            RegisterEncoding::try_from_mod_rm_byte(0b11_111_000).unwrap(),
            RegisterEncoding::BhDi
        );
        assert_eq!(
            RegisterEncoding::try_from_mod_rm_byte(0b00_000_000),
            Err(Error::InvalidModRMMode(0b00))
        );
    }
}
