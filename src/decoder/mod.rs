mod decode;
mod errors;
mod mod_rm;

pub use decode::{decode_instruction, DecodeResult};
pub use errors::DecodeError;
pub use mod_rm::ModRM;

use crate::instructions::*;

impl RegisterEncoding {
    fn try_from_encoding(byte: u8) -> Result<Self, DecodeError> {
        match byte {
            0b000 => Ok(RegisterEncoding::AlAx),
            0b001 => Ok(RegisterEncoding::ClCx),
            0b010 => Ok(RegisterEncoding::DlDx),
            0b011 => Ok(RegisterEncoding::BlBx),
            0b100 => Ok(RegisterEncoding::AhSp),
            0b101 => Ok(RegisterEncoding::ChBp),
            0b110 => Ok(RegisterEncoding::DhSi),
            0b111 => Ok(RegisterEncoding::BhDi),
            _ => Err(DecodeError::InvalidRegisterEncoding(byte)),
        }
    }

    fn try_from_mod_rm_byte(mod_rm_byte: u8) -> Result<Self, DecodeError> {
        if mod_rm_byte >> 6 == 0b11 {
            RegisterEncoding::try_from_encoding(mod_rm_byte >> 3 & 0b111)
        } else {
            Err(DecodeError::InvalidModRMMode(mod_rm_byte))
        }
    }
}

impl SegmentEncoding {
    fn try_from_encoding(encoding: u8) -> Result<Self, DecodeError> {
        match encoding {
            0b00 => Ok(Self::Es),
            0b01 => Ok(Self::Cs),
            0b10 => Ok(Self::Ss),
            0b11 => Ok(Self::Ds),
            _ => Err(DecodeError::InvalidSegmentEncoding(encoding)),
        }
    }
}

pub trait ByteReader {
    fn read_u8(&self) -> Result<u8, DecodeError>;
    fn read_u16(&self) -> Result<u16, DecodeError>;
}

impl ByteReader for &[u8] {
    fn read_u8(&self) -> Result<u8, DecodeError> {
        if !self.is_empty() {
            Ok(self[0])
        } else {
            Err(DecodeError::CouldNotReadExtraBytes)
        }
    }

    fn read_u16(&self) -> Result<u16, DecodeError> {
        if self.len() >= 2 {
            Ok(((self[1] as u16) << 8) + self[0] as u16)
        } else {
            Err(DecodeError::CouldNotReadExtraBytes)
        }
    }
}

impl DataSize {
    fn try_from_encoding(encoding: u8) -> Result<DataSize, DecodeError> {
        match encoding {
            0b0 => Ok(DataSize::Byte),
            0b1 => Ok(DataSize::Word),
            _ => Err(DecodeError::InvalidDataSizeEncoding(encoding)),
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
            Err(DecodeError::InvalidModRMMode(0b00))
        );
    }
}
