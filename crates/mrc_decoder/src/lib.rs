pub mod operations;

mod decode;
mod errors;
mod mod_reg_rm;

#[cfg(feature = "alternate-decoder")]
mod decode2;

#[cfg(not(feature = "alternate-decoder"))]
pub use decode::decode_instruction;

#[cfg(feature = "alternate-decoder")]
pub use decode2::decode_instruction;

pub use errors::{DecodeError, Result};
pub use mod_reg_rm::ModRegRM;
use mrc_instruction::{OperandSize, Register, Segment};

trait TryFromByte<T> {
    fn try_from_byte(byte: u8) -> Result<T>;
}

trait ByteSize<T> {
    fn byte_size(&self) -> usize;
}

impl TryFromByte<Register> for Register {
    fn try_from_byte(byte: u8) -> Result<Self> {
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
            _ => Err(DecodeError::InvalidRegisterEncoding(byte)),
        }
    }
}

impl TryFromByte<Self> for Segment {
    fn try_from_byte(byte: u8) -> Result<Self> {
        match byte {
            0b00 => Ok(Self::ES),
            0b01 => Ok(Self::CS),
            0b10 => Ok(Self::SS),
            0b11 => Ok(Self::DS),
            _ => Err(DecodeError::InvalidSegmentEncoding(byte)),
        }
    }
}

pub trait ByteReader {
    fn read_u8(&self) -> Result<u8>;
    fn read_u16(&self) -> Result<u16>;
}

impl ByteReader for &[u8] {
    fn read_u8(&self) -> Result<u8> {
        self.get(0)
            .map_or(Err(DecodeError::CouldNotReadExtraBytes), |b| Ok(*b))
    }

    fn read_u16(&self) -> Result<u16> {
        let low = self.read_u8()?;
        let high = self.read_u8()?;
        Ok(u16::from_le_bytes([low, high]))
    }
}

#[inline]
fn it_read_byte(it: &mut impl Iterator<Item = u8>) -> Result<u8> {
    it.next()
        .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)
}

#[inline]
fn it_read_word(it: &mut impl Iterator<Item = u8>) -> Result<u16> {
    let first = it_read_byte(it)?;
    let second = it_read_byte(it)?;
    Ok(u16::from_le_bytes([first, second]))
}

impl TryFromByte<Self> for OperandSize {
    fn try_from_byte(byte: u8) -> Result<OperandSize> {
        match byte {
            0b0 => Ok(OperandSize::Byte),
            0b1 => Ok(OperandSize::Word),
            _ => Err(DecodeError::InvalidDataSizeEncoding(byte)),
        }
    }
}

impl ByteSize<Self> for OperandSize {
    fn byte_size(&self) -> usize {
        match self {
            OperandSize::Byte => 1,
            OperandSize::Word => 2,
        }
    }
}
