mod decode;
mod errors;
mod modrm;
pub mod operations;

pub use decode::{decode_instruction, DecodeFn, DecodeFnMap};
pub use errors::{Error, Result};
pub use modrm::Modrm;
use mrc_x86::{OperandSize, Register, Segment};

trait LowBitsDecoder<T> {
    fn try_from_low_bits(bits: u8) -> Result<T>;
}

trait ByteSize<T> {
    fn byte_size(&self) -> usize;
}

impl LowBitsDecoder<Register> for Register {
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

impl LowBitsDecoder<Self> for Segment {
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

fn it_read_byte<It: Iterator<Item = u8>>(it: &mut It) -> Option<u8> {
    it.next()
}

fn it_read_word<It: Iterator<Item = u8>>(it: &mut It) -> Option<u16> {
    let first = it.next()?;
    let second = it.next()?;
    Some(((second as u16) << 8) + first as u16)
}

impl LowBitsDecoder<Self> for OperandSize {
    fn try_from_low_bits(encoding: u8) -> Result<OperandSize> {
        match encoding {
            0b0 => Ok(OperandSize::Byte),
            0b1 => Ok(OperandSize::Word),
            _ => Err(Error::InvalidDataSizeEncoding(encoding)),
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
