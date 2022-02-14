use crate::{DecodeError, ModRegRM, Result};
use mrc_instruction::{Displacement, Immediate, OperandSize};

pub trait OpCodeExt {
    fn operand_size(self) -> OperandSize;
}

impl OpCodeExt for u8 {
    fn operand_size(self) -> OperandSize {
        if self & 1 == 0 {
            OperandSize::Byte
        } else {
            OperandSize::Word
        }
    }
}

pub trait ReadExt {
    fn read_u8(&mut self) -> Result<u8>;

    fn read_u16(&mut self) -> Result<u16> {
        Ok(u16::from_le_bytes([self.read_u8()?, self.read_u8()?]))
    }

    fn read_mrrm(&mut self) -> Result<(ModRegRM, u8)>;

    fn read_immediate(&mut self, operand_size: OperandSize) -> Result<Immediate>;

    fn read_displacement(&mut self, operand_size: OperandSize) -> Result<Displacement>;
}

impl<T: Iterator<Item = u8>> ReadExt for T {
    fn read_u8(&mut self) -> Result<u8> {
        if let Some(byte) = self.next() {
            Ok(byte)
        } else {
            Err(DecodeError::CouldNotReadExtraBytes)
        }
    }

    fn read_mrrm(&mut self) -> Result<(ModRegRM, u8)> {
        let mrrm_byte = self.read_u8()?;
        let mrrm = ModRegRM::try_from_byte(mrrm_byte, self)?;

        Ok((mrrm, mrrm_byte))
    }

    fn read_immediate(&mut self, operand_size: OperandSize) -> Result<Immediate> {
        Ok(match operand_size {
            OperandSize::Byte => Immediate::Byte(self.read_u8()?),
            OperandSize::Word => Immediate::Word(self.read_u16()?),
        })
    }

    fn read_displacement(&mut self, operand_size: OperandSize) -> Result<Displacement> {
        Ok(match operand_size {
            OperandSize::Byte => Displacement::Byte(self.read_u8()? as i8),
            OperandSize::Word => Displacement::Word(self.read_u16()? as i16),
        })
    }
}
