pub mod mod_rm;

use std::fmt;

use crate::instructions::*;
pub use mod_rm::ModRM;
use mod_rm::RegisterOrMemory;

#[derive(PartialEq, Debug)]
pub enum DecodeError {
    InvalidOpCode(u8),
    InvalidRegisterEncoding(u8),
    InvalidIndirectMemoryEncoding(u8),
    InvalidModRMEncoding(u8),
    InvalidModRMMode(u8),
    CouldNotCreateOperandFromModRMEncoding(RegisterOrMemory),
    CouldNotReadExtraBytes,
}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            DecodeError::InvalidOpCode(op_code) => write!(f, "invalid op code: {:#04x}", op_code),
            DecodeError::InvalidModRMEncoding(mod_rm_byte) => {
                write!(f, "invalid modR/M encoding: {:#04x}", mod_rm_byte)
            }
            _ => write!(f, "unknown error"),
        }
    }
}

impl RegisterEncoding {
    fn try_from_byte(byte: u8) -> Result<Self, DecodeError> {
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
}

trait ByteReader {
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

impl Operand {
    fn from_mod_rm_encoding(encoding: RegisterOrMemory) -> Result<Self, DecodeError> {
        match encoding {
            RegisterOrMemory::Indirect(encoding) => Ok(Operand::Indirect(encoding, 0)),
            RegisterOrMemory::Register(register_encoding) => {
                Ok(Operand::Register(register_encoding))
            }
            _ => Err(DecodeError::CouldNotCreateOperandFromModRMEncoding(
                encoding,
            )),
        }
    }
}

fn operands_from_mod_rm(bytes: &[u8]) -> Result<(Operand, Operand), DecodeError> {
    let ModRM(memory_or_register, register) = ModRM::try_from_bytes(bytes)?;

    Ok((
        Operand::from_mod_rm_encoding(memory_or_register)?,
        Operand::Register(register),
    ))
}

trait CodeAndExtra<'a> {
    fn code_and_extra(&'a self) -> (u8, &'a [u8]);
}

impl<'a> CodeAndExtra<'a> for &[u8] {
    fn code_and_extra(&'a self) -> (u8, &'a [u8]) {
        let (code, extra) = self.split_at(1);
        (code[0], extra)
    }
}

pub struct DecodeResult {
    pub bytes_read: usize,
    pub instruction: Instruction,
}

pub fn decode_instruction(data: &[u8]) -> Result<DecodeResult, DecodeError> {
    let (op_code, extra_bytes) = data.code_and_extra();
    // println!("op_code, extra_bytes = {} {:?}", op_code, extra_bytes);

    match op_code {
        /*
        // ADD / Add

        // 0 0 0 0 0 0 d w - Reg/Memory with Register to Either
        0b00000000 => {
            let (destination, source) = operands_from_mod_rm(extra_bytes)?;
            Ok(Instruction::new(
                Operation::Add,
                Some(DataSize::Byte),
                destination,
                source,
            ))
        }
        0b00000001 => {
            let (destination, source) = operands_from_mod_rm(extra_bytes)?;
            Ok(Instruction::new(
                Operation::Add,
                Some(DataSize::Word),
                destination,
                source,
            ))
        }
        0b00000010 => {
            let (destination, source) = operands_from_mod_rm(extra_bytes)?;
            Ok(Instruction::new(
                Operation::Add,
                Some(DataSize::Byte),
                destination,
                source,
            ))
        }
        0b00000011 => {
            let (destination, source) = operands_from_mod_rm(extra_bytes)?;
            Ok(Instruction::new(
                Operation::Add,
                Some(DataSize::Word),
                destination,
                source,
            ))
        }

        // 1 0 0 0 0 0 s w - Immediate to Register/Memory
        0b10000000 => {
            let ModRM(encoding, _) = ModRM::try_from_bytes(extra_bytes)?;
            let immediate = extra_bytes.read_u8()?;
            Ok(Instruction::new(
                Operation::Add,
                Some(DataSize::Byte),
                Operand::from_mod_rm_encoding(encoding)?,
                Operand::Immediate(immediate as u16),
            ))
        }
        0b10000001 => {
            let (mod_rm, mut extra_bytes) = extra_bytes.code_and_extra();
            let mod_rm = ModRM::try_from_mod_rm_byte(mod_rm, &mut extra_bytes)?;
            println!("{:?}", mod_rm);
            Err(DecodeError::Unknown)
        }
        0b10000010 => {
            let mod_rm = ModRM::try_from_mod_rm_byte(data[1], &mut data.as_ref())?;
            println!("{:?}", mod_rm);
            Err(DecodeError::Unknown)
        }
        0b10000011 => {
            let mod_rm = ModRM::try_from_mod_rm_byte(data[1], &mut data.as_ref())?;
            println!("{:?}", mod_rm);
            Err(DecodeError::Unknown)
        }
        */
        // JMP = Unconditional jump

        // 1 1 1 0 1 0 1 0 -> Direct intersegment
        0xEA => {
            let (offset, segment) = extra_bytes.split_at(2);
            let offset = offset.read_u16().unwrap();
            let segment = segment.read_u16().unwrap();

            Ok(DecodeResult {
                bytes_read: 5,
                instruction: Instruction {
                    operation: Operation::Jmp,
                    operands: OperandSet::SegmentAndOffset(segment, offset),
                },
            })
        }

        // Processor control

        // CLI
        // 1 1 1 1 1 0 1 0 -> Clear interrupt
        0xFA => Ok(DecodeResult {
            bytes_read: 1,
            instruction: Instruction {
                operation: Operation::Cli,
                operands: OperandSet::None,
            },
        }),
        _ => Err(DecodeError::InvalidOpCode(op_code)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_encoding_from_byte() {
        assert_eq!(
            RegisterEncoding::try_from_byte(0).unwrap(),
            RegisterEncoding::AlAx
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(1).unwrap(),
            RegisterEncoding::ClCx
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(2).unwrap(),
            RegisterEncoding::DlDx
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(3).unwrap(),
            RegisterEncoding::BlBx
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(4).unwrap(),
            RegisterEncoding::AhSp
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(5).unwrap(),
            RegisterEncoding::ChBp
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(6).unwrap(),
            RegisterEncoding::DhSi
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(7).unwrap(),
            RegisterEncoding::BhDi
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(8),
            Err(DecodeError::InvalidRegisterEncoding(8))
        );
    }
}
