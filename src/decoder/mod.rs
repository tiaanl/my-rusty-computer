pub mod mod_rm;

use std::fmt;

use crate::instructions::*;
pub use mod_rm::ModRM;
use mod_rm::RegisterOrMemory;

#[derive(PartialEq, Debug)]
pub enum DecodeError {
    CouldNotCreateOperandFromModRMEncoding(RegisterOrMemory),
    CouldNotReadExtraBytes,
    InvalidDataSizeEncoding(u8),
    InvalidIndirectMemoryEncoding(u8),
    InvalidModRMEncoding(u8),
    InvalidModRMMode(u8),
    InvalidOpCode(u8),
    InvalidRegisterEncoding(u8),
    InvalidSegmentEncoding(u8),
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

trait CodeAndExtra<'a> {
    fn byte_and_extra(&'a self) -> (u8, &'a [u8]);
}

impl<'a> CodeAndExtra<'a> for &[u8] {
    fn byte_and_extra(&'a self) -> (u8, &'a [u8]) {
        let (code, extra) = self.split_at(1);
        (code[0], extra)
    }
}

pub struct DecodeResult {
    pub bytes_read: usize,
    pub instruction: Instruction,
}

pub fn decode_instruction(data: &[u8]) -> Result<DecodeResult, DecodeError> {
    let (op_code, extra_bytes) = data.byte_and_extra();
    // println!("op_code, extra_bytes = {} {:?}", op_code, extra_bytes);

    match op_code {
        // Data transfer
        //

        // MOV -> Move
        //
        0xB8 => {
            // 1 0 1 1 w reg => Immediate to register
            let data_size = DataSize::try_from_encoding(op_code >> 3 & 0b1)?;
            let destination = Operand::Register(RegisterEncoding::try_from_byte(op_code & 0b111)?);
            let source = Operand::Immediate(match data_size {
                DataSize::Byte => extra_bytes.read_u8()? as u16,
                DataSize::Word => extra_bytes.read_u16()?,
            });

            Ok(DecodeResult {
                bytes_read: 1 + data_size.in_bytes(),
                instruction: Instruction {
                    operation: Operation::Mov,
                    operands: OperandSet::DestinationAndSource(destination, source, data_size),
                },
            })
        }
        0x8E => {
            // Register/memory to segment register
            // 1 0 0 0 1 1 1 0 | mod 0 segment r/m

            let (mod_rm_byte, extra_bytes) = extra_bytes.byte_and_extra();
            let segment_encoding = mod_rm_byte >> 3 & 0b11;
            let destination: Operand =
                Operand::Segment(SegmentEncoding::try_from_encoding(segment_encoding)?);
            let register_or_memory = RegisterOrMemory::try_from(mod_rm_byte, extra_bytes)?;

            Ok(DecodeResult {
                bytes_read: 2,
                instruction: Instruction {
                    operation: Operation::Mov,
                    operands: OperandSet::DestinationAndSource(
                        destination,
                        register_or_memory.into(),
                        DataSize::Word,
                    ),
                },
            })
        }

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
