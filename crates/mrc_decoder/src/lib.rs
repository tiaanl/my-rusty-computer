mod common;
mod decode;
mod errors;
mod mrrm;
mod traits;

pub use decode::decode_instruction;
pub use errors::{DecodeError, Result};
pub use mrrm::{ModRegRM, RegisterOrMemory};

use std::fmt::{Display, Formatter};
use crate::traits::TryFromEncoding;
use mrc_instruction::{Address, Displacement, Instruction, OperandSet, Register, RelativeToAddress, Segment};

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

pub struct DecodedInstruction<'a> {
    address: Address,
    bytes: &'a [u8],
    instruction: Instruction,
    size: u8,
}

impl<'a> DecodedInstruction<'a> {
    pub fn new(address: Address, bytes: &'a [u8], instruction: Instruction, size: u8) -> Self {
        Self {
            address,
            bytes,
            instruction,
            size,
        }
    }
}

const BYTES_TO_PRINT: usize = 7;

impl<'a> Display for DecodedInstruction<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // address
        write!(f, "{}  ", self.address)?;

        // bytes
        let mut b: String = self
            .bytes
            .iter()
            .take(BYTES_TO_PRINT)
            .map(|b| format!("{:02X} ", b))
            .collect();

        for _ in self.bytes.len()..BYTES_TO_PRINT {
            b.push_str("   ");
        }

        write!(f, "{}  ", b)?;

        match self.instruction.operands {
            OperandSet::None => write!(f, "{}", self.instruction.operation)?,
            _ => write!(f, "{} ", self.instruction.operation)?,
        }

        match self.instruction.operands {
            OperandSet::Displacement(displacement) => match displacement {
                Displacement::None => write!(f, "{}", (self.size as u32).relative_to(&self.address)),
                Displacement::Byte(displacement) => {
                    let mut offset = self.address.offset.wrapping_add(self.size as u16);
                    offset = if displacement < 0 {
                        if let Some(neg) = displacement.checked_neg() {
                            offset.wrapping_sub(neg as u16)
                        } else {
                            offset.wrapping_sub(128)
                        }
                    } else {
                        offset.wrapping_add(displacement as u16)
                    };

                    write!(f, "{:#06X}", offset)
                }
                Displacement::Word(displacement) => {
                    let mut offset = self.address.offset.wrapping_add(self.size as u16);
                    offset = if displacement < 0 {
                        offset.wrapping_sub(displacement.abs() as u16)
                    } else {
                        offset.wrapping_add(displacement.abs() as u16)
                    };

                    write!(f, "{:#06X}", offset)
                }
            },

            OperandSet::None => Ok(()),

            _ => write!(f, "{}", self.instruction.operands),
        }
    }
}
