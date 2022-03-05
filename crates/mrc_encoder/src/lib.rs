#![allow(unused)]

use crate::EncodeError::InvalidOperands;
use mrc_decoder::{ModRegRM, RegisterOrMemory};
use mrc_instruction::{
    Displacement, Instruction, Operand, OperandSet, OperandSize, Operation, Register, Segment,
    SizedRegister,
};
use std::convert::Infallible;
use std::hash::Hasher;

#[derive(Debug)]
pub enum EncodeError {
    InvalidOperands,
}

pub fn encode_instruction(instruction: &Instruction) -> Result<Vec<u8>, EncodeError> {
    match *instruction {
        Instruction {
            operation,
            operands:
                OperandSet::DestinationAndSource(
                    Operand::Register(SizedRegister(destination, operand_size)),
                    source,
                ),
            ..
        } if operand_size == source.operand_size() => {
            let op_code = match operation {
                Operation::ADD if operand_size == OperandSize::Byte => 0x00,
                Operation::ADD if operand_size == OperandSize::Word => 0x01,
                _ => return Err(EncodeError::InvalidOperands),
            };
            encode_mod_reg_rm(op_code, destination, &source, operand_size)
        }

        Instruction {
            operation,
            operands:
                OperandSet::DestinationAndSource(
                    destination,
                    Operand::Register(SizedRegister(source, operand_size)),
                ),
            ..
        } if operand_size == destination.operand_size() => {
            let op_code = match operation {
                Operation::ADD if operand_size == OperandSize::Byte => 0x01,
                Operation::ADD if operand_size == OperandSize::Word => 0x02,
                _ => return Err(EncodeError::InvalidOperands),
            };
            encode_mod_reg_rm(op_code, source, &destination, operand_size)
        }

        _ => Err(EncodeError::InvalidOperands),
    }
}

fn encode_mod_reg_rm(
    op_code: u8,
    destination: Register,
    source: &Operand,
    operand_size: OperandSize,
) -> Result<Vec<u8>, EncodeError> {
    let mut result = vec![];
    let mut segment_override = None;

    let register_or_memory = match *source {
        Operand::Direct(segment, offset, _) => {
            if segment != Segment::DS {
                segment_override = Some(segment);
            }
            RegisterOrMemory::Direct(offset)
        }

        Operand::Indirect(segment, addressing_mode, displacement, _) => {
            if segment != Segment::DS {
                segment_override = Some(segment);
            }

            match displacement {
                Displacement::None => RegisterOrMemory::Indirect(addressing_mode),

                Displacement::Byte(displacement) => {
                    RegisterOrMemory::DisplacementByte(addressing_mode, displacement)
                }

                Displacement::Word(displacement) => {
                    RegisterOrMemory::DisplacementWord(addressing_mode, displacement)
                }
            }
        }

        Operand::Register(SizedRegister(register, _)) => RegisterOrMemory::Register(register),

        _ => return Err(InvalidOperands),
    };

    if let Some(segment) = segment_override {
        result.push(segment_override_byte_for(segment));
    }

    result.push(op_code);

    // result.push(ModRegRM::new(destination, register_or_memory).as_byte());

    Ok(result)
}

fn segment_override_byte_for(segment: Segment) -> u8 {
    match segment {
        Segment::ES => 0x26,
        Segment::CS => 0x2E,
        Segment::SS => 0x36,
        Segment::DS => 0x3E,
    }
}

fn encode_immediate_byte(bytes: &mut Vec<u8>, value: u8) {
    bytes.push(value);
}

fn encode_immediate_word(bytes: &mut Vec<u8>, value: u16) {
    for byte in value.to_le_bytes() {
        bytes.push(byte)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use mrc_instruction::{Immediate, Operand, OperandSet, Operation, Register};

    // #[test]
    fn test_add_reg8_reg_mem_8() {
        let result = encode_instruction(&Instruction::new(
            Operation::ADD,
            OperandSet::DestinationAndSource(
                Operand::Register(SizedRegister(Register::AlAx, OperandSize::Byte)),
                Operand::Register(SizedRegister(Register::ClCx, OperandSize::Byte)),
            ),
        ))
        .unwrap();

        assert_eq!(result.as_slice(), &[0x00, 0xC1]);

        let result = encode_instruction(&Instruction::new(
            Operation::ADD,
            OperandSet::DestinationAndSource(
                Operand::Register(SizedRegister(Register::AlAx, OperandSize::Byte)),
                Operand::Direct(Segment::DS, 0x0010, OperandSize::Byte),
            ),
        ))
        .unwrap();

        assert_eq!(result.as_slice(), &[0x00, 0x06, 0x10, 0x00]);
    }
}
