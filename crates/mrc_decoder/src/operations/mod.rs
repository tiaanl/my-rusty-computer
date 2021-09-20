use crate::errors::Result;
use crate::{it_read_byte, it_read_word, Error, LowBitsDecoder, Modrm};
use mrc_x86::{Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register};

pub mod arithmetic;
pub mod data_transfer;
pub mod logic;
pub mod processor_control;
pub mod string_manipulation;

// 1 0 0 0 0 0 s w | mod 0 0 0 r/m | data | data if sw = 01
pub(crate) fn immediate_to_register_memory<It: Iterator<Item = u8>>(
    op_code: u8,
    it: &mut It,
) -> Result<Instruction> {
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
    let modrm_byte = match it.next() {
        Some(byte) => byte,
        None => return Err(Error::CouldNotReadExtraBytes),
    };
    let modrm = Modrm::try_from_byte(modrm_byte, it)?;

    let operation = match (modrm_byte >> 3) & 0b111 {
        0b000 => Operation::Add,
        0b001 => Operation::Adc,
        0b010 => Operation::And,
        0b011 => Operation::Xor,
        0b100 => Operation::Or,
        0b101 => Operation::Sbb,
        0b110 => Operation::Sub,
        0b111 => Operation::Cmp,
        _ => unreachable!(),
    };

    Ok(Instruction::new(
        operation,
        OperandSet::DestinationAndSource(
            Operand(modrm.register_or_memory.into(), operand_size),
            Operand(
                OperandType::Immediate(match operand_size {
                    OperandSize::Byte => it_read_byte(it).unwrap().into(),
                    OperandSize::Word => it_read_word(it).unwrap(),
                }),
                operand_size,
            ),
        ),
    ))
}

// x x x x x x d w | mod reg r/m
pub(crate) fn register_memory_and_register_to_either<It: Iterator<Item = u8>>(
    operation: Operation,
    op_code: u8,
    it: &mut It,
) -> Result<Instruction> {
    let direction = (op_code >> 1) & 1;
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

    let modrm_byte = match it.next() {
        Some(byte) => byte,
        None => return Err(Error::CouldNotReadExtraBytes),
    };
    let modrm = Modrm::try_from_byte(modrm_byte, it)?;

    let destination = Operand(OperandType::Register(modrm.register), operand_size);
    let source = Operand(modrm.register_or_memory.into(), operand_size);

    Ok(Instruction::new(
        operation,
        match direction {
            0 => OperandSet::DestinationAndSource(source, destination),
            _ => OperandSet::DestinationAndSource(destination, source),
        },
    ))
}

// x x x x x x x w | data | data if w = 1
pub(crate) fn immediate_to_accumulator<It: Iterator<Item = u8>>(
    operation: Operation,
    op_code: u8,
    it: &mut It,
) -> Result<Instruction> {
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

    let mut immediate = match it.next() {
        Some(byte) => byte,
        None => return Err(Error::CouldNotReadExtraBytes),
    } as u16;

    if operand_size == OperandSize::Word {
        let next_byte = match it.next() {
            Some(byte) => byte,
            None => return Err(Error::CouldNotReadExtraBytes),
        } as u16;
        immediate |= next_byte << 8;
    }

    Ok(Instruction::new(
        operation,
        OperandSet::DestinationAndSource(
            Operand(OperandType::Register(Register::AlAx), operand_size),
            Operand(OperandType::Immediate(immediate), operand_size),
        ),
    ))
}
