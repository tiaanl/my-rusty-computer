use crate::decode::ByteAndExtra;
use crate::errors::Result;
use crate::{ByteReader, ByteSize, DecodeResult, LowBitsDecoder, Modrm};
use mrc_x86::{
    Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register, Segment,
};

// 1 0 0 0 1 0 d w | mod reg r/m
pub fn register_memory_to_from_register(op_code: u8, bytes: &[u8]) -> Result<DecodeResult> {
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
    let direction = op_code >> 1 & 0b1;

    let (modrm_byte, bytes) = bytes.byte_and_extra();
    let (modrm, _, bytes_read) = Modrm::try_from_byte(modrm_byte, bytes)?;
    assert_eq!(bytes_read, 0);

    let destination = Operand(OperandType::Register(modrm.register), operand_size);
    let source = Operand(modrm.register_or_memory.into(), operand_size);

    let operand_set = match direction {
        0 => OperandSet::DestinationAndSource(source, destination),
        _ => OperandSet::DestinationAndSource(destination, source),
    };

    Ok(DecodeResult {
        bytes_read: 1 + 1, // op_code + modrm_byte
        instruction: Instruction::new(Operation::Mov, operand_set),
    })
}

// 1 1 0 0 0 1 1 w | mod 0 0 0 r/m | data | data if w = 1
pub fn immediate_to_register_memory(op_code: u8, bytes: &[u8]) -> Result<DecodeResult> {
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

    let (modrm_byte, bytes) = bytes.byte_and_extra();
    let (modrm, bytes, bytes_read) = Modrm::try_from_byte(modrm_byte, bytes)?;

    let destination = Operand(modrm.register_or_memory.into(), operand_size);
    let immediate = match operand_size {
        OperandSize::Byte => bytes.read_u8()?.into(),
        OperandSize::Word => bytes.read_u16()?,
    };

    let source = Operand(OperandType::Immediate(immediate), operand_size);

    Ok(DecodeResult {
        bytes_read: 1 + 1 + bytes_read + operand_size.byte_size(), // op_code + modrm + data + immediate_size
        instruction: Instruction::new(
            Operation::Mov,
            OperandSet::DestinationAndSource(destination, source),
        ),
    })
}

// 1 0 1 1 w reg | data | data if w = 1
pub fn immediate_to_register(op_code: u8, bytes: &[u8]) -> Result<DecodeResult> {
    let operand_size = OperandSize::try_from_low_bits(op_code >> 3 & 0b1)?;

    let register = Operand(
        OperandType::Register(Register::try_from_low_bits(op_code & 0b111)?),
        operand_size,
    );
    let immediate = match operand_size {
        OperandSize::Byte => Operand(
            OperandType::Immediate(bytes.read_u8()?.into()),
            operand_size,
        ),
        OperandSize::Word => Operand(OperandType::Immediate(bytes.read_u16()?), operand_size),
    };

    Ok(DecodeResult {
        bytes_read: 1 + operand_size.byte_size(), // op_code + immediate_size
        instruction: Instruction::new(
            Operation::Mov,
            OperandSet::DestinationAndSource(register, immediate),
        ),
    })
}

// 1 0 1 0 0 0 0 w | addr-low | addr-high
pub fn memory_to_accumulator(op_code: u8, bytes: &[u8]) -> Result<DecodeResult> {
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

    let address = bytes.read_u16()?;

    Ok(DecodeResult {
        bytes_read: 1 + 2, // op_code + address
        instruction: Instruction::new(
            Operation::Mov,
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(Register::AlAx), operand_size),
                Operand(OperandType::Direct(address), operand_size),
            ),
        ),
    })
}

// 1 0 1 0 0 0 1 w | addr-low | addr-high
pub fn accumulator_to_memory(op_code: u8, bytes: &[u8]) -> Result<DecodeResult> {
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

    let address = bytes.read_u16()?;

    Ok(DecodeResult {
        bytes_read: 1 + 2, // op_code + address
        instruction: Instruction::new(
            Operation::Mov,
            OperandSet::DestinationAndSource(
                Operand(OperandType::Direct(address), operand_size),
                Operand(OperandType::Register(Register::AlAx), operand_size),
            ),
        ),
    })
}

// 1 0 0 0 1 1 1 0 | mod 0 sreg r/m
pub fn register_memory_to_segment_register(_: u8, bytes: &[u8]) -> Result<DecodeResult> {
    let (modrm_byte, bytes) = bytes.byte_and_extra();
    let (modrm, _, bytes_read) = Modrm::try_from_byte(modrm_byte, bytes)?;
    assert_eq!(bytes_read, 0);

    let register_or_memory = Operand(modrm.register_or_memory.into(), OperandSize::Word);

    let segment_register = Operand(
        OperandType::Segment(Segment::try_from_low_bits(modrm_byte >> 3 & 0b11)?),
        OperandSize::Word,
    );

    Ok(DecodeResult {
        bytes_read: 1 + 1, // op_code + modrm_byte
        instruction: Instruction::new(
            Operation::Mov,
            OperandSet::DestinationAndSource(segment_register, register_or_memory),
        ),
    })
}

// 1 0 0 0 1 1 0 0 | mod 0 sreg r/m
pub fn segment_register_to_register_memory(_: u8, bytes: &[u8]) -> Result<DecodeResult> {
    let (modrm_byte, bytes) = bytes.byte_and_extra();
    let (modrm, _, bytes_read) = Modrm::try_from_byte(modrm_byte, bytes)?;
    assert_eq!(bytes_read, 0);

    let register_or_memory = Operand(modrm.register_or_memory.into(), OperandSize::Word);

    let segment_register = Operand(
        OperandType::Segment(Segment::try_from_low_bits(modrm_byte >> 3 & 0b11)?),
        OperandSize::Word,
    );

    Ok(DecodeResult {
        bytes_read: 1 + 1, // op_code + modrm_byte
        instruction: Instruction::new(
            Operation::Mov,
            OperandSet::DestinationAndSource(register_or_memory, segment_register),
        ),
    })
}
