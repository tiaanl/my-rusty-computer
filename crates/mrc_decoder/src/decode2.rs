#![allow(dead_code)]

use crate::decode::{
    displacement_byte_from_it, displacement_word_from_it, immediate_operand_from_it,
};
use crate::{it_read_byte, it_read_word, DecodeError, ModRegRM, TryFromByte};
use mrc_instruction::data::{OperandType, OperationMap};
use mrc_instruction::{
    Instruction, Operand, OperandKind, OperandSet, OperandSize, Operation, Register, Repeat,
    Segment,
};
use std::collections::HashMap;

#[derive(Debug)]
struct DecodeMap {
    op_code: u8,
    operation_map: mrc_instruction::data::OperationMap,
    destination: mrc_instruction::data::OperandType,
    source: mrc_instruction::data::OperandType,
}

impl DecodeMap {
    fn operation(&self, _op_code: u8, modrm: Option<u8>) -> Operation {
        match self.operation_map {
            OperationMap::Single(operation) => operation,
            OperationMap::ModrmReg(group) => {
                if let Some(index) = modrm {
                    let index = index >> 3 & 0b111;
                    group[index as usize]
                } else {
                    panic!("Special Group must have a modrm byte")
                }
            }
        }
    }
}

fn prepare_data() -> HashMap<u8, DecodeMap> {
    let mut result: HashMap<u8, DecodeMap> = HashMap::with_capacity(256);

    for id in mrc_instruction::data::INSTRUCTION_DATA.iter() {
        result.insert(
            id.0,
            DecodeMap {
                op_code: id.0,
                operation_map: id.1,
                destination: id.2,
                source: id.3,
            },
        );
    }

    result
}

fn is_destination_and_source_from_mod_rm(destination: OperandType, source: OperandType) -> bool {
    matches!(
        destination,
        OperandType::Reg8 | OperandType::Reg16 | OperandType::RegMem8 | OperandType::RegMem16
    ) || matches!(
        source,
        OperandType::Reg8 | OperandType::Reg16 | OperandType::RegMem8 | OperandType::RegMem16
    )
}

pub fn decode_instruction(it: &mut impl Iterator<Item = u8>) -> crate::Result<Instruction> {
    let op_code = it_read_byte(it)?;

    if matches!(op_code, 0x26 | 0x2E | 0x36 | 0x3E) {
        fn override_segment(operand: &mut Operand, segment: Segment) {
            match operand.0 {
                OperandKind::Direct(ref mut seg, ..) | OperandKind::Indirect(ref mut seg, ..) => {
                    *seg = segment
                }
                _ => {}
            }
        }

        let segment = Segment::try_from_byte((op_code >> 3) & 0b11)?;

        let mut instruction = decode_instruction(it)?;

        match instruction.operands {
            OperandSet::DestinationAndSource(ref mut destination, ref mut source) => {
                override_segment(destination, segment);
                override_segment(source, segment);
            }

            OperandSet::Destination(ref mut destination) => {
                override_segment(destination, segment);
            }

            _ => {}
        }

        return Ok(instruction);
    }

    if matches!(op_code, 0xF2 | 0xF3) {
        let mut instruction = decode_instruction(it)?;
        match op_code {
            0xF2 => instruction.repeat = Some(Repeat::NotEqual),
            0xF3 => instruction.repeat = Some(Repeat::Equal),
            _ => unreachable!(),
        }
        return Ok(instruction);
    }

    let data = prepare_data();
    if let Some(decode_map) = data.get(&op_code) {
        match (decode_map.destination, decode_map.source) {
            (OperandType::RegMem8, OperandType::Reg8) => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                let destination = Operand(OperandKind::Register(modrm.register), OperandSize::Byte);
                let source = Operand(modrm.register_or_memory.into(), OperandSize::Byte);

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(destination, source),
                ))
            }

            (OperandType::RegMem16, OperandType::Reg16) => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                let destination = Operand(OperandKind::Register(modrm.register), OperandSize::Word);
                let source = Operand(modrm.register_or_memory.into(), OperandSize::Word);

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(destination, source),
                ))
            }

            (OperandType::Reg8, OperandType::RegMem8) => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                let destination = Operand(modrm.register_or_memory.into(), OperandSize::Byte);
                let source = Operand(OperandKind::Register(modrm.register), OperandSize::Byte);

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(destination, source),
                ))
            }

            (OperandType::Reg16, OperandType::RegMem16) => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                let destination = Operand(modrm.register_or_memory.into(), OperandSize::Word);
                let source = Operand(OperandKind::Register(modrm.register), OperandSize::Word);

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(destination, source),
                ))
            }

            (OperandType::OpCodeReg, OperandType::None) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                OperandSet::Destination(Operand(
                    OperandKind::Register(Register::try_from_byte(op_code & 0b111)?),
                    OperandSize::Word,
                )),
            )),

            (OperandType::AL, OperandType::Imm8) => {
                let immediate = it_read_byte(it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, None),
                    OperandSet::DestinationAndSource(
                        Operand(OperandKind::Register(Register::AlAx), OperandSize::Byte),
                        Operand(
                            OperandKind::Immediate(Immediate::Byte(immediate)),
                            OperandSize::Byte,
                        ),
                    ),
                ))
            }

            (OperandType::Imm8, OperandType::AL) => {
                let immediate = it_read_byte(it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, None),
                    OperandSet::DestinationAndSource(
                        Operand(OperandKind::Immediate(immediate as u16), OperandSize::Byte),
                        Operand(OperandKind::Register(Register::AlAx), OperandSize::Byte),
                    ),
                ))
            }

            (OperandType::AX, OperandType::Imm8) => {
                let immediate = it_read_byte(it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, None),
                    OperandSet::DestinationAndSource(
                        Operand(OperandKind::Register(Register::AlAx), OperandSize::Word),
                        Operand(OperandKind::Immediate(immediate as u16), OperandSize::Byte),
                    ),
                ))
            }

            (OperandType::Imm8, OperandType::AX) => {
                let immediate = it_read_byte(it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, None),
                    OperandSet::DestinationAndSource(
                        Operand(OperandKind::Immediate(immediate as u16), OperandSize::Byte),
                        Operand(OperandKind::Register(Register::AlAx), OperandSize::Word),
                    ),
                ))
            }

            (OperandType::AX, OperandType::Imm16) => {
                let immediate = it_read_word(it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, None),
                    OperandSet::DestinationAndSource(
                        Operand(OperandKind::Register(Register::AlAx), OperandSize::Word),
                        Operand(OperandKind::Immediate(immediate), OperandSize::Word),
                    ),
                ))
            }

            (OperandType::SegReg, OperandType::None) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                OperandSet::Destination(Operand(
                    OperandKind::Segment(Segment::try_from_byte(op_code >> 3 & 0b111)?),
                    OperandSize::Word,
                )),
            )),

            (OperandType::None, OperandType::None) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                OperandSet::None,
            )),

            (OperandType::Displacement8, OperandType::None) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                displacement_byte_from_it(it)?,
            )),

            (OperandType::Displacement16, OperandType::None) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                displacement_word_from_it(it)?,
            )),

            (OperandType::Reg8, OperandType::Imm8) => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(
                        Operand(modrm.register_or_memory.into(), OperandSize::Byte),
                        immediate_operand_from_it(it, OperandSize::Byte)?,
                    ),
                ))
            }

            (OperandType::Reg16, OperandType::Imm8) => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(
                        Operand(modrm.register_or_memory.into(), OperandSize::Word),
                        immediate_operand_from_it(it, OperandSize::Byte)?,
                    ),
                ))
            }

            (OperandType::Reg16, OperandType::Imm16) => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(
                        Operand(modrm.register_or_memory.into(), OperandSize::Word),
                        immediate_operand_from_it(it, OperandSize::Word)?,
                    ),
                ))
            }

            (OperandType::Reg16, OperandType::SignedImm8) => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                // TODO: The source imm needs to be sign extended.
                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(
                        Operand(modrm.register_or_memory.into(), OperandSize::Word),
                        immediate_operand_from_it(it, OperandSize::Byte)?,
                    ),
                ))
            }

            (OperandType::Reg16, OperandType::SegReg) => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(
                        Operand(modrm.register_or_memory.into(), OperandSize::Word),
                        Operand(
                            OperandKind::Segment(Segment::try_from_byte(
                                (modrm_byte >> 3) & 0b111,
                            )?),
                            OperandSize::Word,
                        ),
                    ),
                ))
            }

            (OperandType::SegReg, OperandType::Reg16) => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(
                        Operand(
                            OperandKind::Segment(Segment::try_from_byte(
                                (modrm_byte >> 3) & 0b111,
                            )?),
                            OperandSize::Word,
                        ),
                        Operand(modrm.register_or_memory.into(), OperandSize::Word),
                    ),
                ))
            }

            (OperandType::RegMem16, OperandType::None) => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::Destination(Operand(
                        modrm.register_or_memory.into(),
                        OperandSize::Word,
                    )),
                ))
            }

            (OperandType::AX, OperandType::OpCodeReg) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                OperandSet::DestinationAndSource(
                    Operand(OperandKind::Register(Register::AlAx), OperandSize::Word),
                    Operand(
                        OperandKind::Register(Register::try_from_byte(op_code & 0b111)?),
                        OperandSize::Word,
                    ),
                ),
            )),

            (OperandType::MemFar, OperandType::None) => {
                let offset = it_read_word(it)?;
                let segment = it_read_word(it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, None),
                    OperandSet::SegmentAndOffset(segment, offset),
                ))
            }

            (OperandType::Mem8, OperandType::AL) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                OperandSet::DestinationAndSource(
                    Operand(
                        OperandKind::Direct(Segment::DS, it_read_word(it)?),
                        OperandSize::Byte,
                    ),
                    Operand(OperandKind::Register(Register::AlAx), OperandSize::Byte),
                ),
            )),

            (OperandType::Mem16, OperandType::AX) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                OperandSet::DestinationAndSource(
                    Operand(
                        OperandKind::Direct(Segment::DS, it_read_word(it)?),
                        OperandSize::Word,
                    ),
                    Operand(OperandKind::Register(Register::AlAx), OperandSize::Word),
                ),
            )),

            (OperandType::AL, OperandType::Mem8) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                OperandSet::DestinationAndSource(
                    Operand(OperandKind::Register(Register::AlAx), OperandSize::Byte),
                    Operand(
                        OperandKind::Direct(Segment::DS, it_read_word(it)?),
                        OperandSize::Byte,
                    ),
                ),
            )),

            (OperandType::AX, OperandType::Mem16) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                OperandSet::DestinationAndSource(
                    Operand(OperandKind::Register(Register::AlAx), OperandSize::Word),
                    Operand(
                        OperandKind::Direct(Segment::DS, it_read_word(it)?),
                        OperandSize::Word,
                    ),
                ),
            )),

            (OperandType::OpCodeReg, OperandType::Imm8) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                OperandSet::DestinationAndSource(
                    Operand(
                        OperandKind::Register(Register::try_from_byte(op_code & 0b111)?),
                        OperandSize::Byte,
                    ),
                    Operand(
                        OperandKind::Immediate(it_read_byte(it)? as u16),
                        OperandSize::Byte,
                    ),
                ),
            )),

            (OperandType::OpCodeReg, OperandType::Imm16) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                OperandSet::DestinationAndSource(
                    Operand(
                        OperandKind::Register(Register::try_from_byte(op_code & 0b111)?),
                        OperandSize::Word,
                    ),
                    Operand(OperandKind::Immediate(it_read_word(it)?), OperandSize::Word),
                ),
            )),

            (OperandType::Imm16, OperandType::None) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                OperandSet::Destination(immediate_operand_from_it(it, OperandSize::Word)?),
            )),

            (OperandType::Imm8, OperandType::None) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                OperandSet::Destination(immediate_operand_from_it(it, OperandSize::Byte)?),
            )),

            (OperandType::Reg8, OperandType::Imm1) => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(
                        Operand(modrm.register_or_memory.into(), OperandSize::Byte),
                        Operand(OperandKind::Immediate(1), OperandSize::Byte),
                    ),
                ))
            }

            (OperandType::Reg16, OperandType::Imm1) => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(
                        Operand(modrm.register_or_memory.into(), OperandSize::Word),
                        Operand(OperandKind::Immediate(1), OperandSize::Byte),
                    ),
                ))
            }

            (OperandType::Reg8, cl_or_cx)
                if cl_or_cx == OperandType::CL || cl_or_cx == OperandType::CX =>
            {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                let operand_size = match cl_or_cx {
                    OperandType::CL => OperandSize::Byte,
                    OperandType::CX => OperandSize::Word,
                    _ => unreachable!(),
                };

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(
                        Operand(modrm.register_or_memory.into(), OperandSize::Byte),
                        Operand(OperandKind::Register(Register::ClCx), operand_size),
                    ),
                ))
            }

            (OperandType::Reg16, cl_or_cx)
                if cl_or_cx == OperandType::CL || cl_or_cx == OperandType::CX =>
            {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                let operand_size = match cl_or_cx {
                    OperandType::CL => OperandSize::Byte,
                    OperandType::CX => OperandSize::Word,
                    _ => unreachable!(),
                };

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::DestinationAndSource(
                        Operand(modrm.register_or_memory.into(), OperandSize::Word),
                        Operand(OperandKind::Register(Register::ClCx), operand_size),
                    ),
                ))
            }

            (reg, OperandType::None) if reg == OperandType::Reg8 || reg == OperandType::Reg16 => {
                let modrm_byte = it
                    .next()
                    .map_or(Err(DecodeError::CouldNotReadExtraBytes), Ok)?;
                let modrm = ModRegRM::try_from_byte(modrm_byte, it)?;

                Ok(Instruction::new(
                    decode_map.operation(op_code, Some(modrm_byte)),
                    OperandSet::Destination(Operand(
                        modrm.register_or_memory.into(),
                        match reg {
                            OperandType::Reg8 => OperandSize::Byte,
                            OperandType::Reg16 => OperandSize::Word,
                            _ => unreachable!(),
                        },
                    )),
                ))
            }

            (al_or_ax, OperandType::DX)
                if al_or_ax == OperandType::AL || al_or_ax == OperandType::AX =>
            {
                Ok(Instruction::new(
                    decode_map.operation(op_code, None),
                    OperandSet::DestinationAndSource(
                        Operand(
                            OperandKind::Register(Register::AlAx),
                            match al_or_ax {
                                OperandType::AL => OperandSize::Byte,
                                OperandType::AX => OperandSize::Word,
                                _ => unreachable!(),
                            },
                        ),
                        Operand(OperandKind::Register(Register::DlDx), OperandSize::Word),
                    ),
                ))
            }

            (OperandType::DX, al_or_ax) => Ok(Instruction::new(
                decode_map.operation(op_code, None),
                OperandSet::DestinationAndSource(
                    Operand(OperandKind::Register(Register::DlDx), OperandSize::Word),
                    Operand(
                        OperandKind::Register(Register::AlAx),
                        match al_or_ax {
                            OperandType::AL => OperandSize::Byte,
                            OperandType::AX => OperandSize::Word,
                            _ => unreachable!(),
                        },
                    ),
                ),
            )),

            _ => panic!(
                "Invalid operand pair ({:?}, {:?})",
                decode_map.destination, decode_map.source
            ),
        }
    } else {
        panic!("Invalid op_code");
    }
}
