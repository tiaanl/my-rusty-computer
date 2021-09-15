use crate::memory::MemoryManager;
use mrc_x86::{Instruction, OperandSet, OperandSize, OperandType, Operation, Register, Segment};
use std::cmp::Ordering;

pub type SegmentAndOffset = usize;

pub fn segment_and_offset(segment: u16, offset: u16) -> SegmentAndOffset {
    (segment << 4 + offset).into()
}

const REG_AX: usize = 0x0;
const REG_BX: usize = 0x1;
const REG_CX: usize = 0x2;
const REG_DX: usize = 0x3;
const REG_BP: usize = 0x4;
const REG_SP: usize = 0x5;
const REG_SI: usize = 0x6;
const REG_DI: usize = 0x7;

pub struct Cpu<'a> {
    registers: [u16; 8],
    segments: [u16; 4],
    ip: u16,
    flags: u16,

    memory_manager: &'a mut MemoryManager,
}

impl<'a> Cpu<'a> {
    pub fn new(memory_manager: &'a mut MemoryManager) -> Self {
        Self {
            registers: [0; 8],
            segments: [0; 4],
            ip: 0,
            flags: 0,
            memory_manager,
        }
    }

    pub fn print_registers(&mut self) {
        print!(
            "AX: {:#06X} BX: {:#06X} CX: {:#06X} DX: {:#06X} ",
            self.registers[0], self.registers[1], self.registers[2], self.registers[3]
        );
        println!(
            "SP: {:#06X} BP: {:#06X} SI: {:#06X} DI: {:#06X}",
            self.registers[4], self.registers[5], self.registers[6], self.registers[7]
        );
    }

    pub fn start(&mut self) {
        self.print_registers();
    }

    fn execute(&mut self, instruction: &Instruction) {
        println!("Executing: {}", instruction);

        match instruction.operation {
            Operation::Add => {
                if let OperandSet::DestinationAndSource(destination, source) = &instruction.operands
                {
                    let source_value = self.get_operand_value(&source.0);

                    println!("source_value: {}", source_value);

                    match &destination.0 {
                        OperandType::Register(register) => {
                            let new_value =
                                self.get_register_value(&source.1, register) + source_value;
                            self.set_register_value(&destination.1, register, new_value);
                        }
                        _ => unreachable!("not supported"),
                    }
                } else {
                    unreachable!("Invalid operand set!");
                }
            }

            _ => {
                println!("Unknown instruction!");
            }
        }
    }

    fn get_operand_value(&self, operand_type: &OperandType) -> u16 {
        match operand_type {
            OperandType::Direct(_) => todo!(),
            OperandType::Indirect(_, _) => 0,
            OperandType::Register(encoding) => match encoding {
                Register::AlAx => self.registers[REG_AX],
                Register::ClCx => self.registers[REG_BX],
                Register::DlDx => self.registers[REG_CX],
                Register::BlBx => self.registers[REG_DX],
                Register::AhSp => self.registers[REG_SP],
                Register::ChBp => self.registers[REG_BP],
                Register::DhSi => self.registers[REG_SI],
                Register::BhDi => self.registers[REG_DI],
            },
            OperandType::Segment(encoding) => match encoding {
                Segment::Es => self.segments[0],
                Segment::Cs => self.segments[1],
                Segment::Ss => self.segments[2],
                Segment::Ds => self.segments[3],
            },
            OperandType::Immediate(value) => *value,
        }
    }

    fn get_register_value(&self, operand_size: &OperandSize, register: &Register) -> u16 {
        use Register::*;

        match operand_size {
            OperandSize::Byte => match register {
                AlAx => self.registers[0] & 0x00FF,
                ClCx => self.registers[1] & 0x00FF,
                DlDx => self.registers[2] & 0x00FF,
                BlBx => self.registers[3] & 0x00FF,
                AhSp => self.registers[4] & 0xFF00,
                ChBp => self.registers[5] & 0xFF00,
                DhSi => self.registers[6] & 0xFF00,
                BhDi => self.registers[7] & 0xFF00,
            },
            OperandSize::Word => match register {
                AlAx => self.registers[0],
                ClCx => self.registers[1],
                DlDx => self.registers[2],
                BlBx => self.registers[3],
                AhSp => self.registers[4],
                ChBp => self.registers[5],
                DhSi => self.registers[6],
                BhDi => self.registers[7],
            },
        }
    }

    fn set_register_value(&mut self, data_size: &OperandSize, encoding: &Register, value: u16) {
        use Register::*;

        match data_size {
            OperandSize::Byte => match encoding {
                AlAx => self.registers[0] = (self.registers[0] & 0xFF00) + (value & 0x00FF),
                ClCx => self.registers[1] = (self.registers[1] & 0xFF00) + (value & 0x00FF),
                DlDx => self.registers[2] = (self.registers[2] & 0xFF00) + (value & 0x00FF),
                BlBx => self.registers[3] = (self.registers[3] & 0xFF00) + (value & 0x00FF),
                AhSp => {
                    self.registers[0] = (self.registers[0] & 0x00ff) + ((value & 0x00FF) << 0x08)
                }
                ChBp => {
                    self.registers[1] = (self.registers[1] & 0x00ff) + ((value & 0x00FF) << 0x08)
                }
                DhSi => {
                    self.registers[2] = (self.registers[2] & 0x00ff) + ((value & 0x00FF) << 0x08)
                }
                BhDi => {
                    self.registers[3] = (self.registers[3] & 0x00ff) + ((value & 0x00FF) << 0x08)
                }
            },
            OperandSize::Word => match encoding {
                AlAx => self.registers[0] = value,
                ClCx => self.registers[1] = value,
                DlDx => self.registers[2] = value,
                BlBx => self.registers[3] = value,
                AhSp => self.registers[4] = value,
                ChBp => self.registers[5] = value,
                DhSi => self.registers[6] = value,
                BhDi => self.registers[7] = value,
            },
        }
    }
}
