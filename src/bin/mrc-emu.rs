use mrc::instructions::*;

struct Cpu {
    registers: [u16; 16],
    segments: [u16; 4],
}

impl Cpu {
    fn new() -> Self {
        Self {
            registers: [0; 16],
            segments: [0; 4],
        }
    }

    fn print_registers(&mut self) {
        print!(
            "AX: {:#06X} BX: {:#06X} CX: {:#06X} DX: {:#06X} ",
            self.registers[0], self.registers[1], self.registers[2], self.registers[3]
        );
        println!(
            "SP: {:#06X} BP: {:#06X} SI: {:#06X} DI: {:#06X}",
            self.registers[4], self.registers[5], self.registers[6], self.registers[7]
        );
    }

    fn execute(&mut self, instruction: &Instruction) {
        println!("Executing: {:?}", instruction);

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
                Register::AlAx => self.registers[0],
                Register::ClCx => self.registers[1],
                Register::DlDx => self.registers[2],
                Register::BlBx => self.registers[3],
                Register::AhSp => self.registers[4],
                Register::ChBp => self.registers[5],
                Register::DhSi => self.registers[6],
                Register::BhDi => self.registers[7],
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

fn main() {
    let mut cpu = Cpu::new();

    let instructions = vec![
        Instruction::new(
            Operation::Add,
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
                Operand(OperandType::Immediate(10), OperandSize::Word),
            ),
        ),
        Instruction::new(
            Operation::Add,
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
                Operand(OperandType::Immediate(10), OperandSize::Word),
            ),
        ),
        Instruction::new(
            Operation::Add,
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(Register::AhSp), OperandSize::Byte),
                Operand(OperandType::Immediate(0xB0), OperandSize::Byte),
            ),
        ),
        Instruction::new(
            Operation::Add,
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(Register::AlAx), OperandSize::Byte),
                Operand(OperandType::Immediate(0x01), OperandSize::Byte),
            ),
        ),
    ];

    cpu.print_registers();

    for instruction in instructions {
        cpu.execute(&instruction);
        cpu.print_registers();
    }
}
