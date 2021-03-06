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
        // match instruction.operation {
        //     Operation::Add => {
        //         // Get the source value.
        //         let source_value = self.get_source_value(&instruction.operands);
        //                         match &instruction.destination {
        //             Operand::Register(encoding) => {
        //                 self.set_register_value(&instruction.data_size, encoding, source_value)
        //             }
        //             _ => panic!(),
        //         }
        //     }
        // }
    }

    fn get_operand_value(&self, operand: &Operand) -> u16 {
        match operand {
            Operand::Direct(_) => todo!(),
            Operand::Indirect(_, _) => 0,
            Operand::Register(encoding) => match encoding {
                RegisterEncoding::AlAx => self.registers[0],
                RegisterEncoding::ClCx => self.registers[1],
                RegisterEncoding::DlDx => self.registers[2],
                RegisterEncoding::BlBx => self.registers[3],
                RegisterEncoding::AhSp => self.registers[4],
                RegisterEncoding::ChBp => self.registers[5],
                RegisterEncoding::DhSi => self.registers[6],
                RegisterEncoding::BhDi => self.registers[7],
            },
            Operand::Segment(encoding) => match encoding {
                SegmentEncoding::Es => self.segments[0],
                SegmentEncoding::Cs => self.segments[1],
                SegmentEncoding::Ss => self.segments[2],
                SegmentEncoding::Ds => self.segments[3],
            },
            Operand::Immediate(value) => *value,
        }
    }

    fn set_register_value(
        &mut self,
        data_size: &DataSize,
        encoding: &RegisterEncoding,
        value: u16,
    ) {
        match data_size {
            DataSize::Byte => match encoding {
                RegisterEncoding::AlAx => {
                    self.registers[0] = (self.registers[0] & 0xFF00) + (value & 0x00FF)
                }
                RegisterEncoding::ClCx => {
                    self.registers[1] = (self.registers[1] & 0xFF00) + (value & 0x00FF)
                }
                RegisterEncoding::DlDx => {
                    self.registers[2] = (self.registers[2] & 0xFF00) + (value & 0x00FF)
                }
                RegisterEncoding::BlBx => {
                    self.registers[3] = (self.registers[3] & 0xFF00) + (value & 0x00FF)
                }
                RegisterEncoding::AhSp => {
                    self.registers[0] = (self.registers[0] & 0x00ff) + ((value & 0x00FF) << 0x08)
                }
                RegisterEncoding::ChBp => {
                    self.registers[1] = (self.registers[1] & 0x00ff) + ((value & 0x00FF) << 0x08)
                }
                RegisterEncoding::DhSi => {
                    self.registers[2] = (self.registers[2] & 0x00ff) + ((value & 0x00FF) << 0x08)
                }
                RegisterEncoding::BhDi => {
                    self.registers[3] = (self.registers[3] & 0x00ff) + ((value & 0x00FF) << 0x08)
                }
            },
            DataSize::Word => match encoding {
                RegisterEncoding::AlAx => self.registers[0] = value,
                RegisterEncoding::ClCx => self.registers[1] = value,
                RegisterEncoding::DlDx => self.registers[2] = value,
                RegisterEncoding::BlBx => self.registers[3] = value,
                RegisterEncoding::AhSp => self.registers[4] = value,
                RegisterEncoding::ChBp => self.registers[5] = value,
                RegisterEncoding::DhSi => self.registers[6] = value,
                RegisterEncoding::BhDi => self.registers[7] = value,
            },
        }
    }
}

fn main() {
    let mut cpu = Cpu::new();

    cpu.execute(&Instruction::new(
        Operation::Add,
        OperandSet::DestinationAndSource(
            Operand::Register(RegisterEncoding::AlAx),
            Operand::Immediate(10),
            DataSize::Word,
        ),
    ));

    cpu.print_registers();
    cpu.execute(&Instruction::new(
        Operation::Add,
        OperandSet::DestinationAndSource(
            Operand::Register(RegisterEncoding::AlAx),
            Operand::Immediate(10),
            DataSize::Word,
        ),
    ));

    cpu.print_registers();
    cpu.execute(&Instruction::new(
        Operation::Add,
        OperandSet::DestinationAndSource(
            Operand::Register(RegisterEncoding::AhSp),
            Operand::Immediate(0xB0),
            DataSize::Byte,
        ),
    ));

    cpu.print_registers();
    cpu.execute(&Instruction::new(
        Operation::Add,
        OperandSet::DestinationAndSource(
            Operand::Register(RegisterEncoding::AlAx),
            Operand::Immediate(0x01),
            DataSize::Byte,
        ),
    ));

    cpu.print_registers();
}
