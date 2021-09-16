use crate::memory::{MemoryInterface, MemoryManager};
use mrc_decoder::DataIterator;
use mrc_x86::{
    Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register, Segment,
};
use std::cell::RefCell;
use std::rc::Rc;

pub type SegmentAndOffset = u32;

pub fn segment_and_offset(segment: u16, offset: u16) -> SegmentAndOffset {
    ((segment << 4) + offset).into()
}

pub fn into_segment_and_offset(addr: usize) -> (u16, u16) {
    let offset = (addr & 0x00FF) as u16;
    let segment = (addr >> 16) as u16;
    (segment, offset)
}

const REG_AX: usize = 0x0;
const REG_BX: usize = 0x1;
const REG_CX: usize = 0x2;
const REG_DX: usize = 0x3;
const REG_BP: usize = 0x4;
const REG_SP: usize = 0x5;
const REG_SI: usize = 0x6;
const REG_DI: usize = 0x7;

const SEG_ES: usize = 0x0;
const SEG_CS: usize = 0x1;
const SEG_SS: usize = 0x2;
const SEG_DS: usize = 0x3;

#[derive(Clone)]
struct CpuIterator {
    memory_manager: Rc<RefCell<MemoryManager>>,
    position: SegmentAndOffset,
}

impl<'a> DataIterator for CpuIterator {
    fn peek(&self) -> u8 {
        self.memory_manager.borrow().read_u8(self.position).unwrap()
    }

    fn consume(&mut self) -> u8 {
        let b = self.peek();
        self.advance();
        b
    }

    fn advance(&mut self) {
        self.position += 1
    }
}

//                              111111
//                              5432109876543210
const FLAG_SHIFT_CARRY: u16 = 1 << 0;
const FLAG_SHIFT_PARITY: u16 = 1 << 2;
const FLAG_SHIFT_AUX_CARRY: u16 = 1 << 4;
const FLAG_SHIFT_ZERO: u16 = 1 << 6;
const FLAG_SHIFT_SIGN: u16 = 1 << 7;
const FLAG_SHIFT_TRAP: u16 = 1 << 8;
const FLAG_SHIFT_INTERRUPT: u16 = 1 << 9;
const FLAG_SHIFT_DIRECTION: u16 = 1 << 10;
const FLAG_SHIFT_OVERFLOW: u16 = 1 << 11;

struct Flags {
    value: u16,
}

impl Flags {
    fn new() -> Self {
        Self { value: 0 }
    }

    fn set(&mut self, shift: u16) {
        self.value |= shift;
    }

    fn clear(&mut self, shift: u16) {
        self.value &= !shift;
    }

    fn is_set(&self, shift: u16) -> bool {
        (self.value & shift) != 0
    }
}

pub struct Cpu {
    registers: [u16; 8],
    segments: [u16; 4],
    pub ip: u16,
    flags: Flags,

    memory_manager: Rc<RefCell<MemoryManager>>,
}

impl Cpu {
    pub fn new(memory_manager: Rc<RefCell<MemoryManager>>) -> Self {
        let mut cpu = Self {
            registers: [0; 8],
            segments: [0; 4],
            ip: 0,
            flags: Flags::new(),
            memory_manager,
        };

        cpu.registers[REG_AX] = 0x0000;
        cpu.registers[REG_CX] = 0x00FF;
        cpu.registers[REG_DX] = 0x01DD;
        cpu.registers[REG_BX] = 0x0000;
        cpu.registers[REG_BP] = 0x091C;
        cpu.registers[REG_SP] = 0x0000;
        cpu.registers[REG_SI] = 0x0000;
        cpu.registers[REG_DI] = 0x0000;
        // cpu.segments[SEG_ES] = 0x01DD;
        // cpu.segments[SEG_CS] = 0x01ED;
        // cpu.segments[SEG_SS] = 0x01DD;
        // cpu.segments[SEG_DS] = 0x01ED;
        cpu.flags.set(FLAG_SHIFT_INTERRUPT);

        cpu
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
        print!(
            "ES: {:#06X} CS: {:#06X} SS: {:#06X} DS: {:#06X} ",
            self.segments[SEG_ES],
            self.segments[SEG_CS],
            self.segments[SEG_SS],
            self.segments[SEG_DS]
        );
        print!("IP: {:#06X} flags:", self.ip);
        print!(" C{}", self.flags.is_set(FLAG_SHIFT_CARRY) as u8);
        print!(" P{}", self.flags.is_set(FLAG_SHIFT_PARITY) as u8);
        print!(" A{}", self.flags.is_set(FLAG_SHIFT_AUX_CARRY) as u8);
        print!(" Z{}", self.flags.is_set(FLAG_SHIFT_ZERO) as u8);
        print!(" S{}", self.flags.is_set(FLAG_SHIFT_SIGN) as u8);
        print!(" T{}", self.flags.is_set(FLAG_SHIFT_TRAP) as u8);
        print!(" I{}", self.flags.is_set(FLAG_SHIFT_INTERRUPT) as u8);
        print!(" D{}", self.flags.is_set(FLAG_SHIFT_DIRECTION) as u8);
        println!(" O{}", self.flags.is_set(FLAG_SHIFT_OVERFLOW) as u8);
    }

    pub fn start(&mut self) {
        self.print_registers();

        let mut it = CpuIterator {
            memory_manager: Rc::clone(&self.memory_manager),
            position: segment_and_offset(self.segments[SEG_CS], self.ip),
        };

        loop {
            let start_position = it.position;
            match mrc_decoder::decode_instruction(&mut it) {
                Ok(instruction) => {
                    println!(
                        "{:#06X}:{:#06X}    {}",
                        self.segments[SEG_CS], self.ip, instruction
                    );
                    self.ip += (it.position - start_position) as u16;
                    self.execute(&instruction);
                    self.print_registers();
                }

                Err(err) => {
                    eprintln!("{}", err);
                    break;
                }
            }
        }
    }

    fn execute(&mut self, instruction: &Instruction) {
        match instruction.operation {
            Operation::Add => {
                if let OperandSet::DestinationAndSource(destination, source) = &instruction.operands
                {
                    let source_value = self.get_operand_value(&source);

                    println!("source_value: {}", source_value);

                    match &destination.0 {
                        OperandType::Register(register) => {
                            let new_value =
                                self.get_register_value(register, &source.1) + source_value;
                            self.set_register_value(register, &destination.1, new_value);
                        }
                        _ => unreachable!("not supported"),
                    }
                } else {
                    unreachable!("Invalid operand set!");
                }
            }

            Operation::Call => match instruction.operands {
                OperandSet::Offset(offset) => self.ip += offset,
                _ => todo!(),
            },

            Operation::Cld => {
                self.flags.clear(FLAG_SHIFT_DIRECTION);
            }

            Operation::Cli => {
                self.flags.clear(FLAG_SHIFT_INTERRUPT);
            }

            Operation::Inc => match &instruction.operands {
                OperandSet::Destination(destination) => {
                    let value = self.get_operand_value(destination);
                    self.set_operand_value(destination, value + 1);
                }
                _ => panic!(),
            },

            Operation::Je => match &instruction.operands {
                OperandSet::Offset(offset) => {
                    if self.flags.is_set(FLAG_SHIFT_ZERO) {
                        self.ip += offset
                    }
                }
                _ => panic!(),
            },

            Operation::Or => {
                match &instruction.operands {
                    OperandSet::None => {}
                    OperandSet::Destination(_) => {}
                    OperandSet::DestinationAndSource(destination, source) => {
                        let source_value = self.get_operand_value(source);
                        let destination_value = self.get_operand_value(destination);

                        self.flags.clear(FLAG_SHIFT_OVERFLOW | FLAG_SHIFT_CARRY);

                        match destination.1 {
                            OperandSize::Byte => {
                                let result = destination_value as u8 | source_value as u8;
                                self.set_byte_result_flags(result);
                            }
                            OperandSize::Word => {
                                let result = destination_value | source_value;
                                self.set_word_result_flags(result);
                            }
                        }

                        // The OF and CF flags are cleared; the SF, ZF, and PF flags are set according to the result. The state of the AF flag is undefined.
                    }
                    OperandSet::Offset(_) => {}
                    OperandSet::SegmentAndOffset(_, _) => {}
                }
            }

            Operation::Mov => match &instruction.operands {
                OperandSet::DestinationAndSource(destination, source) => {
                    let value = self.get_operand_value(source);
                    match destination {
                        Operand(OperandType::Register(register), operand_size) => {
                            self.set_register_value(register, operand_size, value);
                        }
                        Operand(OperandType::Segment(segment), _) => {
                            self.set_segment_value(segment, value);
                        }
                        _ => todo!("Destination operand not supported in MOV"),
                    }
                }
                _ => todo!("OperandSet not supported!"),
            },

            Operation::Movsb => {
                let mut cx = self.get_register_value(&Register::ClCx, &OperandSize::Word);

                let ds = self.get_segment_value(SEG_DS);
                let si = self.get_register_value(&Register::DhSi, &OperandSize::Byte);
                let es = self.get_segment_value(SEG_ES);
                let di = self.get_register_value(&Register::BhDi, &OperandSize::Byte);

                loop {
                    let byte = match self
                        .memory_manager
                        .borrow()
                        .read_u8(segment_and_offset(ds, si))
                    {
                        Ok(byte) => byte,
                        Err(err) => panic!("Could not read byte: {:?}", err),
                    };

                    if let Err(err) = self
                        .memory_manager
                        .borrow_mut()
                        .write_u8(segment_and_offset(es, di), byte)
                    {
                        panic!("Could not read byte: {:?}", err);
                    }

                    cx -= 1;

                    if cx == 0 {
                        break;
                    }
                }
            }

            Operation::Std => {
                self.flags.set(FLAG_SHIFT_DIRECTION);
            }

            Operation::Sti => {
                self.flags.set(FLAG_SHIFT_INTERRUPT);
            }

            _ => {
                todo!("Unknown instruction! {}", instruction.operation);
            }
        }
    }

    fn get_operand_value(&self, operand: &Operand) -> u16 {
        match &operand.0 {
            OperandType::Direct(offset) => {
                match self
                    .memory_manager
                    .borrow()
                    .read_u8(segment_and_offset(self.get_segment_value(SEG_DS), *offset))
                {
                    Ok(value) => value.into(),
                    Err(err) => panic!("Could not get direct memory value: {:?}", err),
                }
            }
            OperandType::Indirect(_, _) => todo!(),
            OperandType::Register(register) => self.get_register_value(&register, &operand.1),
            OperandType::Segment(encoding) => match encoding {
                Segment::Es => self.segments[SEG_ES],
                Segment::Cs => self.segments[SEG_CS],
                Segment::Ss => self.segments[SEG_SS],
                Segment::Ds => self.segments[SEG_DS],
            },
            OperandType::Immediate(value) => *value,
        }
    }

    fn set_operand_value(&mut self, operand: &Operand, value: u16) {
        match &operand.0 {
            OperandType::Register(register) => {
                self.set_register_value(&register, &operand.1, value)
            }
            OperandType::Segment(segment) => self.set_segment_value(segment, value),
            _ => todo!(),
        }
    }

    fn get_register_value(&self, register: &Register, operand_size: &OperandSize) -> u16 {
        use Register::*;

        match operand_size {
            OperandSize::Byte => match register {
                AlAx => self.registers[REG_AX] & 0x00FF,
                ClCx => self.registers[REG_CX] & 0x00FF,
                DlDx => self.registers[REG_DX] & 0x00FF,
                BlBx => self.registers[REG_BX] & 0x00FF,
                AhSp => self.registers[REG_AX] & 0xFF00,
                ChBp => self.registers[REG_CX] & 0xFF00,
                DhSi => self.registers[REG_BX] & 0xFF00,
                BhDi => self.registers[REG_BX] & 0xFF00,
            },
            OperandSize::Word => match register {
                AlAx => self.registers[REG_AX],
                ClCx => self.registers[REG_CX],
                DlDx => self.registers[REG_DX],
                BlBx => self.registers[REG_BX],
                AhSp => self.registers[REG_SP],
                ChBp => self.registers[REG_BP],
                DhSi => self.registers[REG_SI],
                BhDi => self.registers[REG_DI],
            },
        }
    }

    fn set_register_value(&mut self, register: &Register, data_size: &OperandSize, value: u16) {
        use Register::*;

        match data_size {
            OperandSize::Byte => match register {
                AlAx => {
                    self.registers[REG_AX] = (self.registers[REG_AX] & 0xFF00) + (value & 0x00FF)
                }
                ClCx => {
                    self.registers[REG_CX] = (self.registers[REG_CX] & 0xFF00) + (value & 0x00FF)
                }
                DlDx => {
                    self.registers[REG_DX] = (self.registers[REG_DX] & 0xFF00) + (value & 0x00FF)
                }
                BlBx => {
                    self.registers[REG_BX] = (self.registers[REG_BX] & 0xFF00) + (value & 0x00FF)
                }
                AhSp => {
                    self.registers[REG_AX] =
                        (self.registers[REG_AX] & 0x00ff) + ((value & 0x00FF) << 0x08)
                }
                ChBp => {
                    self.registers[REG_CX] =
                        (self.registers[REG_CX] & 0x00ff) + ((value & 0x00FF) << 0x08)
                }
                DhSi => {
                    self.registers[REG_DX] =
                        (self.registers[REG_DX] & 0x00ff) + ((value & 0x00FF) << 0x08)
                }
                BhDi => {
                    self.registers[REG_BX] =
                        (self.registers[REG_BX] & 0x00ff) + ((value & 0x00FF) << 0x08)
                }
            },
            OperandSize::Word => match register {
                AlAx => self.registers[REG_AX] = value,
                ClCx => self.registers[REG_CX] = value,
                DlDx => self.registers[REG_DX] = value,
                BlBx => self.registers[REG_BX] = value,
                AhSp => self.registers[REG_SP] = value,
                ChBp => self.registers[REG_BP] = value,
                DhSi => self.registers[REG_SI] = value,
                BhDi => self.registers[REG_DI] = value,
            },
        }
    }

    fn get_segment_value(&self, segment: usize) -> u16 {
        self.segments[segment]
    }

    fn set_segment_value(&mut self, segment: &Segment, value: u16) {
        match segment {
            Segment::Es => self.segments[SEG_ES] = value,
            Segment::Cs => self.segments[SEG_CS] = value,
            Segment::Ss => self.segments[SEG_SS] = value,
            Segment::Ds => self.segments[SEG_DS] = value,
        }
    }

    fn set_byte_result_flags(&mut self, result: u8) {
        if result == 0 {
            self.flags.set(FLAG_SHIFT_ZERO);
        }
        // TODO: Set signed flag.
        // TODO: Set parity flag.
    }

    fn set_word_result_flags(&mut self, result: u16) {
        if result == 0 {
            self.flags.set(FLAG_SHIFT_ZERO);
        }
        // TODO: Set signed flag.
        // TODO: Set parity flag.
    }
}
