mod operations;

use crate::memory::{Bus, MemoryInterface};
use bitflags::bitflags;
use mrc_decoder::decode_instruction;
use mrc_x86::{
    AddressingMode, Displacement, Instruction, Operand, OperandSet, OperandSize, OperandType,
    Operation, Register, Repeat, Segment,
};
use std::cell::RefCell;
use std::fmt::Formatter;
use std::num::Wrapping;
use std::rc::Rc;

/*
Memory areas:

0x00000...0x003FF   1KiB            Real Mode IVT (Interrupt Vector Table)
0x00400...0x004FF   256b            BDA (BIOS data area)
0x00500...0x07BFF   ~30KiB          Conventional memory
0x07C00...0x07DFF   512b            Your OS BootSector
0x07E00...0x7FFFF   480.5KiB        Conventional memory
0x80000...0x9FFFF   128KiB          Extended BIOS Data Area (EBDA)
0xA0000...0xBFFFF   128KiB          Video display memory
0xC0000...0xC7FFF   ~32KiB          Video BIOS
0xC8000...0xEFFFF   ~160KiB         BIOS Expansions
0xF0000...0xFFFFF   64KiB           Motherboard BIOS
*/

pub trait SignificantBit {
    fn least_significant_bit(&self) -> bool;
    fn most_significant_bit(&self) -> bool;
}

impl SignificantBit for u8 {
    fn least_significant_bit(&self) -> bool {
        self & 0x1 != 0
    }

    fn most_significant_bit(&self) -> bool {
        self & 0x80 != 0
    }
}

impl SignificantBit for u16 {
    fn least_significant_bit(&self) -> bool {
        self & 0x1 != 0
    }

    fn most_significant_bit(&self) -> bool {
        self & 0x8000 != 0
    }
}

pub type SegmentAndOffset = u32;

pub fn segment_and_offset(segment: u16, offset: u16) -> SegmentAndOffset {
    ((segment as SegmentAndOffset) << 4) + (offset as SegmentAndOffset)
}

#[allow(dead_code)]
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
struct CpuIterator<M: MemoryInterface> {
    memory: Rc<RefCell<M>>,
    position: SegmentAndOffset,
}

impl<'a, M: MemoryInterface> Iterator for CpuIterator<M> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let byte = self.memory.borrow().read(self.position);
        self.position += 1;
        Some(byte)
    }
}

bitflags! {
    pub struct Flags : u16 {
        const CARRY = 1 << 0;       // 0 - 1
        const UD1 = 1 << 1;         // 1 - 2
        const PARITY = 1 << 2;      // 2 - 4
        const UD2 = 1 << 3;         // 3 - 8
        const AUX_CARRY = 1 << 4;   // 4 - 16
        const UD3 = 1 << 5;         // 5 - 32
        const ZERO = 1 << 6;        // 6 - 64
        const SIGN = 1 << 7;        // 7 - 128
        const TRAP = 1 << 8;        // 8 -
        const INTERRUPT = 1 << 9;   // 9 -
        const DIRECTION = 1 << 10;  // A -
        const OVERFLOW = 1 << 11;   // B -
    }
}

impl std::fmt::Display for Flags {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        macro_rules! print_flag {
            ($name:ident,$flag:expr) => {{
                if self.contains($flag) {
                    write!(f, "{}", stringify!($name))?;
                } else {
                    write!(f, ".")?;
                }
            }};
        }

        print_flag!(O, Flags::OVERFLOW); // 11
        print_flag!(D, Flags::DIRECTION); // 10
        print_flag!(I, Flags::INTERRUPT); // 9
        print_flag!(T, Flags::TRAP); // 8
        print_flag!(S, Flags::SIGN); // 7
        print_flag!(Z, Flags::ZERO); // 6
        print_flag!(U, Flags::UD3); // 5
        print_flag!(A, Flags::AUX_CARRY); // 4
        print_flag!(U, Flags::UD2); // 3
        print_flag!(P, Flags::PARITY); // 2
        print_flag!(U, Flags::UD1); // 1
        print_flag!(C, Flags::CARRY); // 0

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum ExecuteResult {
    Continue,
    Stop,
}

fn illegal_operands(instruction: &Instruction) {
    panic!("Illegal operands! {:?}", instruction)
}

pub struct Cpu<M: MemoryInterface> {
    pub registers: [u16; 8],
    pub segments: [u16; 4],
    pub ip: u16,
    pub flags: Flags,

    bus: Bus<M>,
}

impl<M: MemoryInterface> Cpu<M> {
    pub fn new(memory_interface: M) -> Self {
        let mut cpu = Self {
            registers: [0; 8],
            segments: [0; 4],
            ip: 0x0000,
            flags: Flags::empty(),
            bus: Bus::new(Rc::new(RefCell::new(memory_interface))),
        };

        // if cfg!(feature = "dos") {
        //     cpu.registers[REG_AX] = 0x0000;
        //     cpu.registers[REG_CX] = 0x00FF;
        //     cpu.registers[REG_DX] = 0x01DD;
        //     cpu.registers[REG_BX] = 0x0000;
        //     cpu.registers[REG_BP] = 0x091C;
        //     cpu.registers[REG_SP] = 0xFFFE; // Start at the top of the SS, because we grow down.
        //     cpu.registers[REG_SI] = 0x0000;
        //     cpu.registers[REG_DI] = 0x0000;
        //     cpu.segments[SEG_ES] = 0x0000;
        //     cpu.segments[SEG_CS] = 0x0000;
        //     cpu.segments[SEG_SS] = 0x0000;
        //     cpu.segments[SEG_DS] = 0x0000;
        //     cpu.ip = 0x0100;
        //     cpu.flags |= Flags::INTERRUPT;
        // } else {
        cpu.segments[SEG_ES] = 0x0000;
        cpu.segments[SEG_CS] = 0xF000;
        cpu.segments[SEG_SS] = 0x0000;
        cpu.segments[SEG_DS] = 0x0000;
        cpu.ip = 0xFFF0;
        cpu.flags |= Flags::INTERRUPT;
        // }

        cpu
    }

    pub fn print_registers(&mut self) {
        print!(
            "AX: {:04X} BX: {:04X} CX: {:04X} DX: {:04X} ",
            self.registers[REG_AX],
            self.registers[REG_BX],
            self.registers[REG_CX],
            self.registers[REG_DX]
        );
        println!(
            "SP: {:04X} BP: {:04X} SI: {:04X} DI: {:04X}",
            self.registers[REG_SP],
            self.registers[REG_BP],
            self.registers[REG_SI],
            self.registers[REG_DI]
        );
        print!(
            "ES: {:04X} CS: {:04X} SS: {:04X} DS: {:04X} ",
            self.segments[SEG_ES],
            self.segments[SEG_CS],
            self.segments[SEG_SS],
            self.segments[SEG_DS]
        );

        println!("IP: {:04X} flags: {}", self.ip, self.flags);

        /*
        print!("stack: ");
        let ss = self.segments[SEG_SS];
        let sp = self.registers[REG_SP];
        let mut current = 0xFFFE;
        for _ in 0..5 {
            if current <= sp {
                break;
            }
            let value = self.memory.read_u16(segment_and_offset(ss, current));
            print!("{:04X} ", value);
            current -= 2;
        }
        println!();
        */
    }

    pub fn start(&mut self) {
        self.print_registers();

        let mut instructions_executed = 0usize;

        // for _ in 0..100 {
        loop {
            let mut it = CpuIterator {
                memory: self.bus.interface(),
                position: segment_and_offset(self.segments[SEG_CS], self.ip),
            };

            let start_position = it.position;

            // Print some bytes from memory.
            // print!("Bytes to decode: ");
            // for i in 0..5 {
            //     print!(
            //         "{:02X} ",
            //         self.bus
            //             .read_u8(segment_and_offset(self.segments[SEG_CS], self.ip + i))
            //     );
            // }
            // println!();

            match decode_instruction(&mut it) {
                Ok(instruction) => {
                    println!(
                        "{:04X}:{:04X}    {}",
                        self.segments[SEG_CS], self.ip, instruction
                    );
                    self.ip += (it.position - start_position) as u16;
                    if self.execute(&instruction) == ExecuteResult::Stop {
                        break;
                    }

                    instructions_executed += 1;

                    self.print_registers();
                }

                Err(err) => {
                    eprintln!("{}", err);
                    break;
                }
            }
        }

        log::info!("Instructions executed: {}", instructions_executed);
    }

    fn execute(&mut self, instruction: &Instruction) -> ExecuteResult {
        match instruction.operands {
            OperandSet::DestinationAndSource(
                Operand(ref destination, OperandSize::Byte),
                Operand(ref source, OperandSize::Byte),
            ) => {
                use operations::*;
                use Operation::*;

                let d = self.get_byte_operand_type_value(destination);
                let s = self.get_byte_operand_type_value(source);
                if let Some(did_operation) = match instruction.operation {
                    Adc => Some(arithmetic::add_with_carry_byte(d, s, &mut self.flags)),
                    Add => Some(arithmetic::add_byte(d, s, &mut self.flags)),
                    And => Some(logic::and_byte(d, s, &mut self.flags)),
                    Cmp => Some(arithmetic::compare_byte(d, s, &mut self.flags)),
                    Mul => Some(arithmetic::multiply_byte(d, s, &mut self.flags)),
                    Or => Some(logic::or_byte(d, s, &mut self.flags)),
                    Rol => Some(logic::rol_byte(d, s, &mut self.flags)),
                    Shl => Some(logic::shift_left_byte(d, s, &mut self.flags)),
                    Shr => Some(logic::shift_right_byte(d, s, &mut self.flags)),
                    Sub => Some(arithmetic::sub_byte(d, s, &mut self.flags)),
                    Test => Some(logic::test_byte(d, s, &mut self.flags)),
                    Xor => Some(logic::xor_byte(d, s, &mut self.flags)),
                    _ => None,
                } {
                    if let Some(result) = did_operation {
                        self.set_byte_operand_type_value(destination, result);
                    }
                    return ExecuteResult::Continue;
                }
            }

            OperandSet::DestinationAndSource(
                Operand(ref destination, OperandSize::Word),
                Operand(ref source, OperandSize::Word),
            ) => {
                use operations::*;
                use Operation::*;

                let d = self.get_word_operand_type_value(destination);
                let s = self.get_word_operand_type_value(source);
                if let Some(did_operation) = match instruction.operation {
                    Adc => Some(arithmetic::add_with_carry_word(d, s, &mut self.flags)),
                    Add => Some(arithmetic::add_word(d, s, &mut self.flags)),
                    And => Some(logic::and_word(d, s, &mut self.flags)),
                    Cmp => Some(arithmetic::compare_word(d, s, &mut self.flags)),
                    Mul => Some(arithmetic::multiply_word(d, s, &mut self.flags)),
                    Or => Some(logic::or_word(d, s, &mut self.flags)),
                    Rol => Some(logic::rol_word(d, s, &mut self.flags)),
                    Shl => Some(logic::shift_left_word(d, s, &mut self.flags)),
                    Shr => Some(logic::shift_right_word(d, s, &mut self.flags)),
                    Sub => Some(arithmetic::sub_word(d, s, &mut self.flags)),
                    Test => Some(logic::test_word(d, s, &mut self.flags)),
                    Xor => Some(logic::xor_word(d, s, &mut self.flags)),
                    _ => None,
                } {
                    if let Some(result) = did_operation {
                        self.set_word_operand_type_value(destination, result);
                    }
                    return ExecuteResult::Continue;
                }
            }

            _ => {}
        }

        match instruction.operation {
            Operation::Call => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    // Store the current IP (which is after this CALL) on the stack. So that RET
                    // can pop it.
                    self.push_word(self.ip);
                    self.displace_ip(displacement);
                }
                _ => todo!(),
            },

            Operation::Cbw => {
                let al = self.get_byte_register_value(Register::AlAx);
                if al & 0b10000000 != 0 {
                    self.set_byte_register_value(Register::AhSp, 0b11111111);
                } else {
                    self.set_byte_register_value(Register::AhSp, 0b00000000);
                }
            }

            Operation::Clc => {
                self.flags.remove(Flags::CARRY);
            }

            Operation::Cld => {
                self.flags.remove(Flags::DIRECTION);
            }

            Operation::Cli => {
                self.flags.remove(Flags::INTERRUPT);
            }

            Operation::Dec => match &instruction.operands {
                OperandSet::Destination(destination) => match destination.1 {
                    OperandSize::Byte => {
                        let mut value = self.get_byte_operand_value(destination);
                        value = value.wrapping_sub(1);
                        self.set_byte_operand_value(destination, value);
                    }
                    OperandSize::Word => {
                        let mut value = self.get_word_operand_value(destination);
                        value = value.wrapping_sub(1);
                        self.set_word_operand_value(destination, value);
                    }
                },
                _ => illegal_operands(instruction),
            },

            Operation::Hlt => {
                println!("HALT");
                return ExecuteResult::Stop;
            }

            Operation::In => match instruction.operands {
                OperandSet::DestinationAndSource(
                    Operand(ref destination_type, OperandSize::Byte),
                    Operand(OperandType::Immediate(port), OperandSize::Byte),
                ) => {
                    log::info!("PORT IN: {:02X}", port);
                    self.set_byte_operand_type_value(destination_type, 0);
                }
                _ => illegal_operands(instruction),
            },

            Operation::Inc => match &instruction.operands {
                OperandSet::Destination(destination) => match destination.1 {
                    OperandSize::Byte => {
                        let mut value = self.get_byte_operand_value(destination);
                        value = value.wrapping_add(1);
                        self.set_byte_operand_value(destination, value);
                    }
                    OperandSize::Word => {
                        let mut value = self.get_word_operand_value(destination);
                        value = value.wrapping_add(1);
                        self.set_word_operand_value(destination, value);
                    }
                },
                _ => illegal_operands(instruction),
            },

            Operation::Int => match &instruction.operands {
                OperandSet::Destination(Operand(
                    OperandType::Immediate(value),
                    OperandSize::Byte,
                )) => {
                    if *value == 0x21 {
                        // DOS
                        match self.registers[REG_AX].to_le_bytes() {
                            [0x4C, return_code] => {
                                println!("INT 21h: DOS exit with return code ({})", return_code);
                                return ExecuteResult::Stop;
                            }
                            _ => {
                                println!("INT {}", value);
                            }
                        }
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Jb => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if self.flags.contains(Flags::CARRY) {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Jbe => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if self.flags.contains(Flags::CARRY | Flags::ZERO) {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Je => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if self.flags.contains(Flags::ZERO) {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Jl => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if self.flags.contains(Flags::SIGN) != self.flags.contains(Flags::OVERFLOW) {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Jle => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if self.flags.contains(Flags::ZERO)
                        || self.flags.contains(Flags::SIGN) != self.flags.contains(Flags::OVERFLOW)
                    {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Jmp => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    self.displace_ip(displacement);
                }
                OperandSet::SegmentAndOffset(segment, offset) => {
                    self.segments[SEG_CS] = *segment;
                    self.ip = *offset;
                }
                _ => illegal_operands(instruction),
            },

            Operation::Jnb => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if !self.flags.contains(Flags::CARRY) {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Jnbe => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if !self.flags.contains(Flags::CARRY) && !self.flags.contains(Flags::ZERO) {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Jne => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if !self.flags.contains(Flags::ZERO) {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Jno => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if !self.flags.contains(Flags::OVERFLOW) {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Jnp => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if !self.flags.contains(Flags::PARITY) {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Jns => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if !self.flags.contains(Flags::SIGN) {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Jo => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if self.flags.contains(Flags::OVERFLOW) {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Jp => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if self.flags.contains(Flags::PARITY) {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Js => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if self.flags.contains(Flags::SIGN) {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Lahf => {
                // Use the low byte of the flags register.
                let bytes = self.flags.bits.to_le_bytes();
                self.set_byte_register_value(Register::AhSp, bytes[0]);
            }

            Operation::Lea => match instruction.operands {
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(register), OperandSize::Word),
                    Operand(
                        OperandType::Indirect(segment, ref addressing_mode, ref displacement),
                        OperandSize::Word,
                    ),
                ) => {
                    let addr = self.get_indirect_addr(segment, addressing_mode, displacement);
                    self.set_word_register_value(register, addr as u16);
                }
                _ => illegal_operands(instruction),
            },

            Operation::Loop => match instruction.operands {
                OperandSet::Displacement(ref displacement) => {
                    let cx = self.get_word_register_value(Register::ClCx);
                    let cx = (Wrapping(cx) - Wrapping(1)).0;
                    self.set_word_register_value(Register::ClCx, cx);

                    if cx > 0 {
                        self.displace_ip(displacement);
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Nop => {}

            Operation::Out => match instruction.operands {
                OperandSet::DestinationAndSource(ref destination, ref source) => {
                    let source_value = self.get_byte_operand_value(source);

                    match destination.1 {
                        OperandSize::Byte => {
                            let port = self.get_byte_operand_value(destination);
                            log::info!("OUT: {:02X} to port {:02X}", source_value, port);
                        }
                        OperandSize::Word => {
                            let port = self.get_word_operand_value(destination);
                            log::info!("OUT: {:02X} to port {:04X}", source_value, port);
                        }
                    }
                }
                _ => illegal_operands(instruction),
            },

            Operation::Push => match &instruction.operands {
                OperandSet::Destination(destination) => {
                    let value = self.get_word_operand_value(destination);
                    self.push_word(value);
                }
                _ => illegal_operands(instruction),
            },

            Operation::Pop => match &instruction.operands {
                OperandSet::Destination(destination) => {
                    let value = self.pop_word();
                    self.set_word_operand_value(destination, value);
                }
                _ => illegal_operands(instruction),
            },

            Operation::Mov => match &instruction.operands {
                OperandSet::DestinationAndSource(
                    Operand(destination, OperandSize::Byte),
                    Operand(source, OperandSize::Byte),
                ) => {
                    let source_value = self.get_byte_operand_type_value(source);
                    self.set_byte_operand_type_value(destination, source_value);
                }
                OperandSet::DestinationAndSource(
                    Operand(destination, OperandSize::Word),
                    Operand(source, OperandSize::Word),
                ) => {
                    let source_value = self.get_word_operand_type_value(source);
                    self.set_word_operand_type_value(destination, source_value);
                }
                _ => illegal_operands(instruction),
            },

            Operation::Movsb | Operation::Movsw | Operation::Stosb | Operation::Stosw => {
                let value_size: u16 = match instruction.operation {
                    Operation::Movsb | Operation::Stosb => 1,
                    Operation::Movsw | Operation::Stosw => 2,
                    _ => unreachable!(),
                };

                let ds = self.segments[SEG_DS];
                let mut si = self.get_word_register_value(Register::DhSi);

                let es = self.segments[SEG_ES];
                let mut di = self.get_word_register_value(Register::BhDi);

                let mut count = match instruction.repeat {
                    None => 1,
                    Some(ref repeat) => match repeat {
                        Repeat::Equal => self.get_word_register_value(Register::ClCx),
                        Repeat::NotEqual => self.get_word_register_value(Register::ClCx),
                    },
                };

                let forward = self.flags.contains(Flags::DIRECTION);

                loop {
                    match instruction.operation {
                        Operation::Movsb => {
                            let value = self.bus.read_u8(segment_and_offset(ds, si));
                            self.bus.write_u8(segment_and_offset(es, di), value);
                        }

                        Operation::Movsw => {
                            let value = self.bus.read_u16(segment_and_offset(ds, si));
                            self.bus.write_u16(segment_and_offset(es, di), value);
                        }

                        Operation::Stosb => {
                            let value = self.get_byte_register_value(Register::AlAx);
                            self.bus.write_u8(segment_and_offset(es, di), value);
                        }

                        Operation::Stosw => {
                            let value = self.get_word_register_value(Register::AlAx);
                            self.bus.write_u16(segment_and_offset(es, di), value);
                        }

                        _ => unreachable!(),
                    };

                    if forward {
                        si = si.wrapping_add(value_size);
                        di = si.wrapping_add(value_size);
                    } else {
                        si = si.wrapping_sub(value_size);
                        di = si.wrapping_sub(value_size);
                    }

                    count -= 1;

                    if count == 0 {
                        break;
                    }

                    if let Some(ref repeat) = instruction.repeat {
                        match repeat {
                            Repeat::Equal => {
                                if self.flags.contains(Flags::ZERO) {
                                    break;
                                }
                            }
                            Repeat::NotEqual => {
                                if !self.flags.contains(Flags::ZERO) {
                                    break;
                                }
                            }
                        }
                    }
                }

                self.set_word_register_value(Register::DhSi, si + 1);
                self.set_word_register_value(Register::BhDi, di + 1);

                if instruction.repeat.is_some() {
                    self.set_word_register_value(Register::ClCx, count);
                }
            }

            Operation::Ret => {
                // Pop the return address from the stack.
                self.ip = self.pop_word();
            }

            Operation::Sahf => {
                let ah = self.get_byte_register_value(Register::AhSp);
                self.flags.bits = u16::from_le_bytes([ah, 0]);
            }

            Operation::Stc => {
                self.flags.insert(Flags::CARRY);
            }

            Operation::Std => {
                self.flags.insert(Flags::DIRECTION);
            }

            Operation::Sti => {
                self.flags.insert(Flags::INTERRUPT);
            }

            _ => {
                todo!("Unknown instruction! {}", instruction.operation);
            }
        }

        ExecuteResult::Continue
    }

    fn push_word(&mut self, value: u16) {
        let stack_pointer = self.get_stack_pointer();
        self.bus.write_u16(stack_pointer, value);
        self.adjust_stack_pointer(-2);
    }

    fn pop_word(&mut self) -> u16 {
        self.adjust_stack_pointer(2);
        let stack_pointer = self.get_stack_pointer();
        self.bus.read_u16(stack_pointer)
    }

    fn get_indirect_addr(
        &self,
        segment: Segment,
        addressing_mode: &AddressingMode,
        displacement: &Displacement,
    ) -> SegmentAndOffset {
        let seg = self.get_segment_value(segment);

        let mut addr = match addressing_mode {
            AddressingMode::BxSi => {
                let bx = self.get_word_register_value(Register::BlBx);
                let si = self.get_word_register_value(Register::DhSi);
                segment_and_offset(seg, bx + si)
            }
            AddressingMode::BxDi => {
                let bx = self.get_word_register_value(Register::BlBx);
                let di = self.get_word_register_value(Register::BhDi);
                segment_and_offset(seg, bx + di)
            }
            AddressingMode::BpSi => {
                let bp = self.get_word_register_value(Register::ChBp);
                let si = self.get_word_register_value(Register::DhSi);
                segment_and_offset(seg, bp + si)
            }
            AddressingMode::BpDi => {
                let bp = self.get_word_register_value(Register::ChBp);
                let di = self.get_word_register_value(Register::BhDi);
                segment_and_offset(seg, bp + di)
            }
            AddressingMode::Si => {
                let si = self.get_word_register_value(Register::DhSi);
                segment_and_offset(seg, si)
            }
            AddressingMode::Di => {
                let di = self.get_word_register_value(Register::BhDi);
                segment_and_offset(seg, di)
            }
            AddressingMode::Bp => {
                let bp = self.get_word_register_value(Register::ChBp);
                segment_and_offset(seg, bp)
            }
            AddressingMode::Bx => {
                let bx = self.get_word_register_value(Register::BlBx);
                segment_and_offset(seg, bx)
            }
        } as i64;

        match displacement {
            Displacement::None => {}
            Displacement::Byte(offset) => addr += *offset as i64,
            Displacement::Word(offset) => addr += *offset as i64,
        }

        addr as SegmentAndOffset
    }

    fn get_byte_operand_type_value(&self, operand_type: &OperandType) -> u8 {
        match operand_type {
            OperandType::Direct(segment, offset) => {
                // Get a single byte from DS:offset
                let ds = self.get_segment_value(*segment);
                let addr = segment_and_offset(ds, *offset);
                self.bus.read_u8(addr)
            }

            OperandType::Indirect(segment, addressing_mode, displacement) => self
                .bus
                .read_u8(self.get_indirect_addr(*segment, addressing_mode, displacement)),

            OperandType::Register(ref register) => self.get_byte_register_value(*register),

            OperandType::Segment(_) => panic!("Can't get segment value as a byte."),

            OperandType::Immediate(value) => *value as u8,
        }
    }

    fn get_byte_operand_value(&self, operand: &Operand) -> u8 {
        assert_eq!(OperandSize::Byte, operand.1);
        self.get_byte_operand_type_value(&operand.0)
    }

    fn get_word_operand_type_value(&self, operand_type: &OperandType) -> u16 {
        match operand_type {
            OperandType::Direct(segment, offset) => {
                // Get a single byte from DS:offset
                let seg = self.get_segment_value(*segment);
                let addr = segment_and_offset(seg, *offset);

                self.bus.read_u16(addr)
            }

            OperandType::Indirect(segment, addressing_mode, displacement) => self
                .bus
                .read_u16(self.get_indirect_addr(*segment, addressing_mode, displacement)),

            OperandType::Register(register) => self.get_word_register_value(*register),

            OperandType::Segment(segment) => match segment {
                Segment::Es => self.segments[SEG_ES],
                Segment::Cs => self.segments[SEG_CS],
                Segment::Ss => self.segments[SEG_SS],
                Segment::Ds => self.segments[SEG_DS],
            },

            OperandType::Immediate(value) => *value,
        }
    }

    fn get_word_operand_value(&self, operand: &Operand) -> u16 {
        assert_eq!(OperandSize::Word, operand.1);

        self.get_word_operand_type_value(&operand.0)
    }

    fn set_byte_operand_type_value(&mut self, operand_type: &OperandType, value: u8) {
        match operand_type {
            OperandType::Direct(segment, offset) => {
                let seg = self.get_segment_value(*segment);
                self.bus.write_u8(segment_and_offset(seg, *offset), value);
            }
            OperandType::Indirect(segment, addressing_mode, displacement) => {
                self.bus.write_u8(
                    self.get_indirect_addr(*segment, addressing_mode, displacement),
                    value,
                );
            }
            OperandType::Register(register) => self.set_byte_register_value(*register, value),
            OperandType::Segment(_) => panic!("Can not set segment value with a byte!"),
            OperandType::Immediate(_) => panic!("Can not set immediate value!"),
        }
    }

    fn set_byte_operand_value(&mut self, operand: &Operand, value: u8) {
        assert_eq!(OperandSize::Byte, operand.1);
        self.set_byte_operand_type_value(&operand.0, value);
    }

    fn set_word_operand_type_value(&mut self, operand_type: &OperandType, value: u16) {
        match operand_type {
            OperandType::Direct(segment, offset) => {
                let seg = self.get_segment_value(*segment);
                self.bus.write_u16(segment_and_offset(seg, *offset), value);
            }
            OperandType::Indirect(segment, addressing_mode, displacement) => {
                self.bus.write_u16(
                    self.get_indirect_addr(*segment, addressing_mode, displacement),
                    value,
                );
            }
            OperandType::Register(register) => self.set_word_register_value(*register, value),
            OperandType::Segment(segment) => self.set_segment_value(*segment, value),
            _ => todo!(),
        }
    }

    fn set_word_operand_value(&mut self, operand: &Operand, value: u16) {
        assert_eq!(OperandSize::Word, operand.1);
        self.set_word_operand_type_value(&operand.0, value);
    }

    fn get_byte_register_value(&self, register: Register) -> u8 {
        use Register::*;

        let byte: u8 = match register {
            AlAx => self.registers[REG_AX].to_le_bytes()[0],
            ClCx => self.registers[REG_CX].to_le_bytes()[0],
            DlDx => self.registers[REG_DX].to_le_bytes()[0],
            BlBx => self.registers[REG_BX].to_le_bytes()[0],
            AhSp => self.registers[REG_AX].to_le_bytes()[1],
            ChBp => self.registers[REG_CX].to_le_bytes()[1],
            DhSi => self.registers[REG_DX].to_le_bytes()[1],
            BhDi => self.registers[REG_BX].to_le_bytes()[1],
        } as u8;
        byte
    }

    fn get_word_register_value(&self, register: Register) -> u16 {
        use Register::*;

        match register {
            AlAx => self.registers[REG_AX],
            ClCx => self.registers[REG_CX],
            DlDx => self.registers[REG_DX],
            BlBx => self.registers[REG_BX],
            AhSp => self.registers[REG_SP],
            ChBp => self.registers[REG_BP],
            DhSi => self.registers[REG_SI],
            BhDi => self.registers[REG_DI],
        }
    }

    fn set_byte_register_value(&mut self, register: Register, value: u8) {
        use Register::*;

        match register {
            AlAx => {
                let mut bytes = self.registers[REG_AX].to_le_bytes();
                bytes[0] = value;
                self.registers[REG_AX] = u16::from_le_bytes(bytes);
            }
            ClCx => {
                let mut bytes = self.registers[REG_CX].to_le_bytes();
                bytes[0] = value;
                self.registers[REG_CX] = u16::from_le_bytes(bytes);
            }
            DlDx => {
                let mut bytes = self.registers[REG_DX].to_le_bytes();
                bytes[0] = value;
                self.registers[REG_DX] = u16::from_le_bytes(bytes);
            }
            BlBx => {
                let mut bytes = self.registers[REG_BX].to_le_bytes();
                bytes[0] = value;
                self.registers[REG_BX] = u16::from_le_bytes(bytes);
            }
            AhSp => {
                let mut bytes = self.registers[REG_AX].to_le_bytes();
                bytes[1] = value;
                self.registers[REG_AX] = u16::from_le_bytes(bytes);
            }
            ChBp => {
                let mut bytes = self.registers[REG_CX].to_le_bytes();
                bytes[1] = value;
                self.registers[REG_CX] = u16::from_le_bytes(bytes);
            }
            DhSi => {
                let mut bytes = self.registers[REG_DX].to_le_bytes();
                bytes[1] = value;
                self.registers[REG_DX] = u16::from_le_bytes(bytes);
            }
            BhDi => {
                let mut bytes = self.registers[REG_BX].to_le_bytes();
                bytes[1] = value;
                self.registers[REG_BX] = u16::from_le_bytes(bytes);
            }
        };
    }

    fn set_word_register_value(&mut self, register: Register, value: u16) {
        use Register::*;

        match register {
            AlAx => self.registers[REG_AX] = value,
            ClCx => self.registers[REG_CX] = value,
            DlDx => self.registers[REG_DX] = value,
            BlBx => self.registers[REG_BX] = value,
            AhSp => self.registers[REG_SP] = value,
            ChBp => self.registers[REG_BP] = value,
            DhSi => self.registers[REG_SI] = value,
            BhDi => self.registers[REG_DI] = value,
        }
    }

    fn get_segment_value(&self, segment: Segment) -> u16 {
        match segment {
            Segment::Es => self.segments[SEG_ES],
            Segment::Cs => self.segments[SEG_CS],
            Segment::Ss => self.segments[SEG_SS],
            Segment::Ds => self.segments[SEG_DS],
        }
    }

    fn set_segment_value(&mut self, segment: Segment, value: u16) {
        match segment {
            Segment::Es => self.segments[SEG_ES] = value,
            Segment::Cs => self.segments[SEG_CS] = value,
            Segment::Ss => self.segments[SEG_SS] = value,
            Segment::Ds => self.segments[SEG_DS] = value,
        }
    }

    fn displace_ip(&mut self, displacement: &Displacement) {
        match displacement {
            Displacement::None => {}
            Displacement::Byte(offset) => {
                self.ip = ((self.ip as i32) + (*offset as i32)) as u16;
            }
            Displacement::Word(offset) => {
                self.ip = ((self.ip as i32) + (*offset as i32)) as u16;
            }
        }
    }

    fn get_stack_pointer(&self) -> SegmentAndOffset {
        let ss = self.segments[SEG_SS];
        let sp = self.get_word_register_value(Register::AhSp);
        segment_and_offset(ss, sp)
    }

    fn adjust_stack_pointer(&mut self, offset: i16) {
        let sp = self.get_word_register_value(Register::AhSp);
        let sp = (sp as i16).wrapping_add(offset) as u16;
        self.set_word_register_value(Register::AhSp, sp);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::memory::PhysicalMemory;

    #[test]
    fn test_stack() {
        let memory = PhysicalMemory::with_capacity(0x100);
        let mut cpu = Cpu::new(memory);

        cpu.segments[SEG_SS] = 0x0000;
        cpu.registers[REG_AX] = 0x1010;
        cpu.registers[REG_SP] = 0x0010;

        // push ax
        let result = cpu.execute(&Instruction::new(
            Operation::Push,
            OperandSet::Destination(Operand(
                OperandType::Register(Register::AlAx),
                OperandSize::Word,
            )),
        ));

        assert_eq!(ExecuteResult::Continue, result);
        assert_eq!(0x000E, cpu.registers[REG_SP]);
        assert_eq!(0x1010, cpu.bus.read_u16(segment_and_offset(0x0000, 0x0010)));

        // pop bx
        let result = cpu.execute(&Instruction::new(
            Operation::Pop,
            OperandSet::Destination(Operand(
                OperandType::Register(Register::BlBx),
                OperandSize::Word,
            )),
        ));

        assert_eq!(ExecuteResult::Continue, result);
        assert_eq!(0x0010, cpu.registers[REG_SP]);
        assert_eq!(0x1010, cpu.registers[REG_BX]);
    }

    #[test]
    fn test_registers() {
        let memory = PhysicalMemory::with_capacity(0x100);
        let mut cpu = Cpu::new(memory);
        cpu.set_word_register_value(Register::AlAx, 0x1234);
        assert_eq!(0x34, cpu.get_byte_register_value(Register::AlAx));
        assert_eq!(0x12, cpu.get_byte_register_value(Register::AhSp));
    }

    #[test]
    fn test_lahf_sahf() {
        let memory = PhysicalMemory::with_capacity(0x100);
        let mut cpu = Cpu::new(memory);

        cpu.registers[REG_AX] = 0x0000;
        cpu.flags = Flags::SIGN | Flags::ZERO | Flags::AUX_CARRY | Flags::PARITY | Flags::CARRY;
        assert_eq!(0xD5, cpu.flags.bits);

        let sahf = Instruction::new(Operation::Lahf, OperandSet::None);
        cpu.execute(&sahf);
        assert_eq!(0x00D5, cpu.flags.bits);

        cpu.registers[REG_AX] = 0x0000;
        let sahf = Instruction::new(Operation::Sahf, OperandSet::None);
        cpu.execute(&sahf);
        assert_eq!(0x0000, cpu.flags.bits);
    }
}
