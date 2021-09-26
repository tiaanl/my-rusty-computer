use crate::memory::MemoryManager;
use bitflags::bitflags;
use mrc_decoder::decode_instruction;
use mrc_x86::{
    AddressingMode, Displacement, Instruction, Operand, OperandSet, OperandSize, OperandType,
    Operation, Register, Segment,
};
use std::cell::RefCell;
use std::rc::Rc;

fn high_byte(word: u16) -> u8 {
    (word >> 8) as u8
}

fn low_byte(word: u16) -> u8 {
    word as u8
}

fn high_and_low(word: u16) -> (u8, u8) {
    (high_byte(word), low_byte(word))
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
struct CpuIterator {
    memory_manager: Rc<RefCell<MemoryManager>>,
    position: SegmentAndOffset,
}

impl<'a> Iterator for CpuIterator {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let byte = self.memory_manager.borrow().read_u8(self.position);
        self.position += 1;
        Some(byte)
    }
}

bitflags! {
    struct Flags : u16 {
        const CARRY = 1 << 0;
        const PARITY = 1 << 2;
        const AUX_CARRY = 1 << 4;
        const ZERO = 1 << 6;
        const SIGN = 1 << 7;
        const TRAP = 1 << 8;
        const INTERRUPT = 1 << 9;
        const DIRECTION = 1 << 10;
        const OVERFLOW = 1 << 11;
    }
}

/*
struct Flags {
    value: u16,
}

impl Flags {
    fn new() -> Self {
        Self { bits: 0 }
    }

    fn set(&mut self, flag: Flags) {
        self |= flag;
    }

    fn clear(&mut self, shift: u16) {
        self.value &= !shift;
    }

    fn is_set(&self, shift: u16) -> bool {
        (self.value & shift) != 0
    }
}
*/

pub struct Cpu {
    registers: [u16; 8],
    segments: [u16; 4],
    pub ip: u16,
    flags: Flags,

    memory_manager: Rc<RefCell<MemoryManager>>,
}

impl Default for Cpu {
    fn default() -> Self {
        Self {
            memory_manager: Rc::new(RefCell::new(MemoryManager::new())),
            ..Default::default()
        }
    }
}

impl Cpu {
    pub fn build() -> CpuBuilder {
        CpuBuilder {
            cpu: Default::default(),
        }
    }

    pub fn new(memory_manager: Rc<RefCell<MemoryManager>>) -> Self {
        let mut cpu = Self {
            registers: [0; 8],
            segments: [0; 4],
            ip: 0x0000,
            flags: Flags::empty(),
            memory_manager,
        };

        if cfg!(feature = "dos") {
            cpu.registers[REG_AX] = 0x0000;
            cpu.registers[REG_CX] = 0x00FF;
            cpu.registers[REG_DX] = 0x01DD;
            cpu.registers[REG_BX] = 0x0000;
            cpu.registers[REG_BP] = 0x091C;
            cpu.registers[REG_SP] = 0xFFFE; // Start at the top of the SS, because we grow down.
            cpu.registers[REG_SI] = 0x0000;
            cpu.registers[REG_DI] = 0x0000;
            cpu.segments[SEG_ES] = 0x0000;
            cpu.segments[SEG_CS] = 0x0000;
            cpu.segments[SEG_SS] = 0x0000;
            cpu.segments[SEG_DS] = 0x0000;
            cpu.ip = 0x0100;
            cpu.flags |= Flags::INTERRUPT;
        } else {
            cpu.registers[REG_AX] = 0x0000;
            cpu.registers[REG_CX] = 0x00FF;
            cpu.registers[REG_DX] = 0x01DD;
            cpu.registers[REG_BX] = 0x0000;
            cpu.registers[REG_BP] = 0x091C;
            cpu.registers[REG_SP] = 0xFFFE; // Start at the top of the SS, because we grow down.
            cpu.registers[REG_SI] = 0x0000;
            cpu.registers[REG_DI] = 0x0000;
            cpu.segments[SEG_ES] = 0x0000;
            cpu.segments[SEG_CS] = 0x0000;
            cpu.segments[SEG_SS] = 0x0000;
            cpu.segments[SEG_DS] = 0x0000;
            cpu.ip = 0x7C00;
            cpu.flags |= Flags::INTERRUPT;
        }

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
        print!("IP: {:04X} flags:", self.ip);

        macro_rules! print_flag {
            ($name:ident,$flag:expr) => {{
                print!(" {}{}", stringify!($name), self.flags.contains($flag) as u8);
            }};
        }

        print_flag!(C, Flags::CARRY);
        print_flag!(P, Flags::PARITY);
        print_flag!(A, Flags::AUX_CARRY);
        print_flag!(Z, Flags::ZERO);
        print_flag!(S, Flags::SIGN);
        print_flag!(T, Flags::TRAP);
        print_flag!(I, Flags::INTERRUPT);
        print_flag!(D, Flags::DIRECTION);
        print_flag!(O, Flags::OVERFLOW);
        println!();

        print!("stack: ");
        let ss = self.segments[SEG_SS];
        let sp = self.registers[REG_SP];
        let mut current = 0xFFFE;
        for _ in 0..5 {
            if current <= sp {
                break;
            }
            let value = self
                .memory_manager
                .borrow()
                .read_u16(segment_and_offset(ss, current));
            print!("{:04X} ", value);
            current -= 2;
        }
        println!();
    }

    pub fn start(&mut self) {
        self.print_registers();

        for _ in 0..20 {
            let mut it = CpuIterator {
                memory_manager: Rc::clone(&self.memory_manager),
                position: segment_and_offset(self.segments[SEG_CS], self.ip),
            };

            let start_position = it.position;

            // Print some bytes from memory.
            /*
            print!("Bytes to decode: ");
            for i in 0..5 {
                print!(
                    "{:02X} ",
                    self.memory_manager
                        .borrow()
                        .read_u8(segment_and_offset(self.segments[SEG_CS], self.ip + i))
                );
            }
            println!();
            */

            match decode_instruction(&mut it) {
                Ok(instruction) => {
                    println!(
                        "{:04X}:{:04X}    {}",
                        self.segments[SEG_CS], self.ip, instruction
                    );
                    self.ip += (it.position - start_position) as u16;
                    if let Err(()) = self.execute(&instruction) {
                        break;
                    }
                    self.print_registers();
                }

                Err(err) => {
                    eprintln!("{}", err);
                    break;
                }
            }
        }
    }

    fn execute(&mut self, instruction: &Instruction) -> Result<(), ()> {
        match instruction.operation {
            Operation::Add => match &instruction.operands {
                OperandSet::DestinationAndSource(destination, source) => match destination.1 {
                    OperandSize::Byte => {
                        let mut destination_value = self.get_byte_operand_value(destination);
                        let source_value = self.get_byte_operand_value(source);
                        destination_value = destination_value.wrapping_add(source_value);
                        self.set_byte_operand_value(destination, destination_value);
                    }
                    OperandSize::Word => {
                        let mut destination_value = self.get_word_operand_value(destination);
                        let source_value = self.get_word_operand_value(source);
                        destination_value = destination_value.wrapping_add(source_value);
                        self.set_word_operand_value(destination, destination_value);
                    }
                },
                _ => panic!("Illegal operands!"),
            },

            Operation::Adc => match &instruction.operands {
                OperandSet::DestinationAndSource(destination, source) => match destination.1 {
                    OperandSize::Byte => {
                        let mut destination_value = self.get_byte_operand_value(destination);
                        let source_value = self.get_byte_operand_value(source);
                        destination_value = destination_value.wrapping_add(source_value);
                        destination_value =
                            destination_value.wrapping_add(if self.flags.contains(Flags::CARRY) {
                                1
                            } else {
                                0
                            });
                        self.set_byte_operand_value(destination, destination_value);
                    }
                    OperandSize::Word => {
                        let mut destination_value = self.get_word_operand_value(destination);
                        let source_value = self.get_word_operand_value(source);
                        destination_value = destination_value.wrapping_add(source_value);
                        destination_value =
                            destination_value.wrapping_add(if self.flags.contains(Flags::CARRY) {
                                1
                            } else {
                                0
                            });
                        self.set_word_operand_value(destination, destination_value);
                    }
                },
                _ => panic!("Illegal operands!"),
            },

            Operation::And => match &instruction.operands {
                OperandSet::DestinationAndSource(destination, source) => {
                    match destination.1 {
                        OperandSize::Byte => {
                            let source_value = self.get_byte_operand_value(source);
                            let destination_value = self.get_byte_operand_value(destination);

                            self.flags.remove(Flags::CARRY | Flags::CARRY);

                            let result = destination_value as i16 & source_value as i16;
                            self.set_byte_result_flags(result);
                        }
                        OperandSize::Word => {
                            let source_value = self.get_word_operand_value(source);
                            let destination_value = self.get_word_operand_value(destination);

                            self.flags.remove(Flags::OVERFLOW | Flags::CARRY);

                            let result = destination_value as i32 & source_value as i32;
                            self.set_word_result_flags(result);
                        }
                    }

                    // The OF and CF flags are cleared; the SF, ZF, and PF flags are set according to the result. The state of the AF flag is undefined.
                }
                _ => panic!("Illegal operands!"),
            },

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
                let al = self.get_byte_register_value(&Register::AlAx);
                if al & 0b10000000 != 0 {
                    self.set_byte_register_value(&Register::AhSp, 0b11111111);
                } else {
                    self.set_byte_register_value(&Register::AhSp, 0b00000000);
                }
            }

            Operation::Cmp => match &instruction.operands {
                OperandSet::DestinationAndSource(destination, source) => {
                    macro_rules! op {
                        ($get_operand_value:ident, $set_result_flags:ident, $up_type:tt) => {{
                            let destination_value = self.$get_operand_value(destination);
                            let source_value = self.$get_operand_value(source);

                            let result = destination_value as $up_type - source_value as $up_type;
                            self.$set_result_flags(result);
                        }};
                    }

                    match destination.1 {
                        OperandSize::Byte => {
                            op!(get_byte_operand_value, set_byte_result_flags, i16)
                        }

                        OperandSize::Word => {
                            op!(get_word_operand_value, set_word_result_flags, i32)
                        }
                    }
                }
                _ => unreachable!(),
            },

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
                _ => panic!(),
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
                _ => panic!(),
            },

            Operation::Int => match &instruction.operands {
                OperandSet::Destination(Operand(
                    OperandType::Immediate(value),
                    OperandSize::Byte,
                )) => {
                    if *value == 0x21 {
                        // DOS
                        match high_and_low(self.registers[REG_AX]) {
                            (0x4C, return_code) => {
                                println!("INT 21h: DOS exit with return code ({})", return_code);
                                return Err(());
                            }
                            _ => {
                                println!("INT {}", value);
                            }
                        }
                    }
                }
                _ => panic!("Illegal operands! {:?}", &instruction.operands),
            },

            Operation::Jbe => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if self.flags.contains(Flags::CARRY | Flags::ZERO) {
                        self.displace_ip(displacement);
                    }
                }
                _ => panic!(),
            },

            Operation::Je => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if self.flags.contains(Flags::ZERO) {
                        self.displace_ip(displacement);
                    }
                }
                _ => panic!(),
            },

            Operation::Jmp => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    self.displace_ip(displacement);
                }
                OperandSet::SegmentAndOffset(segment, offset) => {
                    self.segments[SEG_CS] = *segment;
                    self.ip = *offset;
                }
                _ => panic!("Illegal operands! {:?}", &instruction.operands),
            },

            Operation::Jnbe => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if !self.flags.contains(Flags::CARRY) && !self.flags.contains(Flags::ZERO) {
                        self.displace_ip(displacement);
                    }
                }
                _ => panic!(),
            },

            Operation::Jne => match &instruction.operands {
                OperandSet::Displacement(displacement) => {
                    if !self.flags.contains(Flags::ZERO) {
                        self.displace_ip(displacement);
                    }
                }
                _ => panic!(),
            },

            Operation::Lea => match &instruction.operands {
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(register), OperandSize::Word),
                    Operand(
                        OperandType::Indirect(addressing_mode, displacement),
                        OperandSize::Word,
                    ),
                ) => {
                    let addr = self.get_indirect_addr(addressing_mode, displacement);
                    self.set_word_register_value(register, addr as u16);
                }
                _ => panic!("Illegal operands!"),
            },

            Operation::Mul => match &instruction.operands {
                OperandSet::Destination(destination) => match destination.1 {
                    OperandSize::Byte => {
                        let al = self.get_byte_register_value(&Register::AlAx);
                        let mut value = self.get_byte_operand_value(destination);
                        value = value.wrapping_mul(al);
                        self.set_byte_operand_value(destination, value);
                    }
                    OperandSize::Word => {
                        let ax = self.get_word_register_value(&Register::AlAx);
                        let mut value = self.get_word_operand_value(destination);
                        value = value.wrapping_mul(ax);
                        self.set_word_operand_value(destination, value);
                    }
                },
                _ => panic!(),
            },

            Operation::Nop => {}

            Operation::Or => match &instruction.operands {
                OperandSet::DestinationAndSource(destination, source) => {
                    match destination.1 {
                        OperandSize::Byte => {
                            let source_value = self.get_byte_operand_value(source);
                            let destination_value = self.get_byte_operand_value(destination);

                            self.flags.remove(Flags::OVERFLOW | Flags::CARRY);

                            let result = destination_value as i16 | source_value as i16;
                            self.set_byte_result_flags(result);
                        }
                        OperandSize::Word => {
                            let source_value = self.get_word_operand_value(source);
                            let destination_value = self.get_word_operand_value(destination);

                            self.flags.remove(Flags::OVERFLOW | Flags::CARRY);

                            let result = destination_value as i32 | source_value as i32;
                            self.set_word_result_flags(result);
                        }
                    }

                    // The OF and CF flags are cleared; the SF, ZF, and PF flags are set according to the result. The state of the AF flag is undefined.
                }
                _ => panic!("Illegal operands!"),
            },

            Operation::Xor => match &instruction.operands {
                OperandSet::DestinationAndSource(destination, source) => {
                    match destination.1 {
                        OperandSize::Byte => {
                            let source_value = self.get_byte_operand_value(source);
                            let destination_value = self.get_byte_operand_value(destination);

                            self.flags.remove(Flags::OVERFLOW | Flags::CARRY);

                            let result = destination_value as i16 ^ source_value as i16;
                            self.set_byte_result_flags(result);
                        }
                        OperandSize::Word => {
                            let source_value = self.get_word_operand_value(source);
                            let destination_value = self.get_word_operand_value(destination);

                            self.flags.remove(Flags::OVERFLOW | Flags::CARRY);

                            let result = destination_value as i32 ^ source_value as i32;
                            self.set_word_result_flags(result);
                        }
                    }

                    // The OF and CF flags are cleared; the SF, ZF, and PF flags are set according to the result. The state of the AF flag is undefined.
                }
                _ => panic!("Illegal operands!"),
            },

            Operation::Push => match &instruction.operands {
                OperandSet::Destination(destination) => {
                    let value = self.get_word_operand_value(destination);
                    self.push_word(value);
                }
                _ => panic!("Illegal operands!"),
            },

            Operation::Pop => match &instruction.operands {
                OperandSet::Destination(destination) => {
                    let value = self.pop_word();
                    self.set_word_operand_value(destination, value);
                }
                _ => panic!("Illegal operands!"),
            },

            Operation::Mov => match &instruction.operands {
                OperandSet::DestinationAndSource(destination, source) => match destination.1 {
                    OperandSize::Byte => {
                        let source_value = self.get_byte_operand_value(source);
                        self.set_byte_operand_value(destination, source_value);
                    }
                    OperandSize::Word => {
                        let source_value = self.get_word_operand_value(source);
                        self.set_word_operand_value(destination, source_value);
                    }
                },
                _ => todo!("Illegal operands!"),
            },

            Operation::Movsb => {
                let ds = self.get_segment_value(SEG_DS);
                let es = self.get_segment_value(SEG_ES);

                loop {
                    let mut cx = self.get_word_register_value(&Register::ClCx);
                    let si = self.get_word_register_value(&Register::DhSi);
                    let di = self.get_word_register_value(&Register::BhDi);

                    let byte = self
                        .memory_manager
                        .borrow()
                        .read_u8(segment_and_offset(ds, si));

                    self.memory_manager
                        .borrow_mut()
                        .write_u8(segment_and_offset(es, di), byte);

                    self.set_word_register_value(&Register::DhSi, si + 1);
                    self.set_word_register_value(&Register::BhDi, di + 1);

                    cx -= 1;

                    self.set_word_register_value(&Register::ClCx, cx);

                    if cx == 0 {
                        break;
                    }
                }
            }

            Operation::Movsw => {
                let ds = self.get_segment_value(SEG_DS);
                let es = self.get_segment_value(SEG_ES);

                loop {
                    let mut cx = self.get_word_register_value(&Register::ClCx);
                    let si = self.get_word_register_value(&Register::DhSi);
                    let di = self.get_word_register_value(&Register::BhDi);

                    let word = self
                        .memory_manager
                        .borrow()
                        .read_u16(segment_and_offset(ds, si));

                    self.memory_manager
                        .borrow_mut()
                        .write_u16(segment_and_offset(es, di), word);

                    self.set_word_register_value(&Register::DhSi, si + 2);
                    self.set_word_register_value(&Register::BhDi, di + 2);

                    cx -= 1;

                    self.set_word_register_value(&Register::ClCx, cx);

                    if cx == 0 {
                        break;
                    }
                }
            }

            Operation::Ret => {
                // Pop the return address from the stack.
                self.ip = self.pop_word();
            }

            Operation::Rcl => todo!(),

            Operation::Rcr => todo!(),

            Operation::Rol => todo!(),

            Operation::Ror => todo!(),

            Operation::Sar => todo!(),

            Operation::Shl => match &instruction.operands {
                OperandSet::DestinationAndSource(destination, source) => {
                    let shift_value = self.get_byte_operand_value(source);
                    match destination.1 {
                        OperandSize::Byte => {
                            let value = self.get_byte_operand_value(destination);
                            let result = (value as i16) << (shift_value as i16);
                            self.set_byte_operand_value(destination, result as u8);
                            self.set_byte_result_flags(result);
                        }
                        OperandSize::Word => {
                            let value = self.get_word_operand_value(destination);
                            let result = (value as i32) << (shift_value as i32);
                            self.set_word_operand_value(destination, result as u16);
                            self.set_word_result_flags(result);
                        }
                    }
                }
                _ => panic!("Illegal operands!"),
            },

            Operation::Shr => todo!(),

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

        Ok(())
    }

    fn push_word(&mut self, value: u16) {
        let stack_pointer = self.get_stack_pointer();
        self.memory_manager
            .borrow_mut()
            .write_u16(stack_pointer, value);
        self.adjust_stack_pointer(-2);
    }

    fn pop_word(&mut self) -> u16 {
        self.adjust_stack_pointer(2);
        let stack_pointer = self.get_stack_pointer();
        self.memory_manager.borrow().read_u16(stack_pointer)
    }

    fn get_indirect_addr(
        &self,
        addressing_mode: &AddressingMode,
        displacement: &Displacement,
    ) -> SegmentAndOffset {
        let mut addr = match addressing_mode {
            AddressingMode::BxSi => {
                let bx = self.get_word_register_value(&Register::BlBx);
                let si = self.get_word_register_value(&Register::DhSi);
                segment_and_offset(bx, si)
            }
            AddressingMode::BxDi => {
                let bx = self.get_word_register_value(&Register::BlBx);
                let di = self.get_word_register_value(&Register::BhDi);
                segment_and_offset(bx, di)
            }
            AddressingMode::BpSi => {
                let bp = self.get_word_register_value(&Register::ChBp);
                let si = self.get_word_register_value(&Register::DhSi);
                segment_and_offset(bp, si)
            }
            AddressingMode::BpDi => {
                let bp = self.get_word_register_value(&Register::ChBp);
                let di = self.get_word_register_value(&Register::BhDi);
                segment_and_offset(bp, di)
            }
            AddressingMode::Si => {
                let ds = self.get_segment_value(SEG_DS);
                let si = self.get_word_register_value(&Register::DhSi);
                segment_and_offset(ds, si)
            }
            AddressingMode::Di => {
                let ds = self.get_segment_value(SEG_DS);
                let di = self.get_word_register_value(&Register::BhDi);
                segment_and_offset(ds, di)
            }
            AddressingMode::Bp => {
                let ds = self.get_segment_value(SEG_DS);
                let bp = self.get_word_register_value(&Register::ChBp);
                segment_and_offset(ds, bp)
            }
            AddressingMode::Bx => {
                let ds = self.get_segment_value(SEG_DS);
                let bx = self.get_word_register_value(&Register::BlBx);
                segment_and_offset(ds, bx)
            }
        } as i64;

        match displacement {
            Displacement::None => {}
            Displacement::Byte(offset) => addr += *offset as i64,
            Displacement::Word(offset) => addr += *offset as i64,
        }

        addr as SegmentAndOffset
    }

    fn get_byte_operand_value(&self, operand: &Operand) -> u8 {
        assert_eq!(OperandSize::Byte, operand.1);

        match &operand.0 {
            OperandType::Direct(offset) => {
                // Get a single byte from DS:offset
                let ds = self.get_segment_value(SEG_DS);
                let addr = segment_and_offset(ds, *offset);
                self.memory_manager.borrow().read_u8(addr)
            }

            OperandType::Indirect(addressing_mode, displacement) => self
                .memory_manager
                .borrow()
                .read_u8(self.get_indirect_addr(addressing_mode, displacement)),

            OperandType::Register(ref register) => self.get_byte_register_value(register),

            OperandType::Segment(_) => panic!("Can't get segment value as a byte."),

            OperandType::Immediate(value) => *value as u8,
        }
    }

    fn get_word_operand_value(&self, operand: &Operand) -> u16 {
        assert_eq!(OperandSize::Word, operand.1);

        match &operand.0 {
            OperandType::Direct(offset) => {
                // Get a single byte from DS:offset
                let ds = self.get_segment_value(SEG_DS);
                let addr = segment_and_offset(ds, *offset);

                self.memory_manager.borrow().read_u16(addr)
            }

            OperandType::Indirect(addressing_mode, displacement) => self
                .memory_manager
                .borrow()
                .read_u16(self.get_indirect_addr(addressing_mode, displacement)),

            OperandType::Register(ref register) => self.get_word_register_value(register),

            OperandType::Segment(segment) => match segment {
                Segment::Es => self.get_segment_value(SEG_ES),
                Segment::Cs => self.get_segment_value(SEG_CS),
                Segment::Ss => self.get_segment_value(SEG_SS),
                Segment::Ds => self.get_segment_value(SEG_DS),
            },

            OperandType::Immediate(value) => *value,
        }
    }

    fn set_byte_operand_value(&mut self, operand: &Operand, value: u8) {
        assert_eq!(OperandSize::Byte, operand.1);

        match &operand.0 {
            OperandType::Direct(offset) => {
                let ds = self.get_segment_value(SEG_DS);
                self.memory_manager
                    .borrow_mut()
                    .write_u8(segment_and_offset(ds, *offset), value);
            }
            OperandType::Indirect(addressing_mode, displacement) => {
                self.memory_manager
                    .borrow_mut()
                    .write_u8(self.get_indirect_addr(addressing_mode, displacement), value);
            }
            OperandType::Register(register) => self.set_byte_register_value(&register, value),
            OperandType::Segment(_) => panic!("Cannot set segment value with a byte!"),
            OperandType::Immediate(_) => todo!(),
        }
    }

    fn set_word_operand_value(&mut self, operand: &Operand, value: u16) {
        match &operand.0 {
            OperandType::Direct(offset) => {
                let ds = self.get_segment_value(SEG_DS);
                self.memory_manager
                    .borrow_mut()
                    .write_u16(segment_and_offset(ds, *offset), value);
            }
            OperandType::Indirect(addressing_mode, displacement) => {
                self.memory_manager
                    .borrow_mut()
                    .write_u16(self.get_indirect_addr(addressing_mode, displacement), value);
            }
            OperandType::Register(register) => self.set_word_register_value(&register, value),
            OperandType::Segment(segment) => self.set_segment_value(segment, value),
            _ => todo!(),
        }
    }

    fn get_byte_register_value(&self, register: &Register) -> u8 {
        use Register::*;

        let byte: u8 = match register {
            AlAx => self.registers[REG_AX] & 0x00FF,
            ClCx => self.registers[REG_CX] & 0x00FF,
            DlDx => self.registers[REG_DX] & 0x00FF,
            BlBx => self.registers[REG_BX] & 0x00FF,
            AhSp => self.registers[REG_AX] & 0xFF00,
            ChBp => self.registers[REG_CX] & 0xFF00,
            DhSi => self.registers[REG_BX] & 0xFF00,
            BhDi => self.registers[REG_BX] & 0xFF00,
        } as u8;
        byte
    }

    fn get_word_register_value(&self, register: &Register) -> u16 {
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

    fn set_byte_register_value(&mut self, register: &Register, value: u8) {
        use Register::*;

        let word_value = value as u16;

        match register {
            AlAx => self.registers[REG_AX] = (self.registers[REG_AX] & 0xFF00) + word_value,
            ClCx => self.registers[REG_CX] = (self.registers[REG_CX] & 0xFF00) + word_value,
            DlDx => self.registers[REG_DX] = (self.registers[REG_DX] & 0xFF00) + word_value,
            BlBx => self.registers[REG_BX] = (self.registers[REG_BX] & 0xFF00) + word_value,
            AhSp => {
                self.registers[REG_AX] = (self.registers[REG_AX] & 0x00ff) + (word_value << 0x08)
            }
            ChBp => {
                self.registers[REG_CX] = (self.registers[REG_CX] & 0x00ff) + (word_value << 0x08)
            }
            DhSi => {
                self.registers[REG_DX] = (self.registers[REG_DX] & 0x00ff) + (word_value << 0x08)
            }
            BhDi => {
                self.registers[REG_BX] = (self.registers[REG_BX] & 0x00ff) + (word_value << 0x08)
            }
        }
    }

    fn set_word_register_value(&mut self, register: &Register, value: u16) {
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

    fn set_byte_result_flags(&mut self, result: i16) {
        if result == 0 {
            self.flags.insert(Flags::ZERO);
        } else {
            self.flags.remove(Flags::ZERO);
        }

        if result & 0x80 != 0 {
            self.flags.insert(Flags::SIGN);
        } else {
            self.flags.remove(Flags::SIGN);
        }

        // TODO: Set parity flag.
    }

    fn set_word_result_flags(&mut self, result: i32) {
        if result == 0 {
            self.flags.insert(Flags::ZERO);
        } else {
            self.flags.remove(Flags::ZERO);
        }

        if result & 0x8000 != 0 {
            self.flags.insert(Flags::SIGN);
        } else {
            self.flags.remove(Flags::SIGN);
        }

        // TODO: Set signed flag.
        // TODO: Set parity flag.
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
        let ss = self.get_segment_value(SEG_SS);
        let sp = self.get_word_register_value(&Register::AhSp);
        segment_and_offset(ss, sp)
    }

    fn adjust_stack_pointer(&mut self, offset: i16) {
        let sp = self.get_word_register_value(&Register::AhSp);
        let sp = (sp as i16).wrapping_add(offset) as u16;
        self.set_word_register_value(&Register::AhSp, sp);
    }
}

pub struct RegisterPack {
    ax: u16,
    bx: u16,
    cx: u16,
    dx: u16,
    sp: u16,
    bp: u16,
    si: u16,
    di: u16,
}

impl Default for RegisterPack {
    fn default() -> Self {
        Self {
            ax: 0u16,
            bx: 0u16,
            cx: 0u16,
            dx: 0u16,
            sp: 0u16,
            bp: 0u16,
            si: 0u16,
            di: 0u16,
        }
    }
}

pub struct CpuBuilder {
    cpu: Cpu,
}

impl Default for CpuBuilder {
    fn default() -> Self {
        Self {
            cpu: Cpu::new(Rc::new(RefCell::new(MemoryManager::new()))),
        }
    }
}

impl CpuBuilder {
    pub fn with_registers(&mut self, register_pack: RegisterPack) -> &mut Self {
        self.cpu.registers[REG_AX] = register_pack.ax;
        self.cpu.registers[REG_BX] = register_pack.bx;
        self.cpu.registers[REG_CX] = register_pack.cx;
        self.cpu.registers[REG_DX] = register_pack.dx;

        self.cpu.registers[REG_SP] = register_pack.sp;
        self.cpu.registers[REG_BP] = register_pack.bp;
        self.cpu.registers[REG_SI] = register_pack.si;
        self.cpu.registers[REG_DI] = register_pack.di;

        self
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_stack() {
        Cpu::build().with_registers(RegisterPack {
            ax: 0x0000,
            ..Default::default()
        });
    }
}
