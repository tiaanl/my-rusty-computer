use mrc_x86::{
    AddressingMode, Displacement, Instruction, Operand, OperandSet, OperandSize, OperandType,
    Operation, Register, Repeat, Segment,
};

use crate::bus::Address;
use crate::cpu::executor::operations::{arithmetic, logic};
use crate::cpu::Flags;
use crate::error::{Error, Result};
use crate::segment_and_offset;

use super::CPU;

pub mod operations;

#[derive(PartialEq)]
pub enum ExecuteResult {
    Continue,
    Stop,
}

fn illegal_operands(instruction: &Instruction) {
    panic!("Illegal operands! {:?}", instruction)
}

fn indirect_address_for(
    cpu: &CPU,
    segment: Segment,
    addressing_mode: &AddressingMode,
    displacement: &Displacement,
) -> Address {
    let seg = cpu.get_segment_value(segment);

    let mut addr = match addressing_mode {
        AddressingMode::BxSi => {
            let bx = cpu.get_word_register_value(Register::BlBx);
            let si = cpu.get_word_register_value(Register::DhSi);
            segment_and_offset(seg, bx + si)
        }
        AddressingMode::BxDi => {
            let bx = cpu.get_word_register_value(Register::BlBx);
            let di = cpu.get_word_register_value(Register::BhDi);
            segment_and_offset(seg, bx + di)
        }
        AddressingMode::BpSi => {
            let bp = cpu.get_word_register_value(Register::ChBp);
            let si = cpu.get_word_register_value(Register::DhSi);
            segment_and_offset(seg, bp + si)
        }
        AddressingMode::BpDi => {
            let bp = cpu.get_word_register_value(Register::ChBp);
            let di = cpu.get_word_register_value(Register::BhDi);
            segment_and_offset(seg, bp + di)
        }
        AddressingMode::Si => {
            let si = cpu.get_word_register_value(Register::DhSi);
            segment_and_offset(seg, si)
        }
        AddressingMode::Di => {
            let di = cpu.get_word_register_value(Register::BhDi);
            segment_and_offset(seg, di)
        }
        AddressingMode::Bp => {
            let bp = cpu.get_word_register_value(Register::ChBp);
            segment_and_offset(seg, bp)
        }
        AddressingMode::Bx => {
            let bx = cpu.get_word_register_value(Register::BlBx);
            segment_and_offset(seg, bx)
        }
    } as i64;

    match displacement {
        Displacement::None => {}
        Displacement::Byte(offset) => addr += *offset as i64,
        Displacement::Word(offset) => addr += *offset as i64,
    }

    addr as Address
}

mod byte {
    use super::*;

    pub fn bus_read(cpu: &CPU, address: Address) -> Result<u8> {
        cpu.bus.borrow().read(address)
    }

    pub fn bus_write(cpu: &mut CPU, address: Address, value: u8) -> Result<()> {
        cpu.bus.borrow_mut().write(address, value)
    }

    pub fn get_operand_type_value(cpu: &CPU, operand_type: &OperandType) -> Result<u8> {
        match operand_type {
            OperandType::Direct(segment, offset) => {
                // TODO: Handle segment override.
                let ds = cpu.get_segment_value(*segment);
                let address = segment_and_offset(ds, *offset);
                byte::bus_read(cpu, address)
            }

            OperandType::Indirect(segment, addressing_mode, displacement) => {
                let address = indirect_address_for(cpu, *segment, addressing_mode, displacement);
                byte::bus_read(cpu, address)
            }

            OperandType::Register(register) => Ok(cpu.get_byte_register_value(*register)),

            OperandType::Segment(_) => Err(Error::IllegalDataAccess),

            OperandType::Immediate(value) => Ok(*value as u8),
        }
    }

    pub fn get_operand_value(cpu: &CPU, operand: &Operand) -> Result<u8> {
        assert_eq!(OperandSize::Byte, operand.1);
        byte::get_operand_type_value(cpu, &operand.0)
    }

    pub fn set_operand_type_value(
        cpu: &mut CPU,
        operand_type: &OperandType,
        value: u8,
    ) -> Result<()> {
        match operand_type {
            OperandType::Direct(segment, offset) => {
                // TODO: Handle segment override.
                let seg = cpu.get_segment_value(*segment);
                byte::bus_write(cpu, segment_and_offset(seg, *offset), value)
            }
            OperandType::Indirect(segment, addressing_mode, displacement) => {
                let addr = indirect_address_for(cpu, *segment, addressing_mode, displacement);
                byte::bus_write(cpu, addr, value)
            }
            OperandType::Register(register) => {
                cpu.set_byte_register_value(*register, value);
                Ok(())
            }
            _ => Err(Error::IllegalDataAccess),
        }
    }

    pub fn set_operand_value(cpu: &mut CPU, operand: &Operand, value: u8) -> Result<()> {
        assert_eq!(OperandSize::Byte, operand.1);
        byte::set_operand_type_value(cpu, &operand.0, value)
    }
}

mod word {
    use super::*;

    pub fn bus_read(cpu: &CPU, address: Address) -> Result<u16> {
        let lo = byte::bus_read(cpu, address)?;
        let hi = byte::bus_read(cpu, address)?;
        Ok(u16::from_le_bytes([lo, hi]))
    }

    pub fn bus_write(cpu: &mut CPU, address: Address, value: u16) -> Result<()> {
        let bytes = value.to_le_bytes();
        byte::bus_write(cpu, address, bytes[0])?;
        byte::bus_write(cpu, address + 1, bytes[1])?;
        Ok(())
    }

    pub fn set_operand_type_value(
        cpu: &mut CPU,
        operand_type: &OperandType,
        value: u16,
    ) -> Result<()> {
        match operand_type {
            OperandType::Direct(segment, offset) => {
                // TODO: Handle segment override.
                let seg = cpu.get_segment_value(*segment);
                word::bus_write(cpu, segment_and_offset(seg, *offset), value)
            }
            OperandType::Indirect(segment, addressing_mode, displacement) => {
                let addr = indirect_address_for(cpu, *segment, addressing_mode, displacement);
                word::bus_write(cpu, addr, value)
            }
            OperandType::Register(register) => {
                cpu.set_word_register_value(*register, value);
                Ok(())
            }
            OperandType::Segment(segment) => {
                cpu.set_segment_value(*segment, value);
                Ok(())
            }
            _ => Err(Error::IllegalDataAccess),
        }
    }

    pub fn set_operand_value(cpu: &mut CPU, operand: &Operand, value: u16) -> Result<()> {
        assert_eq!(OperandSize::Word, operand.1);
        set_operand_type_value(cpu, &operand.0, value)
    }

    pub fn get_operand_type_value(cpu: &CPU, operand_type: &OperandType) -> Result<u16> {
        match operand_type {
            OperandType::Direct(segment, offset) => {
                // Get a single byte from DS:offset
                let seg = cpu.get_segment_value(*segment);
                let addr = segment_and_offset(seg, *offset);

                word::bus_read(cpu, addr)
            }

            OperandType::Indirect(segment, addressing_mode, displacement) => word::bus_read(
                cpu,
                indirect_address_for(cpu, *segment, addressing_mode, displacement),
            ),

            OperandType::Register(register) => Ok(cpu.get_word_register_value(*register)),
            OperandType::Segment(segment) => Ok(cpu.get_segment_value(*segment)),
            OperandType::Immediate(value) => Ok(*value),
        }
    }

    pub fn get_operand_value(cpu: &CPU, operand: &Operand) -> Result<u16> {
        assert_eq!(OperandSize::Word, operand.1);

        word::get_operand_type_value(cpu, &operand.0)
    }
}

fn push(cpu: &mut CPU, value: u16) -> Result<()> {
    let ss = cpu.get_segment_value(Segment::Ss);
    let sp = cpu.get_word_register_value(Register::AhSp);
    let stack_pointer = segment_and_offset(ss, sp);

    let result = word::bus_write(cpu, stack_pointer, value);

    let sp = sp.wrapping_sub(2);
    cpu.set_word_register_value(Register::AhSp, sp);

    result
}

fn pop(cpu: &mut CPU) -> Result<u16> {
    let mut sp = cpu.get_word_register_value(Register::AhSp);
    sp = sp.wrapping_add(2);
    cpu.set_word_register_value(Register::AhSp, sp);

    let ss = cpu.get_segment_value(Segment::Ss);

    word::bus_read(cpu, segment_and_offset(ss, sp))
}

fn displace_ip(cpu: &mut CPU, displacement: &Displacement) -> Result<()> {
    match displacement {
        Displacement::None => {}
        Displacement::Byte(offset) => {
            cpu.state.ip = ((cpu.state.ip as i32) + (*offset as i32)) as u16;
        }
        Displacement::Word(offset) => {
            cpu.state.ip = ((cpu.state.ip as i32) + (*offset as i32)) as u16;
        }
    }

    Ok(())
}

pub fn execute(cpu: &mut CPU, instruction: &Instruction) -> Result<ExecuteResult> {
    match instruction.operands {
        OperandSet::DestinationAndSource(
            Operand(ref destination, OperandSize::Byte),
            Operand(ref source, OperandSize::Byte),
        ) => {
            use Operation::*;

            let d = byte::get_operand_type_value(cpu, destination)?;
            let s = byte::get_operand_type_value(cpu, source)?;
            if let Some(did_operation) = match instruction.operation {
                Adc => Some(arithmetic::add_with_carry_byte(d, s, &mut cpu.state.flags)),
                Add => Some(arithmetic::add_byte(d, s, &mut cpu.state.flags)),
                And => Some(logic::and_byte(d, s, &mut cpu.state.flags)),
                Cmp => Some(arithmetic::compare_byte(d, s, &mut cpu.state.flags)),
                Mul => Some(arithmetic::multiply_byte(d, s, &mut cpu.state.flags)),
                Or => Some(logic::or_byte(d, s, &mut cpu.state.flags)),
                Rol => Some(logic::rol_byte(d, s, &mut cpu.state.flags)),
                Shl => Some(logic::shift_left_byte(d, s, &mut cpu.state.flags)),
                Shr => Some(logic::shift_right_byte(d, s, &mut cpu.state.flags)),
                Sub => Some(arithmetic::sub_byte(d, s, &mut cpu.state.flags)),
                Test => Some(logic::test_byte(d, s, &mut cpu.state.flags)),
                Xor => Some(logic::xor_byte(d, s, &mut cpu.state.flags)),
                _ => None,
            } {
                if let Some(result) = did_operation {
                    byte::set_operand_type_value(cpu, destination, result)?;
                }
                return Ok(ExecuteResult::Continue);
            }
        }

        OperandSet::DestinationAndSource(
            Operand(ref destination, OperandSize::Word),
            Operand(ref source, OperandSize::Word),
        ) => {
            use Operation::*;

            let d = word::get_operand_type_value(cpu, destination)?;
            let s = word::get_operand_type_value(cpu, source)?;
            if let Some(did_operation) = match instruction.operation {
                Adc => Some(arithmetic::add_with_carry_word(d, s, &mut cpu.state.flags)),
                Add => Some(arithmetic::add_word(d, s, &mut cpu.state.flags)),
                And => Some(logic::and_word(d, s, &mut cpu.state.flags)),
                Cmp => Some(arithmetic::compare_word(d, s, &mut cpu.state.flags)),
                Mul => Some(arithmetic::multiply_word(d, s, &mut cpu.state.flags)),
                Or => Some(logic::or_word(d, s, &mut cpu.state.flags)),
                Rol => Some(logic::rol_word(d, s, &mut cpu.state.flags)),
                Shl => Some(logic::shift_left_word(d, s, &mut cpu.state.flags)),
                Shr => Some(logic::shift_right_word(d, s, &mut cpu.state.flags)),
                Sub => Some(arithmetic::sub_word(d, s, &mut cpu.state.flags)),
                Test => Some(logic::test_word(d, s, &mut cpu.state.flags)),
                Xor => Some(logic::xor_word(d, s, &mut cpu.state.flags)),
                _ => None,
            } {
                if let Some(result) = did_operation {
                    word::set_operand_type_value(cpu, destination, result)?;
                }
                return Ok(ExecuteResult::Continue);
            }
        }

        _ => {}
    }

    match instruction.operation {
        Operation::Call => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                // Store the current IP (which is after this CALL) on the stack. So that RET
                // can pop it.
                push(cpu, cpu.state.ip)?;
                displace_ip(cpu, displacement)?;
            }
            _ => todo!(),
        },

        Operation::Cbw => {
            let al = cpu.get_byte_register_value(Register::AlAx);
            if al & 0b10000000 != 0 {
                cpu.set_byte_register_value(Register::AhSp, 0b11111111);
            } else {
                cpu.set_byte_register_value(Register::AhSp, 0b00000000);
            }
        }

        Operation::Clc => {
            cpu.state.flags.remove(Flags::CARRY);
        }

        Operation::Cld => {
            cpu.state.flags.remove(Flags::DIRECTION);
        }

        Operation::Cli => {
            cpu.state.flags.remove(Flags::INTERRUPT);
        }

        Operation::Dec => match instruction.operands {
            OperandSet::Destination(Operand(ref destination, OperandSize::Byte)) => {
                let mut value = byte::get_operand_type_value(cpu, destination)?;
                value = value.wrapping_sub(1);
                byte::set_operand_type_value(cpu, destination, value)?;
            }
            OperandSet::Destination(Operand(ref destination, OperandSize::Word)) => {
                let mut value = word::get_operand_type_value(cpu, destination)?;
                value = value.wrapping_sub(1);
                word::set_operand_type_value(cpu, destination, value)?;
            }
            _ => illegal_operands(instruction),
        },

        Operation::Hlt => {
            log::info!("HALT");
            return Ok(ExecuteResult::Stop);
        }

        Operation::In => match instruction.operands {
            OperandSet::DestinationAndSource(
                Operand(ref destination_type, OperandSize::Byte),
                Operand(OperandType::Immediate(port), OperandSize::Byte),
            ) => {
                if let Some(io_controller) = &cpu.io_controller {
                    let value = io_controller.borrow().read_byte(port)?;
                    byte::set_operand_type_value(cpu, destination_type, value)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Inc => match &instruction.operands {
            OperandSet::Destination(destination) => match destination.1 {
                OperandSize::Byte => {
                    let mut value = byte::get_operand_value(cpu, destination)?;
                    value = value.wrapping_add(1);
                    byte::set_operand_value(cpu, destination, value)?;
                }
                OperandSize::Word => {
                    let mut value = word::get_operand_value(cpu, destination)?;
                    value = value.wrapping_add(1);
                    word::set_operand_value(cpu, destination, value)?;
                }
            },
            _ => illegal_operands(instruction),
        },

        Operation::Int => match &instruction.operands {
            OperandSet::Destination(Operand(OperandType::Immediate(value), OperandSize::Byte)) => {
                if *value == 0x21 {
                    // DOS
                    match cpu.get_word_register_value(Register::AlAx).to_le_bytes() {
                        [0x4C, return_code] => {
                            println!("INT 21h: DOS exit with return code ({})", return_code);
                            return Ok(ExecuteResult::Stop);
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
                if cpu.state.flags.contains(Flags::CARRY) {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jbe => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::CARRY | Flags::ZERO) {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Je => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::ZERO) {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jl => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::SIGN)
                    != cpu.state.flags.contains(Flags::OVERFLOW)
                {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jle => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::ZERO)
                    || cpu.state.flags.contains(Flags::SIGN)
                    != cpu.state.flags.contains(Flags::OVERFLOW)
                {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jmp => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                displace_ip(cpu, displacement)?;
            }
            OperandSet::SegmentAndOffset(segment, offset) => {
                cpu.set_segment_value(Segment::Cs, *segment);
                cpu.state.ip = *offset;
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jnb => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::CARRY) {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jnbe => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::CARRY) && !cpu.state.flags.contains(Flags::ZERO)
                {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jne => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::ZERO) {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jno => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::OVERFLOW) {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jnp => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::PARITY) {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jns => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::SIGN) {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jo => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::OVERFLOW) {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jp => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::PARITY) {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Js => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::SIGN) {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Lahf => {
            // Use the low byte of the flags register.
            let bytes = cpu.state.flags.bits.to_le_bytes();
            cpu.set_byte_register_value(Register::AhSp, bytes[0]);
        }

        Operation::Lea => match instruction.operands {
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(register), OperandSize::Word),
                Operand(
                    OperandType::Indirect(segment, ref addressing_mode, ref displacement),
                    OperandSize::Word,
                ),
            ) => {
                let addr = indirect_address_for(cpu, segment, addressing_mode, displacement);
                cpu.set_word_register_value(register, addr as u16);
            }
            _ => illegal_operands(instruction),
        },

        Operation::Loop => match instruction.operands {
            OperandSet::Displacement(ref displacement) => {
                let cx = cpu.get_word_register_value(Register::ClCx);
                let cx = cx.wrapping_sub(1);
                cpu.set_word_register_value(Register::ClCx, cx);

                if cx > 0 {
                    displace_ip(cpu, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Nop => {}

        Operation::Out => match instruction.operands {
            OperandSet::DestinationAndSource(ref port, ref value) => {
                if let Some(io_controller) = &cpu.io_controller {
                    let port = match port.1 {
                        OperandSize::Byte => byte::get_operand_value(cpu, port)? as u16,
                        OperandSize::Word => word::get_operand_value(cpu, port)?,
                    };

                    let value = match value.1 {
                        OperandSize::Byte => io_controller
                            .borrow_mut()
                            .write_byte(port, byte::get_operand_value(cpu, value)?),
                        OperandSize::Word => io_controller
                            .borrow_mut()
                            .write_word(port, word::get_operand_value(cpu, value)?),
                    };
                }
            }

            _ => illegal_operands(instruction),
        },

        Operation::Push => match &instruction.operands {
            OperandSet::Destination(destination) => {
                let value = word::get_operand_value(cpu, destination)?;
                push(cpu, value)?;
            }
            _ => illegal_operands(instruction),
        },

        Operation::Pop => match &instruction.operands {
            OperandSet::Destination(destination) => {
                let value = pop(cpu)?;
                word::set_operand_value(cpu, destination, value)?;
            }
            _ => illegal_operands(instruction),
        },

        Operation::Mov => match &instruction.operands {
            OperandSet::DestinationAndSource(
                Operand(destination, OperandSize::Byte),
                Operand(source, OperandSize::Byte),
            ) => {
                let source_value = byte::get_operand_type_value(cpu, source)?;
                byte::set_operand_type_value(cpu, destination, source_value)?;
            }
            OperandSet::DestinationAndSource(
                Operand(destination, OperandSize::Word),
                Operand(source, OperandSize::Word),
            ) => {
                let source_value = word::get_operand_type_value(cpu, source)?;
                word::set_operand_type_value(cpu, destination, source_value)?;
            }
            _ => illegal_operands(instruction),
        },

        Operation::Movsb | Operation::Movsw | Operation::Stosb | Operation::Stosw => {
            let value_size: u16 = match instruction.operation {
                Operation::Movsb | Operation::Stosb => 1,
                Operation::Movsw | Operation::Stosw => 2,
                _ => unreachable!(),
            };

            let ds = cpu.get_segment_value(Segment::Ds);
            let mut si = cpu.get_word_register_value(Register::DhSi);

            let es = cpu.get_segment_value(Segment::Es);
            let mut di = cpu.get_word_register_value(Register::BhDi);

            let mut count = match instruction.repeat {
                None => 1,
                Some(ref repeat) => match repeat {
                    Repeat::Equal => cpu.get_word_register_value(Register::ClCx),
                    Repeat::NotEqual => cpu.get_word_register_value(Register::ClCx),
                },
            };

            let forward = cpu.state.flags.contains(Flags::DIRECTION);

            loop {
                match instruction.operation {
                    Operation::Movsb => {
                        let value = byte::bus_read(cpu, segment_and_offset(ds, si))?;
                        byte::bus_write(cpu, segment_and_offset(es, di), value)?;
                    }

                    Operation::Movsw => {
                        let value = word::bus_read(cpu, segment_and_offset(ds, si))?;
                        word::bus_write(cpu, segment_and_offset(es, di), value)?;
                    }

                    Operation::Stosb => {
                        let value = cpu.get_byte_register_value(Register::AlAx);
                        byte::bus_write(cpu, segment_and_offset(es, di), value)?;
                    }

                    Operation::Stosw => {
                        let value = cpu.get_word_register_value(Register::AlAx);
                        word::bus_write(cpu, segment_and_offset(es, di), value)?;
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
                            if cpu.state.flags.contains(Flags::ZERO) {
                                break;
                            }
                        }
                        Repeat::NotEqual => {
                            if !cpu.state.flags.contains(Flags::ZERO) {
                                break;
                            }
                        }
                    }
                }
            }

            cpu.set_word_register_value(Register::DhSi, si + 1);
            cpu.set_word_register_value(Register::BhDi, di + 1);

            if instruction.repeat.is_some() {
                cpu.set_word_register_value(Register::ClCx, count);
            }
        }

        Operation::Ret => {
            // Pop the return address from the stack.
            cpu.state.ip = pop(cpu)?;
        }

        Operation::Sahf => {
            let ah = cpu.get_byte_register_value(Register::AhSp);
            cpu.state.flags.bits = u16::from_le_bytes([ah, 0]);
        }

        Operation::Stc => {
            cpu.state.flags.insert(Flags::CARRY);
        }

        Operation::Std => {
            cpu.state.flags.insert(Flags::DIRECTION);
        }

        Operation::Sti => {
            cpu.state.flags.insert(Flags::INTERRUPT);
        }

        _ => {
            return Err(Error::IllegalInstruction);
        }
    }

    Ok(ExecuteResult::Continue)
}
