use crate::bus::{segment_and_offset, Address};
use mrc_x86::{
    AddressingMode, Displacement, Instruction, Operand, OperandSet, OperandSize, OperandType,
    Operation, Register, Repeat, Segment,
};

use crate::cpu::executor::operations::{arithmetic, logic, SignificantBit};
use crate::cpu::{Flags, State};
use crate::error::{Error, Result};
use crate::io::IOInterface;

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
    let seg = cpu.state.get_segment_value(segment);

    let mut addr = match addressing_mode {
        AddressingMode::BxSi => {
            let bx = cpu.state.get_word_register_value(Register::BlBx);
            let si = cpu.state.get_word_register_value(Register::DhSi);
            segment_and_offset(seg, bx + si)
        }
        AddressingMode::BxDi => {
            let bx = cpu.state.get_word_register_value(Register::BlBx);
            let di = cpu.state.get_word_register_value(Register::BhDi);
            segment_and_offset(seg, bx + di)
        }
        AddressingMode::BpSi => {
            let bp = cpu.state.get_word_register_value(Register::ChBp);
            let si = cpu.state.get_word_register_value(Register::DhSi);
            segment_and_offset(seg, bp + si)
        }
        AddressingMode::BpDi => {
            let bp = cpu.state.get_word_register_value(Register::ChBp);
            let di = cpu.state.get_word_register_value(Register::BhDi);
            segment_and_offset(seg, bp + di)
        }
        AddressingMode::Si => {
            let si = cpu.state.get_word_register_value(Register::DhSi);
            segment_and_offset(seg, si)
        }
        AddressingMode::Di => {
            let di = cpu.state.get_word_register_value(Register::BhDi);
            segment_and_offset(seg, di)
        }
        AddressingMode::Bp => {
            let bp = cpu.state.get_word_register_value(Register::ChBp);
            segment_and_offset(seg, bp)
        }
        AddressingMode::Bx => {
            let bx = cpu.state.get_word_register_value(Register::BlBx);
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
                let ds = cpu.state.get_segment_value(*segment);
                let address = segment_and_offset(ds, *offset);
                byte::bus_read(cpu, address)
            }

            OperandType::Indirect(segment, addressing_mode, displacement) => {
                let address = indirect_address_for(cpu, *segment, addressing_mode, displacement);
                byte::bus_read(cpu, address)
            }

            OperandType::Register(register) => Ok(cpu.state.get_byte_register_value(*register)),

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
                let seg = cpu.state.get_segment_value(*segment);
                byte::bus_write(cpu, segment_and_offset(seg, *offset), value)
            }
            OperandType::Indirect(segment, addressing_mode, displacement) => {
                let addr = indirect_address_for(cpu, *segment, addressing_mode, displacement);
                byte::bus_write(cpu, addr, value)
            }
            OperandType::Register(register) => {
                cpu.state.set_byte_register_value(*register, value);
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
        let hi = byte::bus_read(cpu, address + 1)?;
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
                let seg = cpu.state.get_segment_value(*segment);
                word::bus_write(cpu, segment_and_offset(seg, *offset), value)
            }
            OperandType::Indirect(segment, addressing_mode, displacement) => {
                let addr = indirect_address_for(cpu, *segment, addressing_mode, displacement);
                word::bus_write(cpu, addr, value)
            }
            OperandType::Register(register) => {
                cpu.state.set_word_register_value(*register, value);
                Ok(())
            }
            OperandType::Segment(segment) => {
                cpu.state.set_segment_value(*segment, value);
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
                let seg = cpu.state.get_segment_value(*segment);
                let addr = segment_and_offset(seg, *offset);

                word::bus_read(cpu, addr)
            }

            OperandType::Indirect(segment, addressing_mode, displacement) => word::bus_read(
                cpu,
                indirect_address_for(cpu, *segment, addressing_mode, displacement),
            ),

            OperandType::Register(register) => Ok(cpu.state.get_word_register_value(*register)),
            OperandType::Segment(segment) => Ok(cpu.state.get_segment_value(*segment)),
            OperandType::Immediate(value) => Ok(*value),
        }
    }

    pub fn get_operand_value(cpu: &CPU, operand: &Operand) -> Result<u16> {
        assert_eq!(OperandSize::Word, operand.1);

        word::get_operand_type_value(cpu, &operand.0)
    }
}

fn push(cpu: &mut CPU, value: u16) -> Result<()> {
    let ss = cpu.state.get_segment_value(Segment::Ss);
    let mut sp = cpu.state.get_word_register_value(Register::AhSp);

    sp = sp.wrapping_sub(2);
    cpu.state.set_word_register_value(Register::AhSp, sp);

    // log::info!("Push {:04X} to [{:04X}:{:04X}]", value, ss, sp,);

    word::bus_write(cpu, segment_and_offset(ss, sp), value)
}

fn pop(cpu: &mut CPU) -> Result<u16> {
    let ss = cpu.state.get_segment_value(Segment::Ss);
    let mut sp = cpu.state.get_word_register_value(Register::AhSp);

    let value = word::bus_read(cpu, segment_and_offset(ss, sp))?;

    // log::info!("Pop {:04X} from [{:04X}:{:04X}]", value, ss, sp,);

    sp = sp.wrapping_add(2);
    cpu.state.set_word_register_value(Register::AhSp, sp);

    Ok(value)
}

fn displace_ip(state: &mut State, displacement: &Displacement) -> Result<()> {
    match displacement {
        Displacement::None => {}
        Displacement::Byte(offset) => {
            state.ip = ((state.ip as i32) + (*offset as i32)) as u16;
        }
        Displacement::Word(offset) => {
            state.ip = ((state.ip as i32) + (*offset as i32)) as u16;
        }
    }

    Ok(())
}

pub fn execute(cpu: &mut CPU, instruction: &Instruction) -> Result<ExecuteResult> {
    macro_rules! destination_and_source_ops {
        ($operation:expr,$d:expr,$s:expr,$size:ident) => {
            match $operation {
                Adc => Some(arithmetic::$size::add_with_carry(
                    $d,
                    $s,
                    &mut cpu.state.flags,
                )),
                Add => Some(arithmetic::$size::add($d, $s, &mut cpu.state.flags)),
                And => Some(logic::$size::and($d, $s, &mut cpu.state.flags)),
                Cmp => Some(arithmetic::$size::compare($d, $s, &mut cpu.state.flags)),
                Mul => Some(arithmetic::$size::multiply($d, $s, &mut cpu.state.flags)),
                Or => Some(logic::$size::or($d, $s, &mut cpu.state.flags)),
                Rol => Some(logic::$size::rotate_left($d, $s, &mut cpu.state.flags)),
                Ror => Some(logic::$size::rotate_right($d, $s, &mut cpu.state.flags)),
                Shl => Some(logic::$size::shift_left($d, $s, &mut cpu.state.flags)),
                Shr => Some(logic::$size::shift_right($d, $s, &mut cpu.state.flags)),
                Sub => Some(arithmetic::$size::subtract($d, $s, &mut cpu.state.flags)),
                Test => Some(logic::$size::test($d, $s, &mut cpu.state.flags)),
                Xor => Some(logic::$size::exclusive_or($d, $s, &mut cpu.state.flags)),
                _ => None,
            }
        };
    }

    match instruction.operands {
        OperandSet::DestinationAndSource(
            Operand(ref destination, OperandSize::Byte),
            Operand(ref source, OperandSize::Byte),
        ) => {
            use Operation::*;

            let d = byte::get_operand_type_value(cpu, destination)?;
            let s = byte::get_operand_type_value(cpu, source)?;
            if let Some(result) = destination_and_source_ops!(instruction.operation, d, s, byte) {
                if let Some(result) = result {
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
            if let Some(result) = destination_and_source_ops!(instruction.operation, d, s, word) {
                if let Some(result) = result {
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
                displace_ip(&mut cpu.state, displacement)?;
            }
            _ => todo!(),
        },

        Operation::Cbw => {
            let al = cpu.state.get_byte_register_value(Register::AlAx);
            if al.most_significant_bit() {
                cpu.state
                    .set_byte_register_value(Register::AhSp, 0b11111111);
            } else {
                cpu.state
                    .set_byte_register_value(Register::AhSp, 0b00000000);
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

        Operation::Esc => {
            // TODO: Implement FPU
        }

        Operation::Hlt => {
            log::info!("HALT");
            return Ok(ExecuteResult::Stop);
        }

        Operation::In => match instruction.operands {
            OperandSet::DestinationAndSource(ref destination, ref port) => {
                let port = match port.1 {
                    OperandSize::Byte => byte::get_operand_value(cpu, port)? as u16,
                    OperandSize::Word => word::get_operand_value(cpu, port)?,
                };

                match destination.1 {
                    OperandSize::Byte => {
                        let value = cpu.io_controller.borrow_mut().read(port)?;
                        byte::set_operand_value(cpu, destination, value)?;
                    }
                    OperandSize::Word => {
                        let value = cpu.io_controller.borrow_mut().read(port)? as u16;
                        word::set_operand_value(cpu, destination, value)?;
                    }
                };
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
                cpu.interrupt_controller.borrow().handle(*value as u8, cpu);
            }

            _ => illegal_operands(instruction),
        },

        Operation::Iret => match instruction.operands {
            OperandSet::None => {
                // TODO: Check stack limits
                // TODO: Check code segment limits

                cpu.state.ip = pop(cpu)?;
                let cs = pop(cpu)?;
                cpu.state.set_segment_value(Segment::Cs, cs);
                cpu.state.flags.bits = pop(cpu)?;
            }

            _ => illegal_operands(instruction),
        },

        Operation::Jb => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::CARRY) {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jbe => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::CARRY | Flags::ZERO) {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jcxz => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.get_word_register_value(Register::ClCx) == 0 {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Je => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::ZERO) {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jl => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::SIGN)
                    != cpu.state.flags.contains(Flags::OVERFLOW)
                {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jnl => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::SIGN)
                    == cpu.state.flags.contains(Flags::OVERFLOW)
                {
                    displace_ip(&mut cpu.state, displacement)?;
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
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jmp => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                displace_ip(&mut cpu.state, displacement)?;
            }
            OperandSet::SegmentAndOffset(segment, offset) => {
                cpu.state.set_segment_value(Segment::Cs, *segment);
                cpu.state.ip = *offset;
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jnb => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::CARRY) {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jnbe => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::CARRY) && !cpu.state.flags.contains(Flags::ZERO)
                {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jne => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::ZERO) {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jno => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::OVERFLOW) {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jnp => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::PARITY) {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jns => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::SIGN) {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jo => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::OVERFLOW) {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Jp => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::PARITY) {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Js => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::SIGN) {
                    displace_ip(&mut cpu.state, displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::Lahf => {
            // Use the low byte of the flags register.
            let bytes = cpu.state.flags.bits.to_le_bytes();
            cpu.state.set_byte_register_value(Register::AhSp, bytes[0]);
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
                cpu.state.set_word_register_value(register, addr as u16);
            }
            _ => illegal_operands(instruction),
        },

        Operation::Loop => match instruction.operands {
            OperandSet::Displacement(ref displacement) => {
                let cx = cpu.state.get_word_register_value(Register::ClCx);
                let cx = cx.wrapping_sub(1);
                cpu.state.set_word_register_value(Register::ClCx, cx);

                if cx > 0 {
                    displace_ip(&mut cpu.state, displacement)?;
                }
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

        Operation::Lodsb
        | Operation::Lodsw
        | Operation::Movsb
        | Operation::Movsw
        | Operation::Stosb
        | Operation::Stosw
        | Operation::Scasb
        | Operation::Scasw => {
            let value_size: u16 = match instruction.operation {
                Operation::Lodsb | Operation::Movsb | Operation::Stosb | Operation::Scasb => 1,
                Operation::Lodsw | Operation::Movsw | Operation::Stosw | Operation::Scasw => 2,
                _ => unreachable!(),
            };

            let ds = cpu.state.get_segment_value(Segment::Ds);
            let mut si = cpu.state.get_word_register_value(Register::DhSi);

            let es = cpu.state.get_segment_value(Segment::Es);
            let mut di = cpu.state.get_word_register_value(Register::BhDi);

            let mut count = match instruction.repeat {
                None => 1,
                Some(ref repeat) => match repeat {
                    Repeat::Equal => cpu.state.get_word_register_value(Register::ClCx),
                    Repeat::NotEqual => cpu.state.get_word_register_value(Register::ClCx),
                },
            };

            let reverse = cpu.state.flags.contains(Flags::DIRECTION);

            loop {
                match instruction.operation {
                    Operation::Lodsb => {
                        let value = byte::bus_read(cpu, segment_and_offset(ds, si))?;
                        cpu.state.set_byte_register_value(Register::AlAx, value);
                    }

                    Operation::Lodsw => {
                        let value = word::bus_read(cpu, segment_and_offset(ds, si))?;
                        cpu.state.set_word_register_value(Register::AlAx, value);
                    }

                    Operation::Movsb => {
                        let value = byte::bus_read(cpu, segment_and_offset(ds, si))?;
                        byte::bus_write(cpu, segment_and_offset(es, di), value)?;
                    }

                    Operation::Movsw => {
                        let value = word::bus_read(cpu, segment_and_offset(ds, si))?;
                        word::bus_write(cpu, segment_and_offset(es, di), value)?;
                    }

                    Operation::Stosb => {
                        let value = cpu.state.get_byte_register_value(Register::AlAx);
                        byte::bus_write(cpu, segment_and_offset(es, di), value)?;
                    }

                    Operation::Stosw => {
                        let value = cpu.state.get_word_register_value(Register::AlAx);
                        word::bus_write(cpu, segment_and_offset(es, di), value)?;
                    }

                    Operation::Scasb => {
                        let destination = byte::bus_read(cpu, segment_and_offset(ds, si))?;
                        let source = byte::bus_read(cpu, segment_and_offset(es, di))?;
                        let _ = operations::arithmetic::byte::compare(
                            destination,
                            source,
                            &mut cpu.state.flags,
                        );
                    }

                    Operation::Scasw => {
                        let destination = word::bus_read(cpu, segment_and_offset(ds, si))?;
                        let source = word::bus_read(cpu, segment_and_offset(es, di))?;
                        let _ = operations::arithmetic::word::compare(
                            destination,
                            source,
                            &mut cpu.state.flags,
                        );
                    }

                    _ => unreachable!(),
                };

                if reverse {
                    si = si.wrapping_sub(value_size);
                    di = di.wrapping_sub(value_size);
                } else {
                    si = si.wrapping_add(value_size);
                    di = di.wrapping_add(value_size);
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

            cpu.state.set_word_register_value(Register::DhSi, si);
            cpu.state.set_word_register_value(Register::BhDi, di);

            if instruction.repeat.is_some() {
                cpu.state.set_word_register_value(Register::ClCx, count);
            }
        }

        Operation::Nop => {}

        Operation::Not => match instruction.operands {
            OperandSet::Destination(Operand(ref destination, OperandSize::Byte)) => {
                let value = byte::get_operand_type_value(cpu, destination)?;
                if let Some(result) = logic::byte::not(value) {
                    byte::set_operand_type_value(cpu, destination, result)?;
                }
            }
            OperandSet::Destination(Operand(ref destination, OperandSize::Word)) => {
                let value = word::get_operand_type_value(cpu, destination)?;
                if let Some(result) = logic::word::not(value) {
                    word::set_operand_type_value(cpu, destination, result)?;
                }
            }

            _ => illegal_operands(instruction),
        },

        Operation::Out => match instruction.operands {
            OperandSet::DestinationAndSource(ref port, Operand(ref value, OperandSize::Byte)) => {
                let port = match port.1 {
                    OperandSize::Byte => byte::get_operand_value(cpu, port)? as u16,
                    OperandSize::Word => word::get_operand_value(cpu, port)?,
                };

                let value = byte::get_operand_type_value(cpu, value)?;
                cpu.io_controller.borrow_mut().write(port, value)?;

                if port == 0x60 || port == 0x80 {
                    log::info!("POST port out ({:04X}): {:02X}", port, value);
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

        Operation::Ret => {
            // Pop the return address from the stack.
            cpu.state.ip = pop(cpu)?;
        }

        // Shift left/right has a special case where a word value can be shifted by cl, which is a byte.
        Operation::Shl | Operation::Shr => {
            if let OperandSet::DestinationAndSource(
                Operand(ref destination, OperandSize::Word),
                Operand(OperandType::Register(Register::ClCx), OperandSize::Byte),
            ) = instruction.operands
            {
                let d = word::get_operand_type_value(cpu, destination)?;
                let s = cpu.state.get_byte_register_value(Register::ClCx) as u16;
                let result = match instruction.operation {
                    Operation::Shl => {
                        operations::logic::word::shift_left(d, s, &mut cpu.state.flags).unwrap()
                    }
                    Operation::Shr => {
                        operations::logic::word::shift_right(d, s, &mut cpu.state.flags).unwrap()
                    }
                    _ => unreachable!(),
                };
                word::set_operand_type_value(cpu, destination, result)?;
            }
        }

        Operation::Sahf => {
            let ah = cpu.state.get_byte_register_value(Register::AhSp);
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

        Operation::Xchg => match instruction.operands {
            OperandSet::DestinationAndSource(
                Operand(ref destination, OperandSize::Byte),
                Operand(ref source, OperandSize::Byte),
            ) => {
                let d = byte::get_operand_type_value(cpu, destination)?;
                let s = byte::get_operand_type_value(cpu, source)?;

                byte::set_operand_type_value(cpu, destination, s)?;
                byte::set_operand_type_value(cpu, source, d)?;
            }

            OperandSet::DestinationAndSource(
                Operand(ref destination, OperandSize::Word),
                Operand(ref source, OperandSize::Word),
            ) => {
                let d = word::get_operand_type_value(cpu, destination)?;
                let s = word::get_operand_type_value(cpu, source)?;

                word::set_operand_type_value(cpu, destination, s)?;
                word::set_operand_type_value(cpu, source, d)?;
            }

            _ => illegal_operands(instruction),
        },

        _ => {
            return Err(Error::IllegalInstruction);
        }
    }

    Ok(ExecuteResult::Continue)
}
