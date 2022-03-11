pub mod operations;

use crate::{
    cpu::{
        executor::operations::{SignificantBit, StateExt},
        Flags, CPU,
    },
    error::{Error, Result},
    segment_and_offset, Address, Bus, Port,
};
use mrc_instruction::{
    AddressingMode, Displacement, Immediate, Instruction, Operand, OperandSet, OperandSize,
    Operation, Register, Repeat, Segment, SizedRegister,
};

#[derive(PartialEq)]
pub enum ExecuteResult {
    Continue,
    Stop,
}

impl<D: Bus<Address>, I: Bus<Port>> CPU<D, I> {
    fn push(&mut self, value: u16) -> Result<()> {
        self.state.set_word_register_value(
            Register::AhSp,
            self.state
                .get_word_register_value(Register::AhSp)
                .wrapping_sub(2),
        );

        word::bus_write(
            &mut self.bus,
            segment_and_offset(
                self.state.get_segment_value(Segment::SS),
                self.state.get_word_register_value(Register::AhSp),
            ),
            value,
        )?;

        // log::info!("Push {:04X} to [{:04X}:{:04X}]", value, ss, sp,);

        Ok(())
    }

    fn pop(&mut self) -> Result<u16> {
        let value = word::bus_read(
            &self.bus,
            segment_and_offset(
                self.state.get_segment_value(Segment::SS),
                self.state.get_word_register_value(Register::AhSp),
            ),
        )?;

        // log::info!("Pop {:04X} from [{:04X}:{:04X}]", value, ss, sp,);

        self.state.set_word_register_value(
            Register::AhSp,
            self.state
                .get_word_register_value(Register::AhSp)
                .wrapping_add(2),
        );

        Ok(value)
    }

    fn displace_ip(&mut self, displacement: &Displacement) -> Result<()> {
        match displacement {
            Displacement::None => {}
            Displacement::Byte(offset) => {
                self.state.ip = ((self.state.ip as i32) + (*offset as i32)) as u16;
            }
            Displacement::Word(offset) => {
                self.state.ip = ((self.state.ip as i32) + (*offset as i32)) as u16;
            }
        }

        Ok(())
    }
}

fn illegal_operands(instruction: &Instruction) {
    panic!("Illegal operands! {:?}", instruction)
}

fn indirect_address_for<D: Bus<Address>, I: Bus<Port>>(
    cpu: &CPU<D, I>,
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
    use mrc_instruction::{Immediate, SizedRegister};

    pub fn bus_read(bus: &impl Bus<Address>, address: Address) -> Result<u8> {
        bus.read(address)
    }

    pub fn bus_write(bus: &mut impl Bus<Address>, address: Address, value: u8) -> Result<()> {
        bus.write(address, value)
    }

    pub fn get_operand_type_value<D: Bus<Address>, I: Bus<Port>>(
        cpu: &CPU<D, I>,
        operand_type: &Operand,
    ) -> Result<u8> {
        match operand_type {
            Operand::Direct(segment, offset, OperandSize::Byte) => {
                // TODO: Handle segment override.
                let ds = cpu.state.get_segment_value(*segment);
                let address = segment_and_offset(ds, *offset);
                byte::bus_read(&cpu.bus, address)
            }

            Operand::Indirect(segment, addressing_mode, displacement, OperandSize::Byte) => {
                let address = indirect_address_for(cpu, *segment, addressing_mode, displacement);
                byte::bus_read(&cpu.bus, address)
            }

            Operand::Register(SizedRegister(register, OperandSize::Byte)) => {
                Ok(cpu.state.get_byte_register_value(*register))
            }

            Operand::Immediate(Immediate::Byte(value)) => Ok(*value as u8),

            _ => {
                log::warn!("Invalid data access");
                Ok(0)
            }
        }
    }

    pub fn get_operand_value<D: Bus<Address>, I: Bus<Port>>(
        cpu: &CPU<D, I>,
        operand: &Operand,
    ) -> Result<u8> {
        assert_eq!(OperandSize::Byte, operand.operand_size());
        byte::get_operand_type_value(cpu, operand)
    }

    pub fn set_operand_type_value<D: Bus<Address>, I: Bus<Port>>(
        cpu: &mut CPU<D, I>,
        operand_type: &Operand,
        value: u8,
    ) -> Result<()> {
        match operand_type {
            Operand::Direct(segment, offset, OperandSize::Byte) => {
                // TODO: Handle segment override.
                let seg = cpu.state.get_segment_value(*segment);
                byte::bus_write(&mut cpu.bus, segment_and_offset(seg, *offset), value)
            }

            Operand::Indirect(segment, addressing_mode, displacement, OperandSize::Byte) => {
                let addr = indirect_address_for(cpu, *segment, addressing_mode, displacement);
                byte::bus_write(&mut cpu.bus, addr, value)
            }

            Operand::Register(SizedRegister(register, OperandSize::Byte)) => {
                cpu.state.set_byte_register_value(*register, value);
                Ok(())
            }

            _ => Err(Error::IllegalDataAccess),
        }
    }

    pub fn set_operand_value<D: Bus<Address>, I: Bus<Port>>(
        cpu: &mut CPU<D, I>,
        operand: &Operand,
        value: u8,
    ) -> Result<()> {
        assert_eq!(OperandSize::Byte, operand.operand_size());
        byte::set_operand_type_value(cpu, operand, value)
    }
}

mod word {
    use super::*;
    use crate::error::Error::IllegalDataAccess;
    use mrc_instruction::{Immediate, SizedRegister};

    pub fn bus_read(bus: &impl Bus<Address>, address: Address) -> Result<u16> {
        let lo = byte::bus_read(bus, address)?;
        let hi = byte::bus_read(bus, address + 1)?;
        Ok(u16::from_le_bytes([lo, hi]))
    }

    pub fn bus_write(bus: &mut impl Bus<Address>, address: Address, value: u16) -> Result<()> {
        let bytes = value.to_le_bytes();
        byte::bus_write(bus, address, bytes[0])?;
        byte::bus_write(bus, address + 1, bytes[1])?;
        Ok(())
    }

    pub fn set_operand_type_value<D: Bus<Address>, I: Bus<Port>>(
        cpu: &mut CPU<D, I>,
        operand_type: &Operand,
        value: u16,
    ) -> Result<()> {
        match operand_type {
            Operand::Direct(segment, offset, OperandSize::Word) => {
                // TODO: Handle segment override.
                let seg = cpu.state.get_segment_value(*segment);
                word::bus_write(&mut cpu.bus, segment_and_offset(seg, *offset), value)
            }

            Operand::Indirect(segment, addressing_mode, displacement, OperandSize::Word) => {
                let addr = indirect_address_for(cpu, *segment, addressing_mode, displacement);
                word::bus_write(&mut cpu.bus, addr, value)
            }

            Operand::Register(SizedRegister(register, OperandSize::Word)) => {
                cpu.state.set_word_register_value(*register, value);
                Ok(())
            }

            Operand::Segment(segment) => {
                cpu.state.set_segment_value(*segment, value);
                Ok(())
            }

            _ => Err(Error::IllegalDataAccess),
        }
    }

    pub fn set_operand_value<D: Bus<Address>, I: Bus<Port>>(
        cpu: &mut CPU<D, I>,
        operand: &Operand,
        value: u16,
    ) -> Result<()> {
        assert_eq!(OperandSize::Word, operand.operand_size());
        set_operand_type_value(cpu, operand, value)
    }

    pub fn get_operand_type_value<D: Bus<Address>, I: Bus<Port>>(
        cpu: &CPU<D, I>,
        operand_type: &Operand,
    ) -> Result<u16> {
        match operand_type {
            Operand::Direct(segment, offset, OperandSize::Word) => {
                // Get a single byte from DS:offset
                let seg = cpu.state.get_segment_value(*segment);
                let addr = segment_and_offset(seg, *offset);

                word::bus_read(&cpu.bus, addr)
            }

            Operand::Indirect(segment, addressing_mode, displacement, OperandSize::Word) => {
                word::bus_read(
                    &cpu.bus,
                    indirect_address_for(cpu, *segment, addressing_mode, displacement),
                )
            }

            Operand::Register(SizedRegister(register, OperandSize::Word)) => {
                Ok(cpu.state.get_word_register_value(*register))
            }

            Operand::Segment(segment) => Ok(cpu.state.get_segment_value(*segment)),

            Operand::Immediate(Immediate::Word(value)) => Ok(*value),

            _ => Err(IllegalDataAccess),
        }
    }

    pub fn get_operand_value<D: Bus<Address>, I: Bus<Port>>(
        cpu: &CPU<D, I>,
        operand: &Operand,
    ) -> Result<u16> {
        assert_eq!(OperandSize::Word, operand.operand_size());

        word::get_operand_type_value(cpu, operand)
    }
}

pub fn execute<D: Bus<Address>, I: Bus<Port>>(
    cpu: &mut CPU<D, I>,
    instruction: &Instruction,
) -> Result<ExecuteResult> {
    macro_rules! destination_and_source_ops {
        ($operation:expr,$d:expr,$s:expr,$size:ident) => {{
            use operations as op;
            use Operation::*;
            match $operation {
                ADC => Some(op::$size::add_with_carry($d, $s, &mut cpu.state.flags)),
                ADD => Some(op::$size::add($d, $s, &mut cpu.state.flags)),
                AND => Some(op::$size::and($d, $s, &mut cpu.state.flags)),
                CMP => Some(op::$size::compare($d, $s, &mut cpu.state.flags)),
                MUL => Some(op::$size::multiply($d, $s, &mut cpu.state.flags)),
                OR => Some(op::$size::or($d, $s, &mut cpu.state.flags)),
                ROL => Some(op::$size::rotate_left($d, $s, &mut cpu.state.flags)),
                ROR => Some(op::$size::rotate_right($d, $s, &mut cpu.state.flags)),
                SHL => Some(op::$size::shift_left($d, $s, &mut cpu.state.flags)),
                SHR => Some(op::$size::shift_right($d, $s, &mut cpu.state.flags)),
                SUB => Some(op::$size::subtract($d, $s, &mut cpu.state.flags)),
                TEST => Some(op::$size::test($d, $s, &mut cpu.state.flags)),
                XOR => Some(op::$size::exclusive_or($d, $s, &mut cpu.state.flags)),
                _ => None,
            }
        }};
    }

    if let OperandSet::DestinationAndSource(ref destination, ref source) = instruction.operands {
        if destination.operand_size() == source.operand_size() {
            match destination.operand_size() {
                OperandSize::Byte => {
                    let d = byte::get_operand_type_value(cpu, destination)?;
                    let s = byte::get_operand_type_value(cpu, source)?;
                    if let Some(result) =
                        destination_and_source_ops!(instruction.operation, d, s, byte)
                    {
                        if let Some(result) = result {
                            byte::set_operand_type_value(cpu, destination, result)?;
                        }
                        return Ok(ExecuteResult::Continue);
                    }
                }
                OperandSize::Word => {
                    let d = word::get_operand_type_value(cpu, destination)?;
                    let s = word::get_operand_type_value(cpu, source)?;
                    if let Some(result) =
                        destination_and_source_ops!(instruction.operation, d, s, word)
                    {
                        if let Some(result) = result {
                            word::set_operand_type_value(cpu, destination, result)?;
                        }
                        return Ok(ExecuteResult::Continue);
                    }
                }
            }
        }
    }

    match instruction.operation {
        Operation::CALL => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                // Store the current IP (which is after this CALL) on the stack. So that RET
                // can pop it.
                cpu.push(cpu.state.ip)?;
                cpu.displace_ip(displacement)?;
            }
            _ => todo!(),
        },

        Operation::CBW => {
            let al = cpu.state.get_byte_register_value(Register::AlAx);
            if al.most_significant_bit() {
                cpu.state
                    .set_byte_register_value(Register::AhSp, 0b11111111);
            } else {
                cpu.state
                    .set_byte_register_value(Register::AhSp, 0b00000000);
            }
        }

        Operation::CLC => {
            cpu.state.flags.remove(Flags::CARRY);
        }

        Operation::CLD => {
            cpu.state.flags.remove(Flags::DIRECTION);
        }

        Operation::CLI => {
            cpu.state.flags.remove(Flags::INTERRUPT);
        }

        Operation::DEC => match instruction.operands {
            OperandSet::Destination(ref destination) => {
                match destination.operand_size() {
                    OperandSize::Byte => {
                        let mut value = byte::get_operand_type_value(cpu, destination)?;
                        value = operations::byte::decrement(value, &mut cpu.state.flags).unwrap();
                        byte::set_operand_type_value(cpu, destination, value)?
                    }
                    OperandSize::Word => {
                        let mut value = word::get_operand_type_value(cpu, destination)?;
                        value = operations::word::decrement(value, &mut cpu.state.flags).unwrap();
                        word::set_operand_type_value(cpu, destination, value)?
                    }
                };
            }
            _ => illegal_operands(instruction),
        },

        Operation::ESC => {
            // TODO: Implement FPU
        }

        Operation::HLT => {
            log::info!("HALT");
            return Ok(ExecuteResult::Stop);
        }

        Operation::IN => match instruction.operands {
            OperandSet::DestinationAndSource(ref destination, ref port) => {
                let port = match port.operand_size() {
                    OperandSize::Byte => byte::get_operand_value(cpu, port)? as u16,
                    OperandSize::Word => word::get_operand_value(cpu, port)?,
                };

                match destination.operand_size() {
                    OperandSize::Byte => {
                        let value = cpu.io_controller.read(port)?;
                        byte::set_operand_value(cpu, destination, value)?;
                    }
                    OperandSize::Word => {
                        let value = cpu.io_controller.read(port)? as u16;
                        word::set_operand_value(cpu, destination, value)?;
                    }
                };
            }

            _ => illegal_operands(instruction),
        },

        Operation::INC => match &instruction.operands {
            OperandSet::Destination(destination) => match destination.operand_size() {
                OperandSize::Byte => {
                    let mut value = byte::get_operand_value(cpu, destination)?;
                    value = operations::byte::increment(value, &mut cpu.state.flags).unwrap();
                    byte::set_operand_value(cpu, destination, value)?;
                }
                OperandSize::Word => {
                    let mut value = word::get_operand_value(cpu, destination)?;
                    value = operations::word::increment(value, &mut cpu.state.flags).unwrap();
                    word::set_operand_value(cpu, destination, value)?;
                }
            },
            _ => illegal_operands(instruction),
        },

        Operation::INT => match instruction.operands {
            OperandSet::Destination(Operand::Immediate(Immediate::Byte(index))) => {
                log::info!("Calling interrupt {}", index);

                cpu.push(cpu.state.flags.bits)?;
                cpu.push(cpu.state.segments.cs)?;
                cpu.push(cpu.state.ip)?;

                cpu.state.flags.set(Flags::INTERRUPT, false);
                cpu.state.flags.set(Flags::TRAP, false);

                // The very top of memory.
                let idt = segment_and_offset(0x0000, 0x0000);
                let addr = idt + 4u32 * index as u32;

                let new_ip = word::bus_read(&cpu.bus, addr)?;
                let new_cs = word::bus_read(&cpu.bus, addr + 2)?;

                // If the far call points to 0000:0000 again, this is probably an invalid interrupt
                // vector.
                if new_ip == 0 && new_cs == 0 {
                    // TODO: Probably not the best error to return here.
                    return Err(Error::IllegalInstruction);
                }

                cpu.state.ip = new_ip;
                cpu.state.segments.cs = new_cs;
            }

            _ => illegal_operands(instruction),
        },

        Operation::IRET => match instruction.operands {
            OperandSet::None => {
                cpu.state.ip = cpu.pop()?;
                cpu.state.segments.cs = cpu.pop()?;
                cpu.state.flags.bits = cpu.pop()?;
            }

            _ => illegal_operands(instruction),
        },

        Operation::JB => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::CARRY) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JBE => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::CARRY | Flags::ZERO) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JCXZ => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.get_word_register_value(Register::ClCx) == 0 {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JE => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::ZERO) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JL => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::SIGN)
                    != cpu.state.flags.contains(Flags::OVERFLOW)
                {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNL => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::SIGN)
                    == cpu.state.flags.contains(Flags::OVERFLOW)
                {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JLE => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::ZERO)
                    || cpu.state.flags.contains(Flags::SIGN)
                        != cpu.state.flags.contains(Flags::OVERFLOW)
                {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JMP => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                cpu.displace_ip(displacement)?;
            }
            OperandSet::SegmentAndOffset(mrc_instruction::Address { segment, offset }) => {
                cpu.state.set_segment_value(Segment::CS, *segment);
                cpu.state.ip = *offset;
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNB => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::CARRY) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNBE => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::CARRY) && !cpu.state.flags.contains(Flags::ZERO)
                {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNE => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::ZERO) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNO => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::OVERFLOW) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNP => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::PARITY) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNS => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if !cpu.state.flags.contains(Flags::SIGN) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JO => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::OVERFLOW) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JP => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::PARITY) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JS => match &instruction.operands {
            OperandSet::Displacement(displacement) => {
                if cpu.state.flags.contains(Flags::SIGN) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::LAHF => cpu.state.load_ah_from_flags(),

        Operation::LEA => match instruction.operands {
            OperandSet::DestinationAndSource(
                Operand::Register(SizedRegister(register, OperandSize::Word)),
                Operand::Indirect(
                    segment,
                    ref addressing_mode,
                    ref displacement,
                    OperandSize::Word,
                ),
            ) => {
                let addr = indirect_address_for(cpu, segment, addressing_mode, displacement);
                cpu.state.set_word_register_value(register, addr as u16);
            }
            _ => illegal_operands(instruction),
        },

        Operation::LOOP => match instruction.operands {
            OperandSet::Displacement(ref displacement) => {
                let cx = cpu.state.get_word_register_value(Register::ClCx);
                let cx = cx.wrapping_sub(1);
                cpu.state.set_word_register_value(Register::ClCx, cx);

                if cx != 0 {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::MOV => match &instruction.operands {
            OperandSet::DestinationAndSource(destination, source)
                if destination.operand_size() == source.operand_size() =>
            {
                match destination.operand_size() {
                    OperandSize::Byte => {
                        let source_value = byte::get_operand_type_value(cpu, source)?;
                        byte::set_operand_type_value(cpu, destination, source_value)?;
                    }
                    OperandSize::Word => {
                        let source_value = word::get_operand_type_value(cpu, source)?;
                        word::set_operand_type_value(cpu, destination, source_value)?;
                    }
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::LODSB | Operation::MOVSB | Operation::STOSB | Operation::SCASB => {
            loop {
                match instruction.operation {
                    Operation::LODSB => operations::byte::load(&cpu.bus, &mut cpu.state)?,
                    Operation::MOVSB => operations::byte::mov(&mut cpu.bus, &mut cpu.state)?,
                    Operation::STOSB => operations::byte::store(&mut cpu.bus, &mut cpu.state)?,
                    Operation::SCASB => operations::byte::scan(&mut cpu.bus, &mut cpu.state)?,
                    _ => unreachable!(),
                }

                // If there is no repeat set for this instruction, then we just stop here, not
                // modifying anything else.
                if instruction.repeat.is_none() {
                    break;
                }

                // Adjust the count.
                cpu.state.set_word_register_value(
                    Register::ClCx,
                    cpu.state
                        .get_word_register_value(Register::ClCx)
                        .wrapping_sub(1),
                );

                let cx = cpu.state.get_word_register_value(Register::ClCx);

                match instruction.repeat.unwrap() {
                    Repeat::Equal => {
                        if cx == 0 {
                            break;
                        }
                    }

                    Repeat::NotEqual => {
                        if cx != 0 {
                            break;
                        }
                    }
                }
            }
        }

        Operation::LODSW | Operation::MOVSW | Operation::STOSW | Operation::SCASW => {
            let value_size: u16 = match instruction.operation {
                Operation::LODSB | Operation::MOVSB | Operation::STOSB | Operation::SCASB => 1,
                Operation::LODSW | Operation::MOVSW | Operation::STOSW | Operation::SCASW => 2,
                _ => unreachable!(),
            };

            let ds = cpu.state.get_segment_value(Segment::DS);
            let mut si = cpu.state.get_word_register_value(Register::DhSi);

            let es = cpu.state.get_segment_value(Segment::ES);
            let mut di = cpu.state.get_word_register_value(Register::BhDi);

            let mut count = match instruction.repeat {
                None => 1,
                Some(repeat) => match repeat {
                    Repeat::Equal => cpu.state.get_word_register_value(Register::ClCx),
                    Repeat::NotEqual => cpu.state.get_word_register_value(Register::ClCx),
                },
            };

            let reverse = cpu.state.flags.contains(Flags::DIRECTION);

            loop {
                match instruction.operation {
                    Operation::LODSB => {
                        let value = byte::bus_read(&cpu.bus, segment_and_offset(ds, si))?;
                        cpu.state.set_byte_register_value(Register::AlAx, value);
                    }

                    Operation::LODSW => {
                        let value = word::bus_read(&cpu.bus, segment_and_offset(ds, si))?;
                        cpu.state.set_word_register_value(Register::AlAx, value);
                    }

                    Operation::MOVSB => {
                        let value = byte::bus_read(&cpu.bus, segment_and_offset(ds, si))?;
                        byte::bus_write(&mut cpu.bus, segment_and_offset(es, di), value)?;
                    }

                    Operation::MOVSW => {
                        let value = word::bus_read(&cpu.bus, segment_and_offset(ds, si))?;
                        word::bus_write(&mut cpu.bus, segment_and_offset(es, di), value)?;
                    }

                    Operation::STOSB => {
                        let value = cpu.state.get_byte_register_value(Register::AlAx);
                        byte::bus_write(&mut cpu.bus, segment_and_offset(es, di), value)?;
                    }

                    Operation::STOSW => {
                        let value = cpu.state.get_word_register_value(Register::AlAx);
                        word::bus_write(&mut cpu.bus, segment_and_offset(es, di), value)?;
                    }

                    Operation::SCASB => {
                        let destination = byte::bus_read(&cpu.bus, segment_and_offset(ds, si))?;
                        let source = byte::bus_read(&cpu.bus, segment_and_offset(es, di))?;
                        let _ =
                            operations::byte::compare(destination, source, &mut cpu.state.flags);
                    }

                    Operation::SCASW => {
                        let destination = word::bus_read(&cpu.bus, segment_and_offset(ds, si))?;
                        let source = word::bus_read(&cpu.bus, segment_and_offset(es, di))?;
                        let _ =
                            operations::word::compare(destination, source, &mut cpu.state.flags);
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

        Operation::NOP => {}

        Operation::NOT => match instruction.operands {
            OperandSet::Destination(ref destination) => match destination.operand_size() {
                OperandSize::Byte => {
                    let value = byte::get_operand_type_value(cpu, destination)?;
                    if let Some(result) = operations::byte::not(value, &mut cpu.state.flags) {
                        byte::set_operand_type_value(cpu, destination, result)?;
                    }
                }
                OperandSize::Word => {
                    let value = word::get_operand_type_value(cpu, destination)?;
                    if let Some(result) = operations::word::not(value, &mut cpu.state.flags) {
                        word::set_operand_type_value(cpu, destination, result)?;
                    }
                }
            },
            _ => illegal_operands(instruction),
        },

        Operation::OUT => match instruction.operands {
            OperandSet::DestinationAndSource(ref port, ref value) => {
                let port = match port.operand_size() {
                    OperandSize::Byte => byte::get_operand_value(cpu, port)? as u16,
                    OperandSize::Word => word::get_operand_value(cpu, port)?,
                };

                let value = byte::get_operand_type_value(cpu, value)?;
                cpu.io_controller.write(port, value)?;

                if port == 0x60 || port == 0x80 {
                    log::info!("POST port out ({:04X}): {:02X}", port, value);
                }
            }

            _ => illegal_operands(instruction),
        },

        Operation::PUSH => match &instruction.operands {
            OperandSet::Destination(destination) => {
                let value = word::get_operand_value(cpu, destination)?;
                cpu.push(value)?;
            }
            _ => illegal_operands(instruction),
        },

        Operation::PUSHF => {
            cpu.push(cpu.state.flags.bits)?;
        }

        Operation::POP => match &instruction.operands {
            OperandSet::Destination(destination) => {
                let value = cpu.pop()?;
                word::set_operand_value(cpu, destination, value)?;
            }
            _ => illegal_operands(instruction),
        },

        Operation::POPF => {
            cpu.state.flags.bits = cpu.pop()?;
        }

        Operation::RET => {
            // Pop the return address from the stack.
            cpu.state.ip = cpu.pop()?;
        }

        // Shift left/right has a special case where a word value can be shifted by cl, which is a byte.
        Operation::SHL | Operation::SHR => match instruction.operands {
            OperandSet::DestinationAndSource(
                ref destination,
                Operand::Register(SizedRegister(Register::ClCx, OperandSize::Byte)),
            ) if destination.operand_size() == OperandSize::Word => {
                let d = word::get_operand_type_value(cpu, destination)?;
                let s = cpu.state.get_byte_register_value(Register::ClCx) as u16;
                let result = match instruction.operation {
                    Operation::SHL => {
                        operations::word::shift_left(d, s, &mut cpu.state.flags).unwrap()
                    }
                    Operation::SHR => {
                        operations::word::shift_right(d, s, &mut cpu.state.flags).unwrap()
                    }
                    _ => unreachable!(),
                };
                word::set_operand_type_value(cpu, destination, result)?;
            }

            _ => illegal_operands(instruction),
        },

        Operation::SAHF => cpu.state.store_ah_into_flags(),

        Operation::STC => {
            cpu.state.flags.insert(Flags::CARRY);
        }

        Operation::STD => {
            cpu.state.flags.insert(Flags::DIRECTION);
        }

        Operation::STI => {
            cpu.state.flags.insert(Flags::INTERRUPT);
        }

        Operation::XCHG => match instruction.operands {
            OperandSet::DestinationAndSource(ref destination, ref source)
                if destination.operand_size() == source.operand_size() =>
            {
                match destination.operand_size() {
                    OperandSize::Byte => {
                        let d = byte::get_operand_type_value(cpu, destination)?;
                        let s = byte::get_operand_type_value(cpu, source)?;

                        byte::set_operand_type_value(cpu, destination, s)?;
                        byte::set_operand_type_value(cpu, source, d)?;
                    }
                    OperandSize::Word => {
                        let d = word::get_operand_type_value(cpu, destination)?;
                        let s = word::get_operand_type_value(cpu, source)?;

                        word::set_operand_type_value(cpu, destination, s)?;
                        word::set_operand_type_value(cpu, source, d)?;
                    }
                }
            }

            _ => illegal_operands(instruction),
        },

        _ => {
            // return Err(Error::IllegalInstruction);
            todo!("Instruction not implemented: {:?}", instruction)
        }
    }

    Ok(ExecuteResult::Continue)
}
