pub mod operations;

use crate::{
    cpu::{
        executor::operations::{SignificantBit, StateExt},
        state::{ByteRegister::AL, WordRegister::*},
        ByteRegister::AH,
        Flags, CPU,
    },
    error::{Error, Result},
    segment_and_offset, Address, Bus,
};
use mrc_instruction::{
    AddressingMode, Displacement, Immediate, Instruction, Operand, OperandSet, OperandSize,
    Operation, RegisterEncoding, Repeat, Segment, Segment::*, SizedRegisterEncoding,
};
use tracing::info;

#[derive(PartialEq)]
pub enum ExecuteResult {
    Continue,
    Stop,
}

impl<D: Bus, I: Bus> CPU<D, I> {
    fn push(&mut self, value: u16) -> Result<()> {
        self.state
            .set_register(SP, self.state.register(SP).wrapping_add(2));

        word::bus_write(
            &mut self.bus,
            segment_and_offset(self.state.segment(SS), self.state.register(SP)),
            value,
        );

        // info!("Push {:04X} to [{:04X}:{:04X}]", value, ss, sp,);

        Ok(())
    }

    fn pop(&mut self) -> Result<u16> {
        let value = word::bus_read(
            &self.bus,
            segment_and_offset(self.state.segment(SS), self.state.register(SP)),
        );

        // info!("Pop {:04X} from [{:04X}:{:04X}]", value, ss, sp,);

        self.state
            .set_register(SP, self.state.register(SP).wrapping_add(2));

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

fn indirect_address_for<D: Bus, I: Bus>(
    cpu: &CPU<D, I>,
    segment: Segment,
    addressing_mode: &AddressingMode,
    displacement: &Displacement,
) -> Address {
    let seg = cpu.state.segment(segment);

    let mut addr = match addressing_mode {
        AddressingMode::BxSi => {
            let bx = cpu.state.register(BX);
            let si = cpu.state.register(SI);
            segment_and_offset(seg, bx.wrapping_add(si))
        }
        AddressingMode::BxDi => {
            let bx = cpu.state.register(BX);
            let di = cpu.state.register(DI);
            segment_and_offset(seg, bx.wrapping_add(di))
        }
        AddressingMode::BpSi => {
            let bp = cpu.state.register(BP);
            let si = cpu.state.register(SI);
            segment_and_offset(seg, bp.wrapping_add(si))
        }
        AddressingMode::BpDi => {
            let bp = cpu.state.register(BP);
            let di = cpu.state.register(DI);
            segment_and_offset(seg, bp.wrapping_add(di))
        }
        AddressingMode::Si => {
            let si = cpu.state.register(DI);
            segment_and_offset(seg, si)
        }
        AddressingMode::Di => {
            let di = cpu.state.register(DI);
            segment_and_offset(seg, di)
        }
        AddressingMode::Bp => {
            let bp = cpu.state.register(BP);
            segment_and_offset(seg, bp)
        }
        AddressingMode::Bx => {
            let bx = cpu.state.register(BX);
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
    use mrc_instruction::{Immediate, SizedRegisterEncoding};
    use tracing::warn;

    pub fn bus_read(bus: &impl Bus, address: Address) -> u8 {
        bus.read(address)
    }

    pub fn bus_write(bus: &mut impl Bus, address: Address, value: u8) {
        bus.write(address, value);
    }

    pub fn get_operand_type_value<D: Bus, I: Bus>(cpu: &CPU<D, I>, operand_type: &Operand) -> u8 {
        match operand_type {
            Operand::Direct(segment, offset, OperandSize::Byte) => {
                // TODO: Handle segment override.
                let segment_value = cpu.state.segment(*segment);
                let address = segment_and_offset(segment_value, *offset);
                byte::bus_read(&cpu.bus, address)
            }

            Operand::Indirect(segment, addressing_mode, displacement, OperandSize::Byte) => {
                let address = indirect_address_for(cpu, *segment, addressing_mode, displacement);
                byte::bus_read(&cpu.bus, address)
            }

            Operand::Register(SizedRegisterEncoding(register, OperandSize::Byte)) => {
                cpu.state.register(*register)
            }

            Operand::Immediate(Immediate::Byte(value)) => *value as u8,

            _ => {
                warn!("Invalid data access");
                0
            }
        }
    }

    pub fn get_operand_value<D: Bus, I: Bus>(cpu: &CPU<D, I>, operand: &Operand) -> u8 {
        assert_eq!(OperandSize::Byte, operand.operand_size());
        byte::get_operand_type_value(cpu, operand)
    }

    pub fn set_operand_type_value<D: Bus, I: Bus>(
        cpu: &mut CPU<D, I>,
        operand_type: &Operand,
        value: u8,
    ) {
        match operand_type {
            Operand::Direct(segment, offset, OperandSize::Byte) => {
                // TODO: Handle segment override.
                let seg = cpu.state.segment(*segment);
                byte::bus_write(&mut cpu.bus, segment_and_offset(seg, *offset), value)
            }

            Operand::Indirect(segment, addressing_mode, displacement, OperandSize::Byte) => {
                let addr = indirect_address_for(cpu, *segment, addressing_mode, displacement);
                byte::bus_write(&mut cpu.bus, addr, value)
            }

            Operand::Register(SizedRegisterEncoding(register, OperandSize::Byte)) => {
                cpu.state.set_register(*register, value);
            }

            _ => warn!("Invalid data access"),
        }
    }

    pub fn set_operand_value<D: Bus, I: Bus>(cpu: &mut CPU<D, I>, operand: &Operand, value: u8) {
        assert_eq!(OperandSize::Byte, operand.operand_size());
        byte::set_operand_type_value(cpu, operand, value)
    }
}

mod word {
    use super::*;
    use mrc_instruction::{Immediate, SizedRegisterEncoding};
    use tracing::warn;

    pub fn bus_read(bus: &impl Bus, address: Address) -> u16 {
        let lo = byte::bus_read(bus, address);
        let hi = byte::bus_read(bus, address + 1);
        u16::from_le_bytes([lo, hi])
    }

    pub fn bus_write(bus: &mut impl Bus, address: Address, value: u16) {
        let bytes = value.to_le_bytes();
        byte::bus_write(bus, address, bytes[0]);
        byte::bus_write(bus, address + 1, bytes[1]);
    }

    pub fn set_operand_type_value<D: Bus, I: Bus>(
        cpu: &mut CPU<D, I>,
        operand_type: &Operand,
        value: u16,
    ) {
        match operand_type {
            Operand::Direct(segment, offset, OperandSize::Word) => {
                // TODO: Handle segment override.
                let seg = cpu.state.segment(*segment);
                word::bus_write(&mut cpu.bus, segment_and_offset(seg, *offset), value);
            }

            Operand::Indirect(segment, addressing_mode, displacement, OperandSize::Word) => {
                let addr = indirect_address_for(cpu, *segment, addressing_mode, displacement);
                word::bus_write(&mut cpu.bus, addr, value);
            }

            Operand::Register(SizedRegisterEncoding(register, OperandSize::Word)) => {
                cpu.state.set_register(*register, value);
            }

            Operand::Segment(segment) => {
                cpu.state.set_segment(*segment, value);
            }

            _ => warn!("Invalid data access"),
        }
    }

    pub fn set_operand_value<D: Bus, I: Bus>(cpu: &mut CPU<D, I>, operand: &Operand, value: u16) {
        assert_eq!(OperandSize::Word, operand.operand_size());
        set_operand_type_value(cpu, operand, value);
    }

    pub fn get_operand_type_value<D: Bus, I: Bus>(cpu: &CPU<D, I>, operand_type: &Operand) -> u16 {
        match operand_type {
            Operand::Direct(segment, offset, OperandSize::Word) => {
                // Get a single byte from DS:offset
                let seg = cpu.state.segment(*segment);
                let addr = segment_and_offset(seg, *offset);

                word::bus_read(&cpu.bus, addr)
            }

            Operand::Indirect(segment, addressing_mode, displacement, OperandSize::Word) => {
                word::bus_read(
                    &cpu.bus,
                    indirect_address_for(cpu, *segment, addressing_mode, displacement),
                )
            }

            Operand::Register(SizedRegisterEncoding(register, OperandSize::Word)) => {
                cpu.state.register(*register)
            }

            Operand::Segment(segment) => cpu.state.segment(*segment),

            Operand::Immediate(Immediate::Word(value)) => *value,

            _ => {
                warn!("Invalid data access");
                0
            }
        }
    }

    pub fn get_operand_value<D: Bus, I: Bus>(cpu: &CPU<D, I>, operand: &Operand) -> u16 {
        assert_eq!(OperandSize::Word, operand.operand_size());

        word::get_operand_type_value(cpu, operand)
    }
}

pub fn execute<D: Bus, I: Bus>(
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
                    let d = byte::get_operand_type_value(cpu, destination);
                    let s = byte::get_operand_type_value(cpu, source);
                    if let Some(result) =
                        destination_and_source_ops!(instruction.operation, d, s, byte)
                    {
                        if let Some(result) = result {
                            byte::set_operand_type_value(cpu, destination, result);
                        }
                        return Ok(ExecuteResult::Continue);
                    }
                }
                OperandSize::Word => {
                    let d = word::get_operand_type_value(cpu, destination);
                    let s = word::get_operand_type_value(cpu, source);
                    if let Some(result) =
                        destination_and_source_ops!(instruction.operation, d, s, word)
                    {
                        if let Some(result) = result {
                            word::set_operand_type_value(cpu, destination, result);
                        }
                        return Ok(ExecuteResult::Continue);
                    }
                }
            }
        }
    }

    match instruction.operation {
        Operation::CALL => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                // Store the current IP (which is after this CALL) on the stack. So that RET
                // can pop it.
                cpu.push(cpu.state.ip)?;
                cpu.displace_ip(displacement)?;
            }
            _ => todo!(),
        },

        Operation::CBW => {
            let al = cpu.state.register(AL);
            if al.most_significant_bit() {
                cpu.state.set_register(AH, 0b11111111);
            } else {
                cpu.state.set_register(AH, 0b00000000);
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
                        let mut value = byte::get_operand_type_value(cpu, destination);
                        value = operations::byte::decrement(value, &mut cpu.state.flags).unwrap();
                        byte::set_operand_type_value(cpu, destination, value)
                    }
                    OperandSize::Word => {
                        let mut value = word::get_operand_type_value(cpu, destination);
                        value = operations::word::decrement(value, &mut cpu.state.flags).unwrap();
                        word::set_operand_type_value(cpu, destination, value)
                    }
                };
            }
            _ => illegal_operands(instruction),
        },

        Operation::ESC => {
            // TODO: Implement FPU
        }

        Operation::HLT => {
            info!("HALT");
            return Ok(ExecuteResult::Stop);
        }

        Operation::IN => match instruction.operands {
            OperandSet::DestinationAndSource(ref destination, ref port) => {
                let address = match port.operand_size() {
                    OperandSize::Byte => byte::get_operand_value(cpu, port) as u16,
                    OperandSize::Word => word::get_operand_value(cpu, port),
                };

                match destination.operand_size() {
                    OperandSize::Byte => {
                        let value = cpu.io_controller.read(address as Address);
                        byte::set_operand_value(cpu, destination, value);
                    }
                    OperandSize::Word => {
                        let value = cpu.io_controller.read(address as Address) as u16;
                        word::set_operand_value(cpu, destination, value);
                    }
                };
            }

            _ => illegal_operands(instruction),
        },

        Operation::INC => match &instruction.operands {
            OperandSet::Destination(destination) => match destination.operand_size() {
                OperandSize::Byte => {
                    let mut value = byte::get_operand_value(cpu, destination);
                    value = operations::byte::increment(value, &mut cpu.state.flags).unwrap();
                    byte::set_operand_value(cpu, destination, value);
                }
                OperandSize::Word => {
                    let mut value = word::get_operand_value(cpu, destination);
                    value = operations::word::increment(value, &mut cpu.state.flags).unwrap();
                    word::set_operand_value(cpu, destination, value)
                }
            },
            _ => illegal_operands(instruction),
        },

        Operation::INT => match instruction.operands {
            OperandSet::Destination(Operand::Immediate(Immediate::Byte(index))) => {
                info!("Calling interrupt {}", index);

                cpu.push(cpu.state.flags.bits())?;
                cpu.push(cpu.state.segment(CS))?;
                cpu.push(cpu.state.ip)?;

                cpu.state.flags.set(Flags::INTERRUPT, false);
                cpu.state.flags.set(Flags::TRAP, false);

                // The very top of memory.
                let interrupt_descriptor_table = segment_and_offset(0x0000, 0x0000);
                let addr = interrupt_descriptor_table + 4u32 * index as Address;

                let ip = word::bus_read(&cpu.bus, addr);
                let cs = word::bus_read(&cpu.bus, addr + 2);

                // If the far call points to 0000:0000 again, this is probably an invalid interrupt
                // vector.
                if ip == 0 && cs == 0 {
                    // TODO: Probably not the best error to return here.
                    return Err(Error::IllegalInstruction);
                }

                cpu.state.set_segment(CS, cs);
                cpu.state.ip = ip;
            }

            _ => illegal_operands(instruction),
        },

        Operation::IRET => match instruction.operands {
            OperandSet::None => {
                cpu.state.ip = cpu.pop()?;
                let cs = cpu.pop()?;
                cpu.state.set_segment(CS, cs);
                cpu.state.flags = Flags::from_bits(cpu.pop()?).unwrap();
            }

            _ => illegal_operands(instruction),
        },

        Operation::JB => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if cpu.state.flags.contains(Flags::CARRY) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JBE => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if cpu.state.flags.contains(Flags::CARRY | Flags::ZERO) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JCXZ => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if cpu.state.register(CX) == 0 {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JE => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if cpu.state.flags.contains(Flags::ZERO) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JL => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if cpu.state.flags.contains(Flags::SIGN)
                    != cpu.state.flags.contains(Flags::OVERFLOW)
                {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNL => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if cpu.state.flags.contains(Flags::SIGN)
                    == cpu.state.flags.contains(Flags::OVERFLOW)
                {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JLE => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
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
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                cpu.displace_ip(displacement)?;
            }
            OperandSet::Destination(Operand::SegmentAndOffset(mrc_instruction::Address {
                segment,
                offset,
            })) => {
                cpu.state.set_segment(CS, *segment);
                cpu.state.ip = *offset;
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNB => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if !cpu.state.flags.contains(Flags::CARRY) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNBE => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if !cpu.state.flags.contains(Flags::CARRY) && !cpu.state.flags.contains(Flags::ZERO)
                {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNE => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if !cpu.state.flags.contains(Flags::ZERO) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNO => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if !cpu.state.flags.contains(Flags::OVERFLOW) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNP => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if !cpu.state.flags.contains(Flags::PARITY) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JNS => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if !cpu.state.flags.contains(Flags::SIGN) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JO => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if cpu.state.flags.contains(Flags::OVERFLOW) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JP => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if cpu.state.flags.contains(Flags::PARITY) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::JS => match &instruction.operands {
            OperandSet::Destination(Operand::Displacement(displacement)) => {
                if cpu.state.flags.contains(Flags::SIGN) {
                    cpu.displace_ip(displacement)?;
                }
            }
            _ => illegal_operands(instruction),
        },

        Operation::LAHF => cpu.state.load_ah_from_flags(),

        Operation::LEA => match instruction.operands {
            OperandSet::DestinationAndSource(
                Operand::Register(SizedRegisterEncoding(register, OperandSize::Word)),
                Operand::Indirect(
                    segment,
                    ref addressing_mode,
                    ref displacement,
                    OperandSize::Word,
                ),
            ) => {
                let addr = indirect_address_for(cpu, segment, addressing_mode, displacement);
                cpu.state.set_register(register, addr as u16);
            }
            _ => illegal_operands(instruction),
        },

        Operation::LOOP => match instruction.operands {
            OperandSet::Destination(Operand::Displacement(ref displacement)) => {
                let cx = cpu.state.register(CX).wrapping_sub(1);

                cpu.state.set_register(CX, cx);

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
                        let source_value = byte::get_operand_type_value(cpu, source);
                        byte::set_operand_type_value(cpu, destination, source_value);
                    }
                    OperandSize::Word => {
                        let source_value = word::get_operand_type_value(cpu, source);
                        word::set_operand_type_value(cpu, destination, source_value);
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
                cpu.state
                    .set_register(CX, cpu.state.register(CX).wrapping_sub(1));

                let cx = cpu.state.register(CX);

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

            let ds = cpu.state.segment(DS);
            let mut si = cpu.state.register(SI);

            let es = cpu.state.segment(ES);
            let mut di = cpu.state.register(DI);

            let mut count = match instruction.repeat {
                None => 1,
                Some(repeat) => match repeat {
                    Repeat::Equal => cpu.state.register(CX),
                    Repeat::NotEqual => cpu.state.register(CX),
                },
            };

            let reverse = cpu.state.flags.contains(Flags::DIRECTION);

            loop {
                match instruction.operation {
                    Operation::LODSB => {
                        let value = byte::bus_read(&cpu.bus, segment_and_offset(ds, si));
                        cpu.state.set_register(AL, value);
                    }

                    Operation::LODSW => {
                        let value = word::bus_read(&cpu.bus, segment_and_offset(ds, si));
                        cpu.state.set_register(AX, value);
                    }

                    Operation::MOVSB => {
                        let value = byte::bus_read(&cpu.bus, segment_and_offset(ds, si));
                        byte::bus_write(&mut cpu.bus, segment_and_offset(es, di), value);
                    }

                    Operation::MOVSW => {
                        let value = word::bus_read(&cpu.bus, segment_and_offset(ds, si));
                        word::bus_write(&mut cpu.bus, segment_and_offset(es, di), value);
                    }

                    Operation::STOSB => {
                        let value = cpu.state.register(AL);
                        byte::bus_write(&mut cpu.bus, segment_and_offset(es, di), value);
                    }

                    Operation::STOSW => {
                        let value = cpu.state.register(AX);
                        word::bus_write(&mut cpu.bus, segment_and_offset(es, di), value);
                    }

                    Operation::SCASB => {
                        let destination = byte::bus_read(&cpu.bus, segment_and_offset(ds, si));
                        let source = byte::bus_read(&cpu.bus, segment_and_offset(es, di));
                        let _ =
                            operations::byte::compare(destination, source, &mut cpu.state.flags);
                    }

                    Operation::SCASW => {
                        let destination = word::bus_read(&cpu.bus, segment_and_offset(ds, si));
                        let source = word::bus_read(&cpu.bus, segment_and_offset(es, di));
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

            cpu.state.set_register(SI, si);
            cpu.state.set_register(DI, di);

            if instruction.repeat.is_some() {
                cpu.state.set_register(CX, count);
            }
        }

        Operation::NOP => {}

        Operation::NOT => match instruction.operands {
            OperandSet::Destination(ref destination) => match destination.operand_size() {
                OperandSize::Byte => {
                    let value = byte::get_operand_type_value(cpu, destination);
                    if let Some(result) = operations::byte::not(value, &mut cpu.state.flags) {
                        byte::set_operand_type_value(cpu, destination, result);
                    }
                }
                OperandSize::Word => {
                    let value = word::get_operand_type_value(cpu, destination);
                    if let Some(result) = operations::word::not(value, &mut cpu.state.flags) {
                        word::set_operand_type_value(cpu, destination, result);
                    }
                }
            },
            _ => illegal_operands(instruction),
        },

        Operation::OUT => match instruction.operands {
            OperandSet::DestinationAndSource(ref port, ref value) => {
                let port = match port.operand_size() {
                    OperandSize::Byte => byte::get_operand_value(cpu, port) as u16,
                    OperandSize::Word => word::get_operand_value(cpu, port),
                };

                let value = byte::get_operand_type_value(cpu, value);
                cpu.io_controller.write(port as Address, value);

                if port == 0x60 || port == 0x80 {
                    info!("POST port out ({:04X}): {:02X}", port, value);
                }
            }

            _ => illegal_operands(instruction),
        },

        Operation::PUSH => match &instruction.operands {
            OperandSet::Destination(destination) => {
                let value = word::get_operand_value(cpu, destination);
                cpu.push(value)?;
            }
            _ => illegal_operands(instruction),
        },

        Operation::PUSHF => {
            cpu.push(cpu.state.flags.bits())?;
        }

        Operation::POP => match &instruction.operands {
            OperandSet::Destination(destination) => {
                let value = cpu.pop()?;
                word::set_operand_value(cpu, destination, value);
            }
            _ => illegal_operands(instruction),
        },

        Operation::POPF => {
            cpu.state.flags = Flags::from_bits_truncate(cpu.pop()?);
        }

        Operation::RET => {
            // Pop the return address from the stack.
            cpu.state.ip = cpu.pop()?;
        }

        // Shift left/right has a special case where a word value can be shifted by cl, which is a byte.
        Operation::SHL | Operation::SHR => match instruction.operands {
            OperandSet::DestinationAndSource(
                ref destination,
                Operand::Register(SizedRegisterEncoding(RegisterEncoding::ClCx, OperandSize::Byte)),
            ) if destination.operand_size() == OperandSize::Word => {
                let d = word::get_operand_type_value(cpu, destination);
                let s = cpu.state.register(CX);
                let result = match instruction.operation {
                    Operation::SHL => {
                        operations::word::shift_left(d, s, &mut cpu.state.flags).unwrap()
                    }
                    Operation::SHR => {
                        operations::word::shift_right(d, s, &mut cpu.state.flags).unwrap()
                    }
                    _ => unreachable!(),
                };
                word::set_operand_type_value(cpu, destination, result);
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
                        let d = byte::get_operand_type_value(cpu, destination);
                        let s = byte::get_operand_type_value(cpu, source);

                        byte::set_operand_type_value(cpu, destination, s);
                        byte::set_operand_type_value(cpu, source, d);
                    }
                    OperandSize::Word => {
                        let d = word::get_operand_type_value(cpu, destination);
                        let s = word::get_operand_type_value(cpu, source);

                        word::set_operand_type_value(cpu, destination, s);
                        word::set_operand_type_value(cpu, source, d);
                    }
                }
            }

            _ => illegal_operands(instruction),
        },

        Operation::LOCK => {}

        _ => {
            // return Err(Error::IllegalInstruction);
            todo!("Instruction not implemented: {:?}", instruction)
        }
    }

    Ok(ExecuteResult::Continue)
}
