use crate::cpu::{Flags, State};
use crate::error::Result;
use crate::{segment_and_offset, Address, Bus};
use mrc_instruction::{Register, Segment};

fn advance(state: &mut State, bytes: u16, register: Register) {
    if !state.flags.contains(Flags::DIRECTION) {
        state.set_word_register_value(
            register,
            state.get_word_register_value(register).wrapping_add(bytes),
        );
    } else {
        state.set_word_register_value(
            register,
            state.get_word_register_value(register).wrapping_sub(bytes),
        );
    }
}

trait Sized<T> {
    fn read(&self, addr: Address) -> Result<T>;
}

impl<T: Bus<Address>> Sized<u8> for T {
    fn read(&self, addr: Address) -> Result<u8> {
        self.read(addr)
    }
}

impl<T: Bus<Address>> Sized<u16> for T {
    fn read(&self, addr: Address) -> Result<u16> {
        Ok(u16::from_le_bytes([self.read(addr)?, self.read(addr)?]))
    }
}

pub mod byte {
    use super::*;
    use crate::cpu::executor::{
        byte::{bus_read, bus_write},
        operations::byte::compare,
    };

    pub fn load(bus: &impl Bus<Address>, state: &mut State) -> Result<()> {
        let value = bus_read(
            bus,
            segment_and_offset(
                state.get_segment_value(Segment::DS),
                state.get_word_register_value(Register::DhSi),
            ),
        )?;

        state.set_byte_register_value(Register::AlAx, value);

        advance(state, 1, Register::DhSi);

        Ok(())
    }

    pub fn mov(bus: &mut impl Bus<Address>, state: &mut State) -> Result<()> {
        let value = bus_read(
            bus,
            segment_and_offset(
                state.get_segment_value(Segment::DS),
                state.get_word_register_value(Register::DhSi),
            ),
        )?;

        bus_write(
            bus,
            segment_and_offset(
                state.get_segment_value(Segment::ES),
                state.get_word_register_value(Register::BhDi),
            ),
            value,
        )?;

        advance(state, 1, Register::DhSi);
        advance(state, 1, Register::BhDi);

        Ok(())
    }

    pub fn store(bus: &mut impl Bus<Address>, state: &mut State) -> Result<()> {
        let value = state.get_byte_register_value(Register::AlAx);

        bus_write(
            bus,
            segment_and_offset(
                state.get_segment_value(Segment::ES),
                state.get_word_register_value(Register::BhDi),
            ),
            value,
        )?;

        advance(state, 1, Register::BhDi);

        Ok(())
    }

    pub fn scan(bus: &mut impl Bus<Address>, state: &mut State) -> Result<()> {
        let destination = bus_read(
            bus,
            segment_and_offset(
                state.get_segment_value(Segment::DS),
                state.get_word_register_value(Register::DhSi),
            ),
        )?;

        let source = bus_read(
            bus,
            segment_and_offset(
                state.get_segment_value(Segment::ES),
                state.get_word_register_value(Register::BhDi),
            ),
        )?;

        let _ = compare(destination, source, &mut state.flags);

        advance(state, 1, Register::DhSi);
        advance(state, 1, Register::BhDi);

        Ok(())
    }
}
