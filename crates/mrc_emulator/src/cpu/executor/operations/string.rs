use crate::cpu::{Flags, State, WordRegister};
use crate::error::Result;
use crate::{segment_and_offset, Address, Bus};
use mrc_instruction::Segment;

fn advance(state: &mut State, register: WordRegister, count: u16) {
    let r: u16 = state.register(register);
    state.set_register(
        register,
        if !state.flags.contains(Flags::DIRECTION) {
            r.wrapping_add(count)
        } else {
            r.wrapping_sub(count)
        },
    );
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
    use Segment::{DS, ES};
    use super::*;
    use crate::cpu::state::WordRegister::{DI, SI};
    use crate::{
        cpu::executor::{
            byte::{bus_read, bus_write},
            operations::byte::compare,
        },
        cpu::state::ByteRegister::AL,
    };

    pub fn load(bus: &impl Bus<Address>, state: &mut State) -> Result<()> {
        let value = bus_read(
            bus,
            segment_and_offset(state.segment(DS), state.register(SI)),
        )?;

        state.set_register(AL, value);

        advance(state, SI, 1);

        Ok(())
    }

    pub fn mov(bus: &mut impl Bus<Address>, state: &mut State) -> Result<()> {
        let value = bus_read(
            bus,
            segment_and_offset(state.segment(DS), state.register(SI)),
        )?;

        bus_write(
            bus,
            segment_and_offset(state.segment(ES), state.register(DI)),
            value,
        )?;

        advance(state, SI, 1);
        advance(state, DI, 1);

        Ok(())
    }

    pub fn store(bus: &mut impl Bus<Address>, state: &mut State) -> Result<()> {
        let value = state.register(AL);

        bus_write(
            bus,
            segment_and_offset(state.segment(ES), state.register(DI)),
            value,
        )?;

        advance(state, DI, 1);

        Ok(())
    }

    pub fn scan(bus: &mut impl Bus<Address>, state: &mut State) -> Result<()> {
        let destination = bus_read(
            bus,
            segment_and_offset(state.segment(DS), state.register(SI)),
        )?;

        let source = bus_read(
            bus,
            segment_and_offset(state.segment(ES), state.register(DI)),
        )?;

        let _ = compare(destination, source, &mut state.flags);

        advance(state, SI, 1);
        advance(state, DI, 1);

        Ok(())
    }
}
