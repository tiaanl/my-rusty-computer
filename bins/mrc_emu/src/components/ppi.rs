//! Emulation of the Intel 8255 Programmable Peripheral Interface

use mrc_emulator::error::Error;
use mrc_emulator::{Bus, Port};

#[derive(Clone, Copy, Debug)]
pub enum Mode {
    Input,
    Output,
}

#[derive(Debug)]
pub struct ProgrammablePeripheralInterface<A, B, C>
where
    A: Bus<Port>,
    B: Bus<Port>,
    C: Bus<Port>,
{
    port_a: A,
    port_b: B,
    port_c: C,

    modes: [Mode; 3],
}

impl<A, B, C> ProgrammablePeripheralInterface<A, B, C>
where
    A: Bus<Port>,
    B: Bus<Port>,
    C: Bus<Port>,
{
    pub fn new(port_a: A, port_b: B, port_c: C) -> Self {
        Self {
            port_a,
            port_b,
            port_c,
            modes: [Mode::Input; 3],
        }
    }
}

impl<A, B, C> Bus<Port> for ProgrammablePeripheralInterface<A, B, C>
where
    A: Bus<Port>,
    B: Bus<Port>,
    C: Bus<Port>,
{
    fn read(&self, address: Port) -> mrc_emulator::error::Result<u8> {
        let address = address & 0b11;

        match address {
            0b00 => self.port_a.read(address),
            0b01 => self.port_b.read(address),
            0b10 => self.port_c.read(address),
            0b11 => Err(Error::InvalidPort(address)),
            _ => unreachable!(),
        }
    }

    fn write(&mut self, address: Port, value: u8) -> mrc_emulator::error::Result<()> {
        let address = address & 0b11;

        match address {
            0b00 => self.port_a.write(address, value),
            0b01 => self.port_b.write(address, value),
            0b10 => self.port_c.write(address, value),
            0b11 => {
                for i in 0..3_usize {
                    self.modes[i] = if value & (1 << i) != 0 {
                        Mode::Input
                    } else {
                        Mode::Output
                    }
                }
                log::info!("Writing to PPI control register: {:?}", self.modes);
                Ok(())
            }
            _ => Err(Error::InvalidPort(address)),
        }
    }
}

pub struct Latch(pub u8);

impl Bus<Port> for Latch {
    fn read(&self, _address: Port) -> mrc_emulator::error::Result<u8> {
        Ok(self.0)
    }

    fn write(&mut self, _address: Port, value: u8) -> mrc_emulator::error::Result<()> {
        self.0 = value;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set_output_values() {
        let mut ppi = ProgrammablePeripheralInterface::default();

        // Set all ports to output mode.
        ppi.write(0x03, 0x80).unwrap();

        // Write values to all ports.
        ppi.write(0x00, 0x0A).unwrap();
        ppi.write(0x01, 0x0B).unwrap();
        ppi.write(0x02, 0x0C).unwrap();

        // Check if the values are persisted.
        assert_eq!(ppi.read(0x00).unwrap(), 0x0A);
        assert_eq!(ppi.read(0x01).unwrap(), 0x0B);
        assert_eq!(ppi.read(0x02).unwrap(), 0x0C);
    }
}
