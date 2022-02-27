//! Emulation of the Intel 8255 Programmable Peripheral Interface

use mrc_emulator::error::Error;
use mrc_emulator::{Bus, Port};

#[derive(Debug)]
pub enum Mode {
    Input,
    Output,
}

#[derive(Debug)]
pub struct ChipPort {
    mode: Mode,
    value: u8,
}

impl Default for ChipPort {
    fn default() -> Self {
        Self {
            mode: Mode::Input,
            value: 0,
        }
    }
}

#[derive(Debug, Default)]
pub struct ProgrammablePeripheralInterface {
    ports: [ChipPort; 3],
}

impl Bus<Port> for ProgrammablePeripheralInterface {
    fn read(&self, address: Port) -> mrc_emulator::error::Result<u8> {
        let address = address & 0b11;

        if address < 3 {
            let value = self.ports[address as usize].value;
            log::info!(
                "Reading from PPI port {}: {:#04X}",
                ['A', 'B', 'C'][address as usize],
                value
            );
            Ok(value)
        } else {
            Err(Error::InvalidPort(address))
        }
    }

    fn write(&mut self, address: Port, value: u8) -> mrc_emulator::error::Result<()> {
        let address = address & 0b11;

        if address == 0b11 {
            for i in 0..3_usize {
                self.ports[i].mode = if value & (1 << i) != 0 {
                    Mode::Input
                } else {
                    Mode::Output
                }
            }
            log::info!("Writing to PPI control register: {:?}", self);
        } else {
            log::info!(
                "Writing to PPI port {}: {:#04X}",
                ['A', 'B', 'C'][address as usize],
                value
            );
            self.ports[address as usize].value;
        }

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
