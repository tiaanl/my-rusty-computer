use mrc_emulator::{error::Result, Bus, Port};

/// Based on Intel 8259A programmable interrupt controller.
#[derive(Default)]
pub struct Pic {}

impl Bus<Port> for Pic {
    fn read(&self, _port: u16) -> Result<u8> {
        todo!()
    }

    fn write(&mut self, _port: u16, _value: u8) -> Result<()> {
        todo!()
    }
}

#[cfg(test)]
mod test {}
