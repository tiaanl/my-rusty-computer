use crate::error::Result;
use crate::io::IOInterface;

// Intel 8253 Programmable Interrupt Timer
// https://en.wikipedia.org/wiki/Intel_8253

// 0x40  Channel 0 data port (read/write)
// 0x41  Channel 1 data port (read/write)
// 0x42  Channel 2 data port (read/write)
// 0x43  Mode/Command register (write only, a read is ignored)

#[allow(unused)]
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
enum AccessMode {
    LatchCountValue = 0b00,
    LoByte = 0b01,
    HiByte = 0b10,
    LoByteHiByte = 0b11,
}

impl Default for AccessMode {
    fn default() -> Self {
        Self::LatchCountValue
    }
}

#[derive(Default)]
struct Channel {
    pub _access_mode: AccessMode,
}

#[derive(Default)]
pub struct ProgrammableIntervalTimer8253 {
    _channels: [Channel; 3],
}

impl IOInterface for ProgrammableIntervalTimer8253 {
    fn read(&self, port: u16) -> Result<u8> {
        let _channel = usize::from(port - 0x40);

        Ok(0xff)
    }

    fn write(&mut self, port: u16, value: u8) -> Result<()> {
        log::info!("Writing to timer port: {:04X} <= {:02X}", port, value);

        match port {
            0x40 | 0x41 | 0x42 => {}

            0x43 => {}

            _ => unreachable!(),
        }

        Ok(())
    }
}
