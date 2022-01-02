use mrc_emulator::error::Error as EmulatorError;
use mrc_emulator::error::Result;
use mrc_emulator::{Bus, Port};
use std::cell::RefCell;

// Intel 8253 Programmable Interrupt Timer
// https://en.wikipedia.org/wiki/Intel_8253

// 0x40  Channel 0 data port (read/write)
// 0x41  Channel 1 data port (read/write)
// 0x42  Channel 2 data port (read/write)
// 0x43  Mode/Command register (write only, reads are ignored)

// Control Register Bits
//
// +-----+-----+-----+-----+----+----+----+-----+
// | SC1 | SC0 | RW1 | RW0 | M2 | M1 | M0 | BCD |
// +-----+-----+-----+-----+----+----+----+-----+
//
// SC - Select Counter
// | SC1 | SC0 |
// |   0 |   0 | Select counter 0
// |   0 |   1 | Select counter 1
// |   1 |   0 | Select counter 2
// |   1 |   1 | Not applicable on 8253
//
// RW - Read/write
// | RW1 | RW0 |
// |   0 |   0 | Counter latch
// |   0 |   1 | R/W least significant byte only
// |   1 |   0 | R/W most significant bits only
// |   1 |   1 | R/W least significant byte first and then most significant byte
//
// M - Mode
// | M2 | M1 | M0 |
// |  0 |  0 |  0 | Mode 0
// |  0 |  0 |  1 | Mode 1
// |  x |  1 |  0 | Mode 2
// |  x |  1 |  1 | Mode 3
// |  1 |  0 |  0 | Mode 4
// |  1 |  0 |  1 | Mode 5
//
// BCD - Binary/BCD
// | BCD |
// |   0 | Binary counter (16-bits)
// |   1 | BCD counter (4 d..)

// const TICK_RATE: usize = 1193182;

#[derive(Copy, Clone, Debug)]
enum ReadWrite {
    Latch,
    LoByte,
    HiByte,
    LoThenHiByte,
}

impl TryFrom<u8> for ReadWrite {
    type Error = EmulatorError;

    fn try_from(value: u8) -> std::result::Result<Self, Self::Error> {
        use ReadWrite::*;

        match value {
            0b00 => Ok(Latch),
            0b01 => Ok(LoByte),
            0b10 => Ok(HiByte),
            0b11 => Ok(LoThenHiByte),
            _ => Err(EmulatorError::IllegalDataAccess),
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct ControlRegister {
    /// Selected counter (0..3)
    counter: u8,
    // Read/Write mode
    read_write: ReadWrite,
    mode: u8,
    bcd: bool,
}

impl TryFrom<u8> for ControlRegister {
    type Error = EmulatorError;

    fn try_from(value: u8) -> std::result::Result<Self, Self::Error> {
        Ok(Self {
            counter: value >> 6,
            read_write: ReadWrite::try_from(value >> 4 & 0b11)?,
            mode: value >> 1 & 0b111,
            bcd: (value & 0b1) != 0,
        })
    }
}

#[derive(Debug)]
struct Counter {
    read_write: ReadWrite,
    mode: u8,
    bcd: bool,
    count: u16,
}

impl Default for Counter {
    fn default() -> Self {
        Self {
            read_write: ReadWrite::Latch,
            mode: 0,
            bcd: false,
            count: 0,
        }
    }
}

#[derive(Default)]
struct Inner {
    counters: [Counter; 3],
}

#[derive(Default)]
pub struct ProgrammableIntervalTimer8253 {
    inner: RefCell<Inner>,
}

impl ProgrammableIntervalTimer8253 {
    pub fn tick(&mut self) {
        self.inner.borrow_mut().counters.iter_mut().for_each(|c| {
            c.count = c.count.wrapping_sub(1);
        });
    }
}

impl Bus<Port> for ProgrammableIntervalTimer8253 {
    fn read(&self, port: u16) -> Result<u8> {
        let counter = port & 0b11;
        // Only the 3 channels can be read from.
        assert_ne!(counter, 4);

        let counter = &mut self.inner.borrow_mut().counters[counter as usize];
        match counter.read_write {
            ReadWrite::Latch => {
                counter.read_write = ReadWrite::LoThenHiByte;
                Ok((counter.count >> 8) as u8)
            }

            ReadWrite::LoByte => Ok((counter.count & 0xFF) as u8),

            ReadWrite::HiByte => Ok((counter.count >> 8) as u8),

            ReadWrite::LoThenHiByte => {
                counter.read_write = ReadWrite::Latch;
                Ok((counter.count & 0xFF) as u8)
            }
        }
    }

    fn write(&mut self, port: u16, value: u8) -> Result<()> {
        // log::info!("Writing to timer port: {:04X} <= {:02X}", port, value);

        let counter = port & 0b11;

        if counter == 0x03 {
            let control_register = ControlRegister::try_from(value)?;
            let counter = &mut self.inner.borrow_mut().counters[control_register.counter as usize];
            counter.read_write = control_register.read_write;
            counter.mode = control_register.mode;
            counter.bcd = control_register.bcd;

            log::info!("Set counter {} to {:?}", control_register.counter, counter);
        } else {
            let counter = &mut self.inner.borrow_mut().counters[counter as usize];

            match counter.read_write {
                ReadWrite::Latch => {
                    counter.count |= u16::from(value) << 8;
                    counter.read_write = ReadWrite::LoThenHiByte;
                }
                ReadWrite::LoByte => {
                    counter.count = u16::from(value);
                }
                ReadWrite::HiByte => {
                    counter.count = u16::from(value) << 8;
                }
                ReadWrite::LoThenHiByte => {
                    counter.count = u16::from(value);
                    counter.read_write = ReadWrite::Latch;
                }
            }

            // todo!()
        }

        Ok(())
    }
}
