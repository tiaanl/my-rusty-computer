//! Intel 8253 Programmable Interrupt Timer
//! https://en.wikipedia.org/wiki/Intel_8253

// Base + 0b00 - Channel 0 data port (read/write)
// Base + 0b01 - Channel 1 data port (read/write)
// Base + 0x10 - Channel 2 data port (read/write)
// Base + 0b11 - Mode/Command register (write only, reads are ignored)
// *Base = Base address where timer is mapped

// IBM PC/XT 5150 had 1 PIT and it was mapped to base 0x40.

// const TICK_RATE: usize = 1193182;

use crate::{Bus, Port};
use std::cell::RefCell;

const COUNTER_COUNT: u8 = 3;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ReadWrite {
    Latch,
    LoByte,
    HiByte,
    LoThenHiByte,
}

impl From<u8> for ReadWrite {
    fn from(value: u8) -> Self {
        use ReadWrite::*;

        match value {
            0b00 => Latch,
            0b01 => LoByte,
            0b10 => HiByte,
            0b11 => LoThenHiByte,
            _ => unreachable!(),
        }
    }
}

/// Control Register Bits
/// | SC1 | SC0 | RW1 | RW0 | M2 | M1 | M0 | BCD |
#[derive(Copy, Clone, Debug)]
struct ControlRegister {
    /// SC - Select Counter
    /// | SC1 | SC0 |
    /// |   0 |   0 | Select counter 0
    /// |   0 |   1 | Select counter 1
    /// |   1 |   0 | Select counter 2
    /// |   1 |   1 | Not applicable on 8253
    select_counter: u8,

    /// RW - Read/write
    /// | RW1 | RW0 |
    /// |   0 |   0 | Counter latch
    /// |   0 |   1 | R/W least significant byte only
    /// |   1 |   0 | R/W most significant bits only
    /// |   1 |   1 | R/W least significant byte first and then most significant byte
    read_write: ReadWrite,

    /// M - Mode
    /// | M2 | M1 | M0 |
    /// |  0 |  0 |  0 | Mode 0
    /// |  0 |  0 |  1 | Mode 1
    /// |  x |  1 |  0 | Mode 2
    /// |  x |  1 |  1 | Mode 3
    /// |  1 |  0 |  0 | Mode 4
    /// |  1 |  0 |  1 | Mode 5
    mode: u8,

    /// BCD - Binary/BCD
    /// | BCD |
    /// |   0 | Binary counter (16-bits)
    /// |   1 | BCD counter (4 d..)
    bcd: bool,
}

#[cfg(test)]
impl ControlRegister {
    fn new(select_counter: u8, read_write: ReadWrite, mode: u8, bcd: bool) -> Self {
        debug_assert!(select_counter < COUNTER_COUNT);
        debug_assert!(mode < 6);
        Self {
            select_counter,
            read_write,
            mode,
            bcd,
        }
    }
}

impl From<u8> for ControlRegister {
    fn from(value: u8) -> Self {
        Self {
            select_counter: value >> 6,
            read_write: ReadWrite::from(value >> 4 & 0b11),
            mode: value >> 1 & 0b111,
            bcd: (value & 0b1) != 0,
        }
    }
}

impl From<ControlRegister> for u8 {
    fn from(control_register: ControlRegister) -> Self {
        let mut result = 0_u8;

        // Select counter
        debug_assert!(control_register.select_counter <= 3);
        result |= control_register.select_counter << 6;

        // Read/Write
        result |= match control_register.read_write {
            ReadWrite::Latch => 0b00 << 4,
            ReadWrite::LoByte => 0b01 << 4,
            ReadWrite::HiByte => 0b10 << 4,
            ReadWrite::LoThenHiByte => 0b11 << 4,
        };

        // Mode
        debug_assert!(control_register.mode <= 5);
        result |= control_register.mode << 1;

        // BCD
        if control_register.bcd {
            result |= 1;
        }

        result
    }
}

#[derive(Debug)]
struct Counter {
    read_write: ReadWrite,
    mode: u8,
    bcd: bool,
    count: u16,
    latched: Option<u16>,
}

impl Default for Counter {
    fn default() -> Self {
        Self {
            read_write: ReadWrite::Latch,
            mode: 0,
            bcd: false,
            count: 0,
            latched: None,
        }
    }
}

impl Counter {
    fn tick(&mut self) {
        self.count = self.count.wrapping_sub(1);
        if self.count == 0 && self.mode == 0 {
            println!("TRIGGERING INTERRUPT 0");
        }
    }
}

#[derive(Default)]
struct Inner {
    counters: [Counter; COUNTER_COUNT as usize],
}

#[derive(Default)]
pub struct ProgrammableIntervalTimer8253 {
    inner: RefCell<Inner>,
}

impl ProgrammableIntervalTimer8253 {
    pub fn tick(&mut self) {
        self.inner
            .borrow_mut()
            .counters
            .iter_mut()
            .for_each(Counter::tick);
    }
}

impl Bus<Port> for ProgrammableIntervalTimer8253 {
    fn read(&self, port: u16) -> u8 {
        let counter = port & 0b11;

        if counter == 0b11 {
            log::warn!("Only the 3 counters can be read from.");
            return 0;
        }

        let counter = &mut self.inner.borrow_mut().counters[counter as usize];
        let value = match counter.read_write {
            ReadWrite::Latch => {
                counter.read_write = ReadWrite::LoThenHiByte;
                (counter.count >> 8) as u8
            }

            ReadWrite::LoByte => (counter.count & 0xFF) as u8,

            ReadWrite::HiByte => (counter.count >> 8) as u8,

            ReadWrite::LoThenHiByte => {
                counter.read_write = ReadWrite::Latch;
                (counter.count & 0xFF) as u8
            }
        };

        log::info!("Read from timer port {:#06X}: {:#04X}", port, value);

        value
    }

    fn write(&mut self, port: u16, value: u8) {
        // log::info!("Writing to timer port: {:#06X} <= {:#04X}", port, value);

        let counter = port & 0b11;

        if counter == 0x03 {
            // Writing to the Mode/Command register.
            let control_register = ControlRegister::from(value);
            debug_assert!(control_register.select_counter < COUNTER_COUNT);
            let counter =
                &mut self.inner.borrow_mut().counters[control_register.select_counter as usize];

            if matches!(control_register.read_write, ReadWrite::Latch) {
                let latched_count = counter.count;
                // If we already have a latched value, then we just ignore the request to latch.
                if counter.latched.is_none() {
                    counter.latched = Some(latched_count);

                    log::info!(
                        "Latched counter {} to {}",
                        control_register.select_counter,
                        latched_count
                    );
                }
            } else {
                counter.read_write = control_register.read_write;
                counter.mode = control_register.mode;
                counter.bcd = control_register.bcd;

                log::info!(
                    "Set counter {} to {:?}",
                    control_register.select_counter,
                    counter
                );
            }
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

            log::info!("Set counter {} to {:?}", port & 0b11, counter);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initialize() {
        pretty_env_logger::init();

        let mut timer = ProgrammableIntervalTimer8253::default();

        // Counter 0 to mode 0 and binary.
        timer.write(
            0x43,
            ControlRegister::new(0, ReadWrite::LoByte, 0, false).into(),
        );

        // Set the count for timer 0 to 64.
        timer.write(0x40, 64);

        // TODO: There should be no need to tick the timer?
        timer.tick();

        // Read the value of the timer.
        let value = timer.read(0x40);

        assert!(value < 64);
    }
}
