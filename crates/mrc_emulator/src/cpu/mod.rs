mod executor;
mod state;

pub use executor::{execute, ExecuteResult};
pub use state::{ByteRegister, Flags, State, WordRegister};

use crate::{error, Address, Bus, Cpu, ExecuteError, Port};
use mrc_decoder::{decode_instruction, DecodedInstruction};
use mrc_instruction::Segment;
use Segment::CS;

/// An emulated 8086 CPU.  Contains all data and functions to access it.
pub struct CPU<D: Bus<Address>, I: Bus<Port>> {
    pub state: State,
    io_controller: I,
    bus: D,
}

impl<D: Bus<Address>, I: Bus<Port>> CPU<D, I> {
    pub fn new(bus: D, io_controller: I) -> Self {
        Self {
            // By default when the CPU starts, we enable interrupts and the 2nd flag is always set.
            state: State::default().with_flags(Flags::RESERVED_1 | Flags::INTERRUPT),
            io_controller,
            bus,
        }
    }

    pub fn reset(&mut self) {
        self.state = State::default();
        self.state.set_segment(CS, 0xF000);
        self.state.ip = 0xFFF0;
    }

    pub fn jump_to(&mut self, segment: u16, offset: u16) {
        self.state.set_segment(CS, segment);
        self.state.ip = offset;
    }

    pub fn tick(&mut self) -> Result<ExecuteResult, error::Error> {
        const PRINT: bool = false;

        let cs = self.state.segment(CS);
        let ip = self.state.ip;

        let instruction = decode_instruction(self)?;

        if PRINT {
            println!("state: {}", self.state);

            if false {
                let bytes_read = (self.state.ip - ip) as u8;
                let mut byte_buffer = [0_u8; 16];

                for i in 0..(bytes_read as usize) {
                    let byte = self
                        .bus
                        .read(mrc_instruction::Address::new(cs, ip + i as u16).flat())?;
                    byte_buffer[i as usize] = byte;
                }

                println!(
                    "{}",
                    DecodedInstruction::new(
                        mrc_instruction::Address::new(cs, ip),
                        &byte_buffer[..bytes_read as usize],
                        instruction,
                        bytes_read
                    )
                );
            } else {
                println!("{:04X}:{:04X} {}", cs, ip, &instruction);
            }
        }

        execute(self, &instruction)
    }

    pub fn start(&mut self) -> Result<(), error::Error> {
        loop {
            if let Ok(result) = self.tick() {
                if result == ExecuteResult::Stop {
                    break;
                }
            }
        }

        Ok(())
    }

    pub fn read_from_bus(&self, address: Address) -> Result<u8, error::Error> {
        self.bus.read(address)
    }
}

/// The decoder requires an iterator to fetch bytes.
impl<D: Bus<Address>, I: Bus<Port>> Iterator for CPU<D, I> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let address = mrc_instruction::Address::new(self.state.segment(CS), self.state.ip).flat();
        if let Ok(byte) = self.bus.read(address) {
            let (new_ip, overflow) = self.state.ip.overflowing_add(1);
            if overflow {
                log::error!("IP overflow!");
                None
            } else {
                self.state.ip = new_ip;
                Some(byte)
            }
        } else {
            None
        }
    }
}

impl<D: Bus<Address>, I: Bus<Port>> Cpu for CPU<D, I> {
    fn reset(&mut self) {
        Self::reset(self)
    }

    fn step(&mut self) -> Result<usize, ExecuteError> {
        Self::tick(self).unwrap();
        Ok(0)
    }
}
