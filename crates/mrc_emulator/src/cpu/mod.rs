use std::cell::RefCell;
use std::rc::Rc;

use bitflags::bitflags;

use mrc_decoder::decode_instruction;
use mrc_x86::{Register, Segment};

use crate::bus::{BusInterface, segment_and_offset};
use crate::cpu::executor::{execute, ExecuteResult};
use crate::error::{Error, Result};

mod executor;

bitflags! {
    pub struct Flags : u16 {
        const CARRY = 1 << 0;
        const _UNDEFINED_1 = 1 << 1;
        const PARITY = 1 << 2;
        const _UNDEFINED_3 = 1 << 3;
        const AUX_CARRY = 1 << 4;
        const _UNDEFINED_5 = 1 << 5;
        const ZERO = 1 << 6;
        const SIGN = 1 << 7;
        const TRAP = 1 << 8;
        const INTERRUPT = 1 << 9;
        const DIRECTION = 1 << 10;
        const OVERFLOW = 1 << 11;
    }
}

/// An emulated 8086 CPU.  Contains all data and functions to access it.
pub struct CPU {
    registers: [u16; 8],
    segments: [u16; 4],

    // TODO: Do not have this public.  Right now it is required to set the reset vector. Maybe set
    //       with a builder config?
    pub(crate) ip: u16,
    flags: Flags,

    bus: Rc<RefCell<dyn BusInterface>>,
}

impl CPU {
    pub fn new(bus: Rc<RefCell<dyn BusInterface>>) -> Self {
        Self {
            registers: [0; 8],
            segments: [0; 4],
            ip: 0,
            flags: Flags::empty(),
            bus,
        }
    }

    pub fn start(&mut self) -> Result<()> {
        loop {
            // print_bus_bytes(self);

            let start_cs = self.segments[Segment::Cs as usize];
            let start_ip = self.ip;

            let instruction = match decode_instruction(self) {
                Ok(instruction) => {
                    instruction
                }
                Err(err) => {
                    log::error!("CPU Error: {}", err);
                    return Err(Error::DecodeError(err));
                }
            };

            println!("{:04X}:{:04X} {}", start_cs, start_ip, &instruction);

            let result = execute(self, &instruction)?;
            if result == ExecuteResult::Stop {
                break;
            }
        }

        Ok(())
    }

    fn get_byte_register_value(&self, register: Register) -> u8 {
        use Register::*;

        let byte: u8 = match register {
            AlAx => self.registers[Register::AlAx as usize].to_le_bytes()[0],
            ClCx => self.registers[Register::ClCx as usize].to_le_bytes()[0],
            DlDx => self.registers[Register::DlDx as usize].to_le_bytes()[0],
            BlBx => self.registers[Register::BlBx as usize].to_le_bytes()[0],
            AhSp => self.registers[Register::AlAx as usize].to_le_bytes()[1],
            ChBp => self.registers[Register::ClCx as usize].to_le_bytes()[1],
            DhSi => self.registers[Register::DlDx as usize].to_le_bytes()[1],
            BhDi => self.registers[Register::BlBx as usize].to_le_bytes()[1],
        } as u8;
        byte
    }

    fn get_word_register_value(&self, register: Register) -> u16 {
        use Register::*;

        match register {
            AlAx => self.registers[Register::AlAx as usize],
            ClCx => self.registers[Register::ClCx as usize],
            DlDx => self.registers[Register::DlDx as usize],
            BlBx => self.registers[Register::BlBx as usize],
            AhSp => self.registers[Register::AhSp as usize],
            ChBp => self.registers[Register::ChBp as usize],
            DhSi => self.registers[Register::DhSi as usize],
            BhDi => self.registers[Register::BhDi as usize],
        }
    }

    fn set_byte_register_value(&mut self, register: Register, value: u8) {
        use Register::*;

        match register {
            AlAx => {
                let mut bytes = self.registers[Register::AlAx as usize].to_le_bytes();
                bytes[0] = value;
                self.registers[Register::AlAx as usize] = u16::from_le_bytes(bytes);
            }
            ClCx => {
                let mut bytes = self.registers[Register::ClCx as usize].to_le_bytes();
                bytes[0] = value;
                self.registers[Register::ClCx as usize] = u16::from_le_bytes(bytes);
            }
            DlDx => {
                let mut bytes = self.registers[Register::DlDx as usize].to_le_bytes();
                bytes[0] = value;
                self.registers[Register::DlDx as usize] = u16::from_le_bytes(bytes);
            }
            BlBx => {
                let mut bytes = self.registers[Register::BlBx as usize].to_le_bytes();
                bytes[0] = value;
                self.registers[Register::BlBx as usize] = u16::from_le_bytes(bytes);
            }
            AhSp => {
                let mut bytes = self.registers[Register::AlAx as usize].to_le_bytes();
                bytes[1] = value;
                self.registers[Register::AlAx as usize] = u16::from_le_bytes(bytes);
            }
            ChBp => {
                let mut bytes = self.registers[Register::ClCx as usize].to_le_bytes();
                bytes[1] = value;
                self.registers[Register::ClCx as usize] = u16::from_le_bytes(bytes);
            }
            DhSi => {
                let mut bytes = self.registers[Register::DlDx as usize].to_le_bytes();
                bytes[1] = value;
                self.registers[Register::DlDx as usize] = u16::from_le_bytes(bytes);
            }
            BhDi => {
                let mut bytes = self.registers[Register::BlBx as usize].to_le_bytes();
                bytes[1] = value;
                self.registers[Register::BlBx as usize] = u16::from_le_bytes(bytes);
            }
        };
    }

    fn set_word_register_value(&mut self, register: Register, value: u16) {
        use Register::*;

        match register {
            AlAx => self.registers[Register::AlAx as usize] = value,
            ClCx => self.registers[Register::ClCx as usize] = value,
            DlDx => self.registers[Register::DlDx as usize] = value,
            BlBx => self.registers[Register::BlBx as usize] = value,
            AhSp => self.registers[Register::AhSp as usize] = value,
            ChBp => self.registers[Register::ChBp as usize] = value,
            DhSi => self.registers[Register::DhSi as usize] = value,
            BhDi => self.registers[Register::BhDi as usize] = value,
        }
    }

    pub fn get_segment_value(&self, segment: Segment) -> u16 {
        use Segment::*;

        match segment {
            Es => self.segments[segment as usize],
            Cs => self.segments[segment as usize],
            Ss => self.segments[segment as usize],
            Ds => self.segments[segment as usize],
        }
    }

    pub fn set_segment_value(&mut self, segment: Segment, value: u16) {
        use Segment::*;

        match segment {
            Es => self.segments[segment as usize] = value,
            Cs => self.segments[segment as usize] = value,
            Ss => self.segments[segment as usize] = value,
            Ds => self.segments[segment as usize] = value,
        }
    }
}

/// The decoder requires an iterator to fetch bytes.
impl Iterator for CPU {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let address = segment_and_offset(self.segments[Segment::Cs as usize], self.ip);
        if let Ok(byte) = self.bus.borrow().read(address) {
            let (new_ip, overflow) = self.ip.overflowing_add(1);
            if overflow {
                log::error!("IP overflow!");
                None
            } else {
                self.ip = new_ip;
                Some(byte)
            }
        } else {
            None
        }
    }
}

fn print_bus_bytes(cpu: &CPU) {
    print!("Bytes to decode: ");
    let start = segment_and_offset(cpu.get_segment_value(Segment::Cs), cpu.ip);
    for i in 0..5 {
        let addr = start + i;

        let byte = match cpu.bus.borrow().read(addr) {
            Ok(byte) => byte,
            Err(_) => break,
        };

        print!("{:02X} ", byte);
    }
    println!();
}
