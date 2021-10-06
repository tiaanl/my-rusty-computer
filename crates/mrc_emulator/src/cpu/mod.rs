use std::cell::RefCell;
use std::rc::Rc;

use bitflags::bitflags;

use mrc_x86::Segment;

use crate::bus::{BusInterface, segment_and_offset};

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

    pub fn start(&mut self) {
        match mrc_decoder::decode_instruction(self) {
            Ok(instruction) => {
                println!("{}", instruction);
            }
            Err(err) => {
                log::error!("CPU Error: {}", err);
            }
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
            self.ip += 1;
            Some(byte)
        } else {
            None
        }
    }
}
