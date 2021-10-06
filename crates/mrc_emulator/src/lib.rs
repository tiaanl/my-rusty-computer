use std::cell::RefCell;
use std::rc::Rc;

use mrc_x86::Segment;

use crate::bus::{Bus, BusError};
use crate::cpu::CPU;

mod cpu;
mod bus;
pub mod ram;
pub mod rom;

trait Installable {
    fn install(bus: &mut Bus) -> Result<(), BusError>;
}

/// An emulator with some basic components.
pub struct Emulator {
    bus: Rc<RefCell<Bus>>,
    cpu: CPU,
}

impl Emulator {
    pub fn new() -> Self {
        let bus = Rc::new(RefCell::new(Bus::default()));
        Self {
            bus: Rc::clone(&bus),
            cpu: CPU::new(bus),
        }
    }

    pub fn set_reset_vector(&mut self, segment: u16, offset: u16) {
        self.cpu.set_segment_value(Segment::Cs, segment);
        self.cpu.ip = offset;
    }

    pub fn bus(&self) -> Rc<RefCell<Bus>> {
        Rc::clone(&self.bus)
    }

    /// Start the emulator. This function will not return until the emulator is requested to exit
    /// or it runs into an error.
    pub fn boot(&mut self) {
        self.cpu.start();
    }
}
