use std::cell::RefCell;
use std::rc::Rc;

pub use bus::segment_and_offset;
use mrc_x86::Segment;

use crate::bus::Bus;
use crate::cpu::CPU;
use crate::error::Result;
use crate::io::IOController;

mod bus;
mod cpu;
mod io;
pub mod error;
pub mod ram;
pub mod rom;

trait Installable {
    fn install(bus: &mut Bus) -> Result<()>;
}

/// An emulator with some basic components.
pub struct Emulator {
    bus: Rc<RefCell<Bus>>,
    io_controller: Rc<RefCell<IOController>>,
    cpu: CPU,
}

impl Emulator {
    pub fn new() -> Self {
        let bus = Rc::new(RefCell::new(Bus::default()));
        let io_controller = Rc::new(RefCell::new(IOController::default()));
        Self {
            bus: Rc::clone(&bus),
            io_controller: Default::default(),
            cpu: CPU::new(bus, Some(io_controller)),
        }
    }

    pub fn set_reset_vector(&mut self, segment: u16, offset: u16) {
        self.cpu.set_segment_value(Segment::Cs, segment);
        self.cpu.state.ip = offset;
    }

    pub fn bus(&self) -> Rc<RefCell<Bus>> {
        Rc::clone(&self.bus)
    }

    /// Start the emulator. This function will not return until the emulator is requested to exit
    /// or it runs into an error.
    pub fn boot(&mut self) {
        if let Err(err) = self.cpu.start() {
            log::error!("{}", err);
        }
    }
}
