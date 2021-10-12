use std::cell::RefCell;
use std::rc::Rc;

pub use bus::segment_and_offset;
pub use bus::Address;
pub use bus::BusInterface;
pub use irq::InterruptHandler;
use mrc_x86::Segment;

use crate::bus::Bus;
use crate::cpu::{ExecuteResult, CPU};
use crate::error::Result;
use crate::io::IOController;
use crate::irq::InterruptController;

mod bus;
pub mod cpu;
pub mod error;
mod io;
mod irq;
pub mod ram;
pub mod rom;

trait Installable {
    fn install(bus: &mut Bus) -> Result<()>;
}

/// An emulator with some basic components.
pub struct Emulator {
    bus: Rc<RefCell<Bus>>,
    _io_controller: Rc<RefCell<IOController>>,
    interrupt_controller: Rc<RefCell<InterruptController>>,
    cpu: CPU,
}

impl Default for Emulator {
    fn default() -> Self {
        let bus = Rc::new(RefCell::new(Bus::default()));
        let io_controller = Rc::new(RefCell::new(IOController::default()));
        let interrupt_controller = Rc::new(RefCell::new(InterruptController::default()));
        Self {
            bus: Rc::clone(&bus),
            _io_controller: io_controller.clone(),
            interrupt_controller: interrupt_controller.clone(),

            cpu: CPU::new(bus, Some(io_controller), Some(interrupt_controller)),
        }
    }
}

impl Emulator {
    pub fn set_reset_vector(&mut self, segment: u16, offset: u16) {
        self.cpu.set_segment_value(Segment::Cs, segment);
        self.cpu.state.ip = offset;
    }

    pub fn bus(&self) -> Rc<RefCell<Bus>> {
        self.bus.clone()
    }

    pub fn interrupt_controller(&self) -> Rc<RefCell<InterruptController>> {
        self.interrupt_controller.clone()
    }

    /// Start the emulator. This function will not return until the emulator is requested to exit
    /// or it runs into an error.
    pub fn boot(&mut self) {
        if let Err(err) = self.cpu.start() {
            log::error!("{}", err);
        }
    }

    pub fn tick(&mut self) -> bool {
        self.cpu.tick().unwrap() == ExecuteResult::Continue
    }
}
