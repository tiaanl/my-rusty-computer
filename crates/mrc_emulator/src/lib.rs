use std::cell::RefCell;
use std::rc::Rc;

pub use bus::BusInterface;
use mrc_instruction::Segment;

use crate::bus::Bus;
use crate::cpu::{ExecuteResult, CPU};
use crate::io::IOController;

pub mod builder;
pub mod bus;
pub mod cpu;
pub mod error;
pub mod io;
pub mod shared;

pub trait Installable {
    fn map_bus(&mut self, _bus: &mut Bus) {}
    fn map_io(&mut self, _io_controller: &mut IOController) {}
}

/// An emulator with some basic components.
pub struct Emulator {
    bus: Rc<RefCell<Bus>>,
    io_controller: Rc<RefCell<IOController>>,
    pub cpu: CPU,
}

impl Default for Emulator {
    fn default() -> Self {
        let bus = Rc::new(RefCell::new(Bus::default()));
        let io_controller = Rc::new(RefCell::new(IOController::default()));
        Self {
            bus: Rc::clone(&bus),
            io_controller: io_controller.clone(),
            cpu: CPU::new(bus, io_controller),
        }
    }
}

impl Emulator {
    pub fn set_reset_vector(&mut self, segment: u16, offset: u16) {
        self.cpu.state.set_segment_value(Segment::Cs, segment);
        self.cpu.state.ip = offset;
    }

    pub fn install(&mut self, component: &mut impl Installable) {
        component.map_bus(&mut self.bus.borrow_mut());
        component.map_io(&mut self.io_controller.borrow_mut());
    }

    pub fn bus(&self) -> Rc<RefCell<Bus>> {
        self.bus.clone()
    }

    pub fn io_controller(&self) -> Rc<RefCell<IOController>> {
        self.io_controller.clone()
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
