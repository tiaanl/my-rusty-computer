use crate::bus::{Address, Bus, InterfaceContainer};
use crate::cpu::CPU;
use crate::io::{IOController, IOInterface};
use crate::irq::InterruptController;
use crate::{BusInterface, Emulator, InterruptHandler};
use mrc_x86::Segment;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Range;
use std::rc::Rc;

pub struct EmulatorBuilder {
    reset_vector: Option<(u16, u16)>,
    bus_interfaces: Vec<InterfaceContainer>,
    io_interfaces: HashMap<u16, Rc<RefCell<dyn IOInterface>>>,
    interrupt_handlers: HashMap<u8, Rc<RefCell<dyn InterruptHandler>>>,
}

impl EmulatorBuilder {
    pub fn new() -> Self {
        Self {
            reset_vector: None,
            bus_interfaces: vec![],
            io_interfaces: HashMap::new(),
            interrupt_handlers: HashMap::new(),
        }
    }

    pub fn reset_vector(&mut self, segment: u16, offset: u16) -> &mut Self {
        self.reset_vector = Some((segment, offset));
        self
    }

    pub fn map_address(
        &mut self,
        range: Range<Address>,
        interface: impl BusInterface + 'static,
    ) -> &mut Self {
        self.bus_interfaces.push(InterfaceContainer {
            range,
            interface: Box::new(interface),
        });
        self
    }

    pub fn map_io(&mut self, port: u16, interface: impl IOInterface + 'static) -> &mut Self {
        self.io_interfaces
            .insert(port, Rc::new(RefCell::new(interface)));

        self
    }

    pub fn map_io_range(
        &mut self,
        first_port: u16,
        port_count: u16,
        interface: impl IOInterface + 'static,
    ) -> &mut Self {
        let interface = Rc::new(RefCell::new(interface));

        for port in first_port..(first_port + port_count) {
            self.io_interfaces.insert(port, interface.clone());
        }

        self
    }

    pub fn map_interrupt(
        &mut self,
        interrupt_num: u8,
        interface: impl InterruptHandler + 'static,
    ) -> &mut Self {
        let interface = Rc::new(RefCell::new(interface));
        self.interrupt_handlers.insert(interrupt_num, interface);

        self
    }

    /// Consumes the builder and return an Emulator with all the builder values.
    pub fn build(self) -> Emulator {
        let bus = Rc::new(RefCell::new(Bus::with_interfaces(self.bus_interfaces)));

        let io_controller = Rc::new(RefCell::new(IOController::with_interfaces(
            self.io_interfaces,
        )));

        let interrupt_controller = Rc::new(RefCell::new(InterruptController::with_handlers(
            self.interrupt_handlers,
        )));

        let mut emulator = Emulator {
            bus: bus.clone(),
            io_controller: io_controller.clone(),
            interrupt_controller: interrupt_controller.clone(),
            cpu: CPU::new(bus, io_controller, interrupt_controller),
        };

        if let Some((segment, offset)) = self.reset_vector {
            emulator.cpu.state.set_segment_value(Segment::Cs, segment);
            emulator.cpu.state.ip = offset;
        }

        emulator
    }
}
