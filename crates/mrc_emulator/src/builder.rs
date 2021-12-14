use crate::bus::{Address, Bus, InterfaceContainer};
use crate::cpu::CPU;
use crate::io::{IOController, IOInterface};
use crate::{BusInterface, Emulator};
use mrc_instruction::Segment;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Range;
use std::rc::Rc;

#[derive(Default)]
pub struct EmulatorBuilder {
    reset_vector: Option<(u16, u16)>,
    bus_interfaces: Vec<InterfaceContainer>,
    io_interfaces: HashMap<u16, Rc<RefCell<dyn IOInterface>>>,
}

impl EmulatorBuilder {
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

    pub fn map_io(&mut self, port: u16, interface: Rc<RefCell<dyn IOInterface>>) -> &mut Self {
        self.io_interfaces.insert(port, interface);

        self
    }

    pub fn map_io_range(
        &mut self,
        first_port: u16,
        port_count: u16,
        interface: Rc<RefCell<dyn IOInterface>>,
    ) -> &mut Self {
        for port in first_port..(first_port + port_count) {
            self.io_interfaces.insert(port, interface.clone());
        }

        self
    }

    /// Consumes the builder and return an Emulator with all the builder values.
    pub fn build(self) -> Emulator {
        let bus = Rc::new(RefCell::new(Bus::with_interfaces(self.bus_interfaces)));

        let io_controller = Rc::new(RefCell::new(IOController::with_interfaces(
            self.io_interfaces,
        )));

        let mut emulator = Emulator {
            bus: bus.clone(),
            io_controller: io_controller.clone(),
            cpu: CPU::new(bus, io_controller),
        };

        if let Some((segment, offset)) = self.reset_vector {
            emulator.cpu.state.set_segment_value(Segment::Cs, segment);
            emulator.cpu.state.ip = offset;
        }

        emulator
    }
}
