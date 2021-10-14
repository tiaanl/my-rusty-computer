#![allow(unused_variables)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::error::Result;

fn _port_name(port: u16) -> Option<&'static str> {
    match port {
        0x20 | 0x21 => Some("PIC1"),
        0xA0 | 0xA1 => Some("PIC2"),
        _ => None,
    }
}

pub trait IOInterface {
    fn read(&self, port: u16) -> Result<u8>;
    fn write(&mut self, port: u16, value: u8) -> Result<()>;
}

#[derive(Default)]
pub struct IOController {
    interfaces: HashMap<u16, Rc<RefCell<dyn IOInterface>>>,
}

impl IOInterface for IOController {
    fn read(&self, port: u16) -> Result<u8> {
        if let Some(interface) = self.interfaces.get(&port) {
            interface.borrow().read(port).and_then(|value| {
                log::info!("Read {:#02X} from port {:#04X}", value, port);
                Ok(value)
            })
        } else {
            log::warn!("Reading from unmapped port. ({:04X})", port);
            Ok(0)
        }
    }

    fn write(&mut self, port: u16, value: u8) -> Result<()> {
        if let Some(interface) = self.interfaces.get(&port) {
            log::info!("IO: Writing {:#02X} to port {:#04X}", value, port);
            interface.borrow_mut().write(port, value)
        } else {
            log::warn!("Writing to unmapped port. ({:04X})", port);
            Ok(())
        }
    }
}

impl IOController {
    pub fn map(&mut self, port: u16, interface: Rc<RefCell<dyn IOInterface>>) {
        if self.interfaces.get(&port).is_some() {
            log::warn!("Overwriting port with new interface. ({:04X})", port);
        }
        self.interfaces.insert(port, interface);
    }

    pub fn map_range(&mut self, first_port: u16, count: u16, interface: Rc<RefCell<dyn IOInterface>>) {
        for port in first_port..(first_port + count) {
            self.map(port, interface.clone());
        }
    }

    pub fn map_set(&mut self, ports: &[u16], interface: Rc<RefCell<dyn IOInterface>>) {
        for port in ports {
            self.map(*port, interface.clone());
        }
    }
}
