#![allow(unused_variables)]

use crate::error::Result;

fn _port_name(port: u16) -> Option<&'static str> {
    match port {
        0x20 | 0x21 => Some("PIC1"),
        0xA0 | 0xA1 => Some("PIC2"),
        _ => None,
    }
}

#[derive(Default)]
pub struct IOController {}

impl IOController {
    pub fn read_byte(&self, port: u16) -> Result<u8> {
        let value = 0u8;
        // log::info!("IO R {:04X} {:02X}", port, value);
        Ok(value)
    }

    pub fn write_byte(&mut self, port: u16, value: u8) -> Result<()> {
        // log::info!("IO W {:04X} {:02X}", port, value);
        Ok(())
    }

    pub fn read_word(&self, port: u16) -> Result<u16> {
        let value = 0u16;
        // log::info!("IO R {:04X} {:04X}", port, value);
        Ok(value)
    }

    pub fn write_word(&mut self, port: u16, value: u16) -> Result<()> {
        // log::info!("IO W {:04X} {:04X}", port, value);
        Ok(())
    }
}
