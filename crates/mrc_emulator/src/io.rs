use mrc_x86::OperandSize;

use crate::error::Result;

fn port_name(port: u16) -> Option<&'static str> {
    match port {
        0x20 | 0x21 => Some("PIC1"),
        0xA0 | 0xA1 => Some("PIC2"),
        _ => None,
    }
}

enum DataValue {
    Byte(u8),
    Word(u16),
}

enum Call {
    Read(u16, OperandSize),
    Write(u16, OperandSize, DataValue),
}

fn log(call: &Call) {
    let mut str = "IO ".to_owned();
    match call {
        Call::Read(port, ..) => {
            str += format!("R {:04X} ", port).as_str();
        }
        Call::Write(port, ..) => {
            str += format!("W {:04X} ", port).as_str();
        }
    }

    match call {
        Call::Read(_, size) | Call::Write(_, size, _) => match size {
            OperandSize::Byte => str += "8 ",
            OperandSize::Word => str += "16 ",
        }
    }

    if let Call::Write(_, _, value) = call {
        match value {
            DataValue::Byte(value) => str += format!("{:02X}", value).as_str(),
            DataValue::Word(value) => str += format!("{:04X}", value).as_str(),
        }
    }

    log::info!("{}", str);
}

pub struct IOController {}

impl Default for IOController {
    fn default() -> Self {
        Self {}
    }
}

impl IOController {
    pub fn read_byte(&self, port: u16) -> Result<u8> {
        let value = 0u8;
        log::info!("IO R {:04X} {:02X}", port, value);
        Ok(value)
    }

    pub fn write_byte(&mut self, port: u16, value: u8) -> Result<()> {
        log::info!("IO W {:04X} {:02X}", port, value);
        Ok(())
    }

    pub fn read_word(&self, port: u16) -> Result<u16> {
        let value = 0u16;
        log::info!("IO R {:04X} {:04X}", port, value);
        Ok(value)
    }

    pub fn write_word(&mut self, port: u16, value: u16) -> Result<()> {
        log::info!("IO W {:04X} {:04X}", port, value);
        Ok(())
    }
}
