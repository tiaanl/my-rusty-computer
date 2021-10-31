use crate::error::Result;
use crate::io::IOInterface;

#[derive(Default)]
pub struct DMAController;

impl IOInterface for DMAController {
    fn read(&self, _port: u16) -> Result<u8> {
        todo!()
    }

    fn write(&mut self, _port: u16, _value: u8) -> Result<()> {
        todo!()
    }
}
