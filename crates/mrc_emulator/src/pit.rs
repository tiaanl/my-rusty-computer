// 00A0-00AF ----	PIC 2	(Programmable Interrupt Controller 8259)
//
// 00A0	r/w	NMI mask register (XT)
//
// 00A0	r/w	PIC 2  same as 0020 for PIC 1
// 00A1	r/w	PIC 2  same as 0021 for PIC 1 except for OCW1:
// bit 7 = 0  reserved
// bit 6 = 0  enable fixed disk interrupt
// bit 5 = 0  enable coprocessor exception interrupt
// bit 4 = 0  enable mouse interrupt
// bit 3 = 0  reserved
// bit 2 = 0  reserved
// bit 1 = 0  enable redirect cascade
// bit 0 = 0  enable real-time clock interrupt

use crate::io::IOInterface;

pub struct ProgrammableInterruptController8259 {
    first_port: u16,
    _mask: u8,
}

impl ProgrammableInterruptController8259 {
    pub fn new(first_port: u16) -> Self {
        Self {
            first_port,
            _mask: 0,
        }
    }
}

impl IOInterface for ProgrammableInterruptController8259 {
    fn read(&self, port: u16) -> crate::error::Result<u8> {
        let _port_offset = port - self.first_port;

        todo!()
    }

    fn write(&mut self, _port: u16, _value: u8) -> crate::error::Result<()> {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use crate::pit::ProgrammableInterruptController8259;

    #[test]
    fn test() {}
}
