use mrc_emulator::{error::Result, io::IOInterface};

#[derive(Copy, Clone, PartialEq)]
pub enum OperationMode {
    /// There is only 1 PIC.
    Single,

    /// 2 PICs are set up in master/slave and this one is set to the master.
    Master,

    /// 2 PICs are set up in master/slave and this one is set to the slave.
    Slave,
}

/// Emulates an Intel 8259A programmable interrupt controller. The controller was used for both
/// 8085 and 8086/88 based machines, but we are only modeling relevant information for the 8086/88
/// processors.
pub struct ProgrammableInterruptController8259 {
    /// The mode that this PIC is operating in.
    mode: OperationMode,

    /// Specifies which ICW is expected next.  When the chip starts, it is set to 0 which means ICW1
    /// is expected.
    icw_index: u8,

    /// Initialization control words.
    icw: [u8; 4],

    /// Used to filter out interrupts. E.g. if bit 1 is set to 0, interrupt 1 will not trigger.
    mask_register: u8,

    /// When a bit is set, the corresponding interrupt has made a request.
    _interrupt_request_register: u8,

    /// When a bit is set, the corresponding interrupt is being run.
    _in_service_register: u8,
}

impl ProgrammableInterruptController8259 {
    pub fn new(mode: OperationMode) -> Self {
        Self {
            mode,
            icw_index: 0,
            icw: [0; 4],
            mask_register: 0,
            _interrupt_request_register: 0,
            _in_service_register: 0,
        }
    }

    pub fn requires_icw4(&self) -> bool {
        self.icw[0] & (1 << 0) != 0
    }

    /// True if the PIC is operating is single mode and not cascade mode. (bit 1 is set)
    pub fn is_single(&self) -> bool {
        self.icw[0] & (1 << 1) != 0
    }
}

impl IOInterface for ProgrammableInterruptController8259 {
    fn read(&self, _port: u16) -> Result<u8> {
        Ok(0)
    }

    fn write(&mut self, port: u16, value: u8) -> Result<()> {
        if port & 1 == 0 && value & (1 << 4) != 0 {
            // Conditions for accepting ICW1:
            // - The A0 line from the address bus must be a zero.
            // - The D4 bit must be set. If the D4 bit is not set, we would be programming either OCW2
            //   or OCW3.

            // Bit layout for ICW1:
            // 0: This bit specifies whether ICW4 is required.
            // 1: Single (1) or cascade (0).
            // 2: Address interval 4 (1) or address interval 8 (0).
            // 3: Level trigger mode (1) or edge trigger mode (0).
            // 4: Specifies ICW1 is being set (always 1).
            // 5..7: Interrupt vector list?
            self.icw[0] = value;
            self.icw_index = 1; // Not we expect ICW2.

            self.mask_register = 0x00;

            println!("Setting ICW1 to {:#02X}", value);
        } else if port & 1 == 1 && self.icw_index == 1 {
            // Conditions for ICW2:
            // - We already received ICW1.
            // - The A0 line from the address bus must be an one.
            self.icw[1] = value;

            // In single mode we don't expect ICW3 and skip to ICW4.
            self.icw_index = if self.is_single() { 3 } else { 2 };

            println!("Setting ICW2 to {:#02X}", value);
        } else if port & 1 == 1 && self.icw_index == 2 && !self.is_single() {
            // Conditions for ICW3:
            // - We already received ICW2.
            // - The A0 line from the address bus must be an one.
            self.icw[2] = value;
            self.icw_index = 3; // Expect ICW4

            println!("Setting ICW3 to {:#02X}", value);
        } else if self.icw_index == 3 && self.requires_icw4() {
            self.icw[3] = value;
            self.icw_index = 4; // Done initialization.

            println!("Setting ICW4 to {:#02X}", value);
        } else {
            self.mask_register = value;
            todo!()
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn initialize() {
        let mut pic = ProgrammableInterruptController8259::new(OperationMode::Single);

        assert!(pic.write(0x20, 0x17).is_ok());
        assert!(pic.write(0x21, 0x98).is_ok());
        assert!(pic.write(0x21, 0x01).is_ok());

        assert_eq!(0x17, pic.icw[0]);
        assert_eq!(0x98, pic.icw[1]);
        assert_eq!(0x00, pic.icw[2]);
        assert_eq!(0x01, pic.icw[3]);
        assert!(pic.is_single());
        assert!(pic.requires_icw4());
        assert_eq!(0x00, pic.mask_register);
    }

    #[test]
    fn initialize_cascade() {
        let mut master = ProgrammableInterruptController8259::new(OperationMode::Master);
        let mut slave = ProgrammableInterruptController8259::new(OperationMode::Slave);

        // Master initialization:
        assert!(master.write(0xD0, 0x1D).is_ok());
        assert!(master.write(0xD1, 0x6B).is_ok());
        assert!(master.write(0xD1, 0x08).is_ok());
        assert!(master.write(0xD1, 0x6B).is_ok());

        // Slave initialization:
        assert!(slave.write(0xF0, 0x1D).is_ok());
        assert!(slave.write(0xF1, 0x6B).is_ok());

        assert!(slave.write(0xF0, 0x03).is_ok());
        assert!(master.write(0xD0, 0x80).is_ok());
    }
}
