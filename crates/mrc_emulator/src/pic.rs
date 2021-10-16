use crate::error::Result;
use crate::io::IOInterface;

/// Emulates a 8259A programmable interrupt controller.
///
/// Code assumes it is 2 8259A's running in master/slave mode.
pub struct ProgrammableInterruptController8259 {
    /// Specifies which ICW is expected next.  When the chip starts, it is set to 0 which means ICW1
    /// is expected.
    icw_index: u8,

    /// Used to filter out interrupts. If bit 1 is set to 0, interrupt 1 will not trigger.
    mask_register: u8,

    /// When a bit is set, the corresponding interrupt has made a request.
    request_register: u8,

    /// When a bit is set, the corresponding interrupt is being run.
    in_service_register: u8,
    // mask_register: u8,
    // request_register: u8,
    // service_register: u8,
    // /// Which ICW we are at.
    // icw_step: u8,
    // icw: [u8; 5],
    // ocw: [u8; 5],
    // _interrupt_vector_offset: u8,
    // /// Which IRQ has the highest priority
    // _priority: u8,
    // _auto_eoi: u8,
    // /// Remember what to return when reading from OCW3
    // read_mode: u8,
    // _vector: u8,
    // _last_interrupt: u8,
    // _enabled: u8,
}

impl Default for ProgrammableInterruptController8259 {
    fn default() -> Self {
        Self {
            icw_index: 0,
            mask_register: 0,
            request_register: 0,
            in_service_register: 0,
        }
    }
}

impl ProgrammableInterruptController8259 {
    fn write_icw1(&mut self, value: u8) -> Result<()> {
        assert_ne!(value & (1 << 4), 0);

        self.mask_register = 0x00;
        // TODO: Assign highest priority to IRQ0 and lowest to IRQ7.
        // TODO: Set slave identification number to 7.
        // TODO: Clear special mode and sets the status read to request_register.

        // Increase the icw_index so that we expect ICW2 next.
        self.icw_index += 1;

        Ok(())
    }

    fn write_icw2(&mut self, value: u8) -> Result<()> {
        Ok(())
    }

    fn write_icw(&mut self, value: u8) -> Result<()> {
        Ok(())
        /*
        {
            // bit 7-5  = 0  only used in 80/85 mode
            // bit 4    = 1  ICW1 is being issued
            // bit 3    = 0  edge triggered mode
            //          = 1  level triggered mode
            // bit 2    = 0  successive interrupt vectors use 8 bytes
            //          = 1  successive interrupt vectors use 4 bytes
            // bit 1    = 0  cascade mode
            //          = 1  single mode, no ICW3 needed
            // bit 0    = 0  no ICW4 needed
            //          = 1  ICW4 needed

            let icw1_is_being_issued: u8 = value >> 4 & 1;
            let trigger_mode = value >> 3 & 1;
            let vector_size = value >> 2 & 1;
            let mode = value >> 1 & 1;
            let icw4_needed = value & 1;

            log::info!(
                "PIC: icw1_is_being_issued: {}",
                if icw1_is_being_issued == 1 {
                    "yes"
                } else {
                    "no"
                }
            );
            log::info!(
                "PIC: trigger_mode: {}",
                if trigger_mode == 0 {
                    "edge trigger mode"
                } else {
                    "level trigger mode"
                }
            );
            log::info!("PIC: vector_size: {} bytes", 8 >> vector_size);
            log::info!(
                "PIC: mode: {}",
                if mode == 0 {
                    "cascade mode"
                } else {
                    "single, no ICW3 needed"
                }
            );
            log::info!(
                "PIC: icw4_needed: {}",
                if icw4_needed == 1 { "yes" } else { "no" }
            );
        }

        if value & 0x10 != 0 {
            // ICW1
            log::info!("PIC: ICW1 = {:#02X}", value);

            self.mask_register = 0x00;
            self.icw[1] = value;
            self.icw_step = 2;
            self.read_mode = 0;
        } else if (value & 0x08) == 0 {
            // OCW2
            log::info!("PIC: OCW2 = {:#02X}", value);

            self.ocw[2] = value;
            match value & 0xE0 {
                0x60 => {
                    // Specify EOI.
                    self.request_register &= !(1 << (value & 0x03)); // TODO: ! == ~
                    self.service_register &= !(1 << (value & 0x03)); // TODO: ! == ~
                }

                0x40 => {
                    // No operation.
                }

                0x20 => {
                    // Non-specific EOI.
                    self.request_register &= !self.service_register; // TODO: ! == ~
                    self.service_register = 0x00;
                }

                _ => {
                    log::warn!("PIC: Unhandled EOI type. ({:#02X})", value & 0xE0);
                }
            }
        } else {
            // OCW3
            log::info!("PIC: OCW3: {:#02X}", value);

            self.ocw[3] = value;
            if value & 0x02 != 0 {
                self.read_mode = value & 1;
            }
        }

        Ok(())
        */
    }

    fn write_data(&mut self, _value: u8) -> Result<()> {
        Ok(())
    }
}

impl IOInterface for ProgrammableInterruptController8259 {
    fn read(&self, _port: u16) -> Result<u8> {
        todo!()
    }

    fn write(&mut self, port: u16, value: u8) -> Result<()> {
        // TODO: These conditions are not 100% accurate.
        if port & 1 == 0 && value & (1 << 4) != 0 && self.icw_index == 0 {
            self.write_icw1(value)
        } else if port & 1 != 0 && self.icw_index == 1 {
            self.write_icw2(value)
        } else {
            todo!()
        }
    }
}

#[cfg(test)]
mod test {
    use crate::io::IOInterface;
    use crate::pic::ProgrammableInterruptController8259;

    #[test]
    fn initialize() {
        env_logger::init();

        let mut pic = ProgrammableInterruptController8259::default();

        // PIC is waiting for ICW1

        // Write ICW1
        assert!(pic.write(0x0020, 0x13).is_ok());
        assert_eq!(1, pic.icw_index);
        assert_eq!(0x00, pic.mask_register);
        assert_eq!(0x00, pic.request_register);
        assert_eq!(0x00, pic.in_service_register);

        assert!(pic.write(0x0021, 0x08).is_ok());

        assert!(pic.write(0x0021, 0x09).is_ok());

        assert!(pic.write(0x0021, 0xFF).is_ok());
    }
}
