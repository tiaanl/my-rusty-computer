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

use crate::error::Result;
use crate::io::IOInterface;

pub struct ProgrammableInterruptController8259 {
    // imr
    mask_register: u8,
    // irr
    request_register: u8,
    // isr
    service_register: u8,
    /// Which ICW we are at.
    icw_step: u8,
    icw: [u8; 5],
    ocw: [u8; 5],
    interrupt_vector_offset: u8,
    /// Which IRQ has the highest priority
    priority: u8,
    auto_eoi: u8,
    /// Remember what to return when reading from OCW3
    read_mode: u8,
    vector: u8,
    last_interrupt: u8,
    enabled: u8,
}

impl Default for ProgrammableInterruptController8259 {
    fn default() -> Self {
        Self {
            mask_register: 0,
            request_register: 0,
            service_register: 0,
            icw_step: 0,
            icw: [0; 5],
            ocw: [0; 5],
            interrupt_vector_offset: 8,
            priority: 0,
            auto_eoi: 0,
            read_mode: 0,
            vector: 0,
            last_interrupt: 0,
            enabled: 0,
        }
    }
}

impl ProgrammableInterruptController8259 {
    fn write_command(&mut self, value: u8) -> Result<()> {
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
    }

    fn write_data(&mut self, value: u8) -> Result<()> {
        Ok(())
    }
}

impl IOInterface for ProgrammableInterruptController8259 {
    fn read(&self, port: u16) -> Result<u8> {
        todo!()
    }

    fn write(&mut self, port: u16, value: u8) -> Result<()> {
        if port & 1 == 0 {
            self.write_command(value)
        } else {
            self.write_data(value)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::pic::ProgrammableInterruptController8259;

    #[test]
    fn test() {}
}
