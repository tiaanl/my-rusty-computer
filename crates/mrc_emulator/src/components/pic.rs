//! Emulation of Intel 8259A programmable interrupt controller.

use crate::{error::Result, Bus, Port};

pub struct ProgrammableInterruptController {
    /// Set to true if all the ICW's were received and the chip is ready to operate.
    initialized: bool,

    interrupt_mask_register: u8,
    interrupt_request_low: u8,

    icw2_expected: bool,
    icw4_expected: bool,
}

impl Default for ProgrammableInterruptController {
    fn default() -> Self {
        Self {
            initialized: false,
            interrupt_mask_register: 0xFF,
            interrupt_request_low: 0x00,
            icw2_expected: false,
            icw4_expected: false,
        }
    }
}

impl ProgrammableInterruptController {
    fn read_low(&self) -> Result<u8> {
        //todo!()
        Ok(0)
    }

    fn read_high(&self) -> Result<u8> {
        Ok(self.interrupt_mask_register)
    }

    fn write_low(&mut self, value: u8) -> Result<()> {
        log::info!("Writing to PIC low port: {:#04X}", value);

        if value & 0x10 != 0 {
            return self.write_icw1(value);
        }

        Ok(())
    }

    fn write_high(&mut self, value: u8) -> Result<()> {
        if self.icw2_expected {
            self.write_icw2(value)?;
        } else if self.icw4_expected {
            self.write_icw4(value)?;
        } else {
            // We have all the ICW's, so this must be an OCW1 write.
            log::info!("Setting interrupt mask register to: {:08b}", value);
            self.interrupt_mask_register = value;
        }

        Ok(())
    }

    /// ICW1:
    /// | D7   | D6   | D5   | D4   | D3   | D2  | D1   | D0   |
    /// | INT3 | INT1 | INT0 | INIT | LTIM | ADI | SNGL | ICW4 |
    ///
    /// D0: 1 = Initialization sequence requires that ICW4 is sent.
    /// D1: 0 = Chip is paired with another in cascade mode.
    ///     1 = Operates as a single chip.
    /// D2: 0 = Address interval of 8 bytes.
    ///     1 = Address interval of 4 bytes.
    /// D3: 0 = Edge triggered mode.
    ///     1 = Level triggered mode.
    /// D4: 1 = Chip is being initialized.
    fn write_icw1(&mut self, value: u8) -> Result<()> {
        let icw4_expected = value & 0x01 != 0;
        log::info!(
            "ICW1 received | {} | {} | {} | {}",
            if icw4_expected {
                "icw4 expected"
            } else {
                "icw4 not expected"
            },
            if value & 0x02 != 0 {
                "single mode"
            } else {
                "cascade mode"
            },
            if value & 0x04 != 0 {
                "interval of 4"
            } else {
                "interval of 8"
            },
            if value & 0x08 != 0 {
                "level triggered"
            } else {
                "edge triggered"
            }
        );

        self.initialized = false;

        self.interrupt_mask_register = 0x00;
        self.interrupt_request_low = 0x07;

        self.icw2_expected = true;
        self.icw4_expected = icw4_expected;

        Ok(())
    }

    fn write_icw2(&mut self, value: u8) -> Result<()> {
        debug_assert!(
            self.icw2_expected,
            "ICW2 with value {:#04X} not expected to be written!",
            value
        );

        log::info!("ICW2 received");

        // If bit 0, 1 and 2 are set to 0, then we are in 8086/8088 mode and can process icw2.
        if value & 0x07 == 0 {
            // TODO update isr base
            self.icw2_expected = false;
        }

        Ok(())
    }

    fn write_icw4(&mut self, value: u8) -> Result<()> {
        debug_assert!(
            self.icw4_expected,
            "ICW4 with value {:#04X} not expected to be written!",
            value
        );

        log::info!(
            "ICW4: {} | {}",
            if value & 0x01 != 0 {
                "8086/8088 mode"
            } else {
                "MCS-80/85 mode"
            },
            if value & 0x02 != 0 {
                "auto EOI"
            } else {
                "normal EOI"
            }
        );

        self.icw4_expected = false;

        Ok(())
    }
}

impl Bus<Port> for ProgrammableInterruptController {
    fn read(&self, port: u16) -> Result<u8> {
        if port & 0b1 == 0 {
            self.read_low()
        } else {
            self.read_high()
        }
    }

    fn write(&mut self, port: u16, value: u8) -> Result<()> {
        if port & 0b1 == 0 {
            self.write_low(value)
        } else {
            self.write_high(value)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_init() {
        pretty_env_logger::init();

        let mut pic = ProgrammableInterruptController::default();

        // Write ICW1 with ICW4 not expected.
        pic.write(0x0000, 0x10).unwrap();

        assert_eq!(pic.initialized, false);
        assert_eq!(pic.interrupt_mask_register, 0x00);
        assert_eq!(pic.interrupt_request_low, 0x07);
        assert_eq!(pic.icw2_expected, true);
        assert_eq!(pic.icw4_expected, false);

        let mut pic = ProgrammableInterruptController::default();

        // Write ICW1 with ICW4 being expected.
        pic.write(0x0000, 0x11).unwrap();

        assert_eq!(pic.initialized, false);
        assert_eq!(pic.interrupt_mask_register, 0x00);
        assert_eq!(pic.interrupt_request_low, 0x07);
        assert_eq!(pic.icw2_expected, true);
        assert_eq!(pic.icw4_expected, true);
    }
}
