//! Emulation of Intel 8259A programmable interrupt controller.

use crate::{error::Result, Bus, Port};

#[derive(Default)]
pub struct ProgrammableInterruptController {
    irq_base: u8,
    _delay: u8,
    icw_current: u8,
    icw: [u8; 4],
    imr: u8,
    irr: u8,
    isr: u8,
    ir_low: u8,
    ocw3: u8,
}

impl ProgrammableInterruptController {
    fn read_low(&self) -> Result<u8> {
        todo!()
    }

    fn read_high(&self) -> Result<u8> {
        Ok(self.imr)
    }

    fn write_low(&mut self, value: u8) -> Result<()> {
        log::info!("Writing to PIC low port: {:#04X}", value);

        if value >> 4 & 1 == 1 {
            // This is ocw1.
            self.icw_current = 0;
            self.icw[self.icw_current as usize] = value;
            self.icw_current += 1;

            self.imr = 0;
            self.ir_low = 7;
            self.irr = 0;
            self.isr = 0;
            self.ocw3 = 0x08 | 0x02; // TODO: This needs more explanation
        } else if value >> 3 & 0b1 == 0 {
            // This is ocw2.
            let ocw2 = value & 0xE0;
            if ocw2 & 0x20 != 0 {
                // This must be an ocw2 with an EOI command.
                let mut _ir_low = 0; // TODO: This should have a undefined state.
                let mut ir_end = 0;
                if (ocw2 & 0x60) != 0 {
                    // Specific EOI command.
                    _ir_low = value & 0x07;
                    ir_end = 1 << _ir_low;
                } else {
                    _ir_low = self.ir_low + 1;
                    loop {
                        _ir_low &= 7;
                        let ir = 1 << _ir_low;
                        if self.isr & ir != 0 {
                            ir_end = ir;
                            break;
                        }
                        if _ir_low == self.ir_low {
                            _ir_low += 1;
                            break;
                        }
                        _ir_low += 1;
                    }
                }

                let _irq = self.irq_base + _ir_low;

                if self.isr & ir_end != 0 {
                    self.isr &= !ir_end;
                    // self.check_irr();
                } else {
                    // TODO: Report some kind of error here.
                }

                if ocw2 & 0x80 != 0 {
                    todo!("Unsupported OCW2 rotate {:#04X}", ocw2)
                }
            } else if ocw2 == 0xC0 {
                // This OCW2 changes the lower priority interrupt to the specified level. The
                // default is 7.
                self.ir_low = value & 0x07;
            } else {
                todo!("Other OCW2 command not supported!")
            }
        } else {
            // This must be an OCW3 request.
            if value & (0x04 | 0x60) != 0 {
                todo!("Unsupported OCW3 {:#04X}", value)
            }
            self.ocw3 = value;
        }

        Ok(())
    }

    fn write_high(&mut self, value: u8) -> Result<()> {
        log::info!("Writing to PIC high port: {:#04X}", value);

        if self.icw_current < 4 {
            self.icw[self.icw_current as usize] = value;
            self.icw_current += 1;
            if self.icw_current == 2 && (self.icw[0] & 0x02 != 0) {
                self.icw_current += 1;
            }
            if self.icw_current == 3 && (self.icw[0] & 0x01) == 0 {
                self.icw_current += 1;
            }
        } else {
            // We have all the ICW's, so this must be an OCW1 write.
            self.imr = value;

            // TODO: Delay the cpu interrupts here?

            //self.check_irr();
        }

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
mod test {}
