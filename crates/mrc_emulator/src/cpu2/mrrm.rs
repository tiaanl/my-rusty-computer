use super::{Intel8088, Operand, BP, BX, DI, DS, SI, SS};
use crate::{segment_and_offset, Address, Bus, Port};

impl<D: Bus<Address>, I: Bus<Port>> Intel8088<D, I> {
    pub(crate) fn mod_reg_rm_to_operands(&mut self, mrrm: u8) -> (Operand, Operand) {
        let (reg, rm) = self.mod_rm_to_operands(mrrm);
        (Operand::Register(reg), rm)
    }

    pub(crate) fn mod_rm_to_operands(&mut self, mrrm: u8) -> (u8, Operand) {
        let reg = (mrrm >> 3) & 0b111;
        let rm = mrrm & 0b111;

        let rm = match mrrm >> 6 {
            0b00 => {
                if rm == 0b110 {
                    let lo = self.fetch().unwrap();
                    let hi = self.fetch().unwrap();

                    Operand::Memory(segment_and_offset(
                        self.segments[self.segment_override.unwrap_or(DS)],
                        u16::from_le_bytes([lo, hi]),
                    ))
                } else {
                    Operand::Memory(self.mod_reg_rm_effective_addr(rm, 0))
                }
            }

            0b01 => {
                let lo = self.fetch().unwrap();
                let disp = i16::from_le_bytes([lo, 0]);
                Operand::Memory(self.mod_reg_rm_effective_addr(rm, disp))
            }

            0b10 => {
                let lo = self.fetch().unwrap();
                let hi = self.fetch().unwrap();
                let disp = i16::from_le_bytes([lo, hi]);
                Operand::Memory(self.mod_reg_rm_effective_addr(rm, disp))
            }

            0b11 => Operand::Register(rm),

            _ => unreachable!(),
        };

        (reg, rm)
    }

    fn mod_reg_rm_effective_addr(&mut self, rm: u8, disp: i16) -> Address {
        let disp = disp as u16;
        match rm {
            0b000 => segment_and_offset(
                self.segments[self.segment_override.unwrap_or(DS)],
                self.registers[BX]
                    .wrapping_add(self.registers[SI])
                    .wrapping_add(disp),
            ),
            0b001 => segment_and_offset(
                self.segments[self.segment_override.unwrap_or(DS)],
                self.registers[BX]
                    .wrapping_add(self.registers[DI])
                    .wrapping_add(disp),
            ),
            0b010 => segment_and_offset(
                self.segments[self.segment_override.unwrap_or(SS)],
                self.registers[BP]
                    .wrapping_add(self.registers[SI])
                    .wrapping_add(disp),
            ),
            0b011 => segment_and_offset(
                self.segments[self.segment_override.unwrap_or(SS)],
                self.registers[BP]
                    .wrapping_add(self.registers[DI])
                    .wrapping_add(disp),
            ),
            0b100 => segment_and_offset(
                self.segments[self.segment_override.unwrap_or(DS)],
                self.registers[SI].wrapping_add(disp),
            ),
            0b101 => segment_and_offset(
                self.segments[self.segment_override.unwrap_or(DS)],
                self.registers[DI].wrapping_add(disp),
            ),
            0b110 => segment_and_offset(
                self.segments[self.segment_override.unwrap_or(SS)],
                self.registers[BP].wrapping_add(disp),
            ),
            0b111 => segment_and_offset(
                self.segments[self.segment_override.unwrap_or(DS)],
                self.registers[BX].wrapping_add(disp),
            ),

            _ => unreachable!(),
        }
    }
}
