use super::{Intel8088, Operand, BP, BX, DI, DS, SI, SS};
use crate::{segment_and_offset, Address, Bus};

pub(crate) enum ModRegRMDirection {
    RegFirst,
    RegMemFirst,
}

impl<D: Bus, I: Bus> Intel8088<D, I> {
    pub(crate) fn read_operand_byte(&self, operand: Operand) -> u8 {
        match operand {
            Operand::Register(encoding) => self.read_register_byte(encoding as usize),
            Operand::Memory(addr) => self.read_data_bus_byte(addr),
        }
    }

    pub(crate) fn read_operand_word(&self, operand: Operand) -> u16 {
        match operand {
            Operand::Register(encoding) => self.read_register_word(encoding as usize),
            Operand::Memory(addr) => self.read_data_bus_word(addr),
        }
    }

    pub(crate) fn write_operand_byte(&mut self, operand: Operand, value: u8) {
        match operand {
            Operand::Register(encoding) => self.write_register_byte(encoding as usize, value),
            Operand::Memory(addr) => self.write_data_bus_byte(addr, value),
        }
    }

    pub(crate) fn write_operand_word(&mut self, operand: Operand, value: u16) {
        match operand {
            Operand::Register(encoding) => self.write_register_word(encoding as usize, value),
            Operand::Memory(addr) => self.write_data_bus_word(addr, value),
        }
    }

    pub(crate) fn mod_reg_rm_arithmetic_byte(
        &mut self,
        op: impl super::calc::arithmetic::Operation<u8>,
        direction: ModRegRMDirection,
        reg_cycles: usize,
        mem_cycles: usize,
    ) {
        let mrrm = self.fetch();
        let (reg, rm) = self.mod_reg_rm_to_operands(mrrm);

        let (dst, src, res_op) = self.mod_reg_rm_operands_byte(reg, rm, direction);

        let result = op.op(dst, src, &mut self.flags);

        self.write_operand_byte(res_op, result);
        self.consume_cycles_for_operand(rm, reg_cycles, mem_cycles);
    }

    pub(crate) fn _mod_reg_rm_logic_byte(
        &mut self,
        op: impl super::calc::logic::Operation<u8>,
        direction: ModRegRMDirection,
        reg_cycles: usize,
        mem_cycles: usize,
    ) {
        let mrrm = self.fetch();
        let (reg, rm) = self.mod_reg_rm_to_operands(mrrm);

        let (dst, src, _) = self.mod_reg_rm_operands_byte(reg, rm, direction);

        op.op(dst, src, &mut self.flags);

        self.consume_cycles_for_operand(rm, reg_cycles, mem_cycles);
    }

    pub(crate) fn mod_reg_rm_arithmetic_word(
        &mut self,
        op: impl super::calc::arithmetic::Operation<u16>,
        direction: ModRegRMDirection,
        reg_cycles: usize,
        mem_cycles: usize,
    ) {
        let mrrm = self.fetch();
        let (reg, rm) = self.mod_reg_rm_to_operands(mrrm);

        let (dst, src, res_op) = self.mod_reg_rm_operands_word(reg, rm, direction);

        let result = op.op(dst, src, &mut self.flags);

        self.write_operand_word(res_op, result);
        self.consume_cycles_for_operand(rm, reg_cycles, mem_cycles);
    }

    pub(crate) fn mod_reg_rm_logic_word(
        &mut self,
        op: impl super::calc::logic::Operation<u16>,
        direction: ModRegRMDirection,
        reg_cycles: usize,
        mem_cycles: usize,
    ) {
        let mrrm = self.fetch();
        let (reg, rm) = self.mod_reg_rm_to_operands(mrrm);

        let (dst, src, _) = self.mod_reg_rm_operands_word(reg, rm, direction);

        op.op(dst, src, &mut self.flags);

        self.consume_cycles_for_operand(rm, reg_cycles, mem_cycles);
    }

    fn mod_reg_rm_operands_byte(
        &mut self,
        reg: Operand,
        rm: Operand,
        direction: ModRegRMDirection,
    ) -> (u8, u8, Operand) {
        match direction {
            ModRegRMDirection::RegFirst => {
                (self.read_operand_byte(reg), self.read_operand_byte(rm), reg)
            }
            ModRegRMDirection::RegMemFirst => {
                (self.read_operand_byte(rm), self.read_operand_byte(reg), rm)
            }
        }
    }

    fn mod_reg_rm_operands_word(
        &mut self,
        reg: Operand,
        rm: Operand,
        direction: ModRegRMDirection,
    ) -> (u16, u16, Operand) {
        match direction {
            ModRegRMDirection::RegFirst => {
                (self.read_operand_word(reg), self.read_operand_word(rm), reg)
            }
            ModRegRMDirection::RegMemFirst => {
                (self.read_operand_word(rm), self.read_operand_word(reg), rm)
            }
        }
    }

    pub(crate) fn mod_reg_rm_to_operands<O: From<Operand>>(&mut self, mrrm: u8) -> (O, Operand) {
        let (reg, rm) = self.mod_rm_to_operands(mrrm);
        (Operand::Register(reg).into(), rm)
    }

    pub(crate) fn mod_rm_to_operands(&mut self, mrrm: u8) -> (u8, Operand) {
        let reg = (mrrm >> 3) & 0b111;
        let rm = mrrm & 0b111;

        let rm = match mrrm >> 6 {
            0b00 => {
                if rm == 0b110 {
                    let lo = self.fetch();
                    let hi = self.fetch();

                    Operand::Memory(segment_and_offset(
                        self.segments[self.segment_override.unwrap_or(DS)],
                        u16::from_le_bytes([lo, hi]),
                    ))
                } else {
                    Operand::Memory(self.mod_reg_rm_effective_addr(rm, 0))
                }
            }

            0b01 => {
                let lo = self.fetch();
                let disp = i16::from_le_bytes([lo, 0]);
                Operand::Memory(self.mod_reg_rm_effective_addr(rm, disp))
            }

            0b10 => {
                let lo = self.fetch();
                let hi = self.fetch();
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
