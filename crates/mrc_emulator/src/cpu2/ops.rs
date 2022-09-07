use super::{calc, Flags, Intel8088, AX, CS, CX};
use crate::cpu2::Operand;
use crate::{Address, Bus, Port};

macro_rules! op {
    ($op_code:literal, $exec:ident) => {{
        OpCodeEntry { exec: Self::$exec }
    }};
}

struct OpCodeEntry<D: Bus<Address>, I: Bus<Port>> {
    exec: fn(&mut Intel8088<D, I>),
}

impl<D: Bus<Address>, I: Bus<Port>> Intel8088<D, I> {
    pub fn execute_op_code(&mut self, op_code: u8) {
        (Self::OP_CODE_TABLE[op_code as usize].exec)(self);
    }

    const OP_CODE_TABLE: [OpCodeEntry<D, I>; 0x100] = [
        op!(0x00, op_add_rm8_reg8),
        op!(0x01, op_add_rm16_reg16),
        op!(0x02, op_add_reg8_rm8),
        op!(0x03, op_add_reg16_rm16),
        op!(0x04, op_invalid),
        op!(0x05, op_invalid),
        op!(0x06, op_invalid),
        op!(0x07, op_invalid),
        op!(0x08, op_invalid),
        op!(0x09, op_invalid),
        op!(0x0A, op_invalid),
        op!(0x0B, op_invalid),
        op!(0x0C, op_invalid),
        op!(0x0D, op_invalid),
        op!(0x0E, op_invalid),
        op!(0x0F, op_invalid),
        op!(0x10, op_invalid),
        op!(0x11, op_invalid),
        op!(0x12, op_invalid),
        op!(0x13, op_invalid),
        op!(0x14, op_invalid),
        op!(0x15, op_invalid),
        op!(0x16, op_invalid),
        op!(0x17, op_invalid),
        op!(0x18, op_invalid),
        op!(0x19, op_invalid),
        op!(0x1A, op_invalid),
        op!(0x1B, op_invalid),
        op!(0x1C, op_invalid),
        op!(0x1D, op_invalid),
        op!(0x1E, op_invalid),
        op!(0x1F, op_invalid),
        op!(0x20, op_invalid),
        op!(0x21, op_invalid),
        op!(0x22, op_invalid),
        op!(0x23, op_invalid),
        op!(0x24, op_invalid),
        op!(0x25, op_invalid),
        op!(0x26, op_invalid),
        op!(0x27, op_invalid),
        op!(0x28, op_invalid),
        op!(0x29, op_invalid),
        op!(0x2A, op_invalid),
        op!(0x2B, op_invalid),
        op!(0x2C, op_invalid),
        op!(0x2D, op_invalid),
        op!(0x2E, op_invalid),
        op!(0x2F, op_invalid),
        op!(0x30, op_invalid),
        op!(0x31, op_invalid),
        op!(0x32, op_xor_reg8_rm8),
        op!(0x33, op_xor_reg16_rm16),
        op!(0x34, op_invalid),
        op!(0x35, op_invalid),
        op!(0x36, op_invalid),
        op!(0x37, op_invalid),
        op!(0x38, op_invalid),
        op!(0x39, op_invalid),
        op!(0x3A, op_invalid),
        op!(0x3B, op_invalid),
        op!(0x3C, op_invalid),
        op!(0x3D, op_invalid),
        op!(0x3E, op_invalid),
        op!(0x3F, op_invalid),
        op!(0x40, op_invalid),
        op!(0x41, op_invalid),
        op!(0x42, op_invalid),
        op!(0x43, op_invalid),
        op!(0x44, op_invalid),
        op!(0x45, op_invalid),
        op!(0x46, op_invalid),
        op!(0x47, op_invalid),
        op!(0x48, op_invalid),
        op!(0x49, op_invalid),
        op!(0x4A, op_invalid),
        op!(0x4B, op_invalid),
        op!(0x4C, op_invalid),
        op!(0x4D, op_invalid),
        op!(0x4E, op_invalid),
        op!(0x4F, op_invalid),
        op!(0x50, op_invalid),
        op!(0x51, op_invalid),
        op!(0x52, op_invalid),
        op!(0x53, op_invalid),
        op!(0x54, op_invalid),
        op!(0x55, op_invalid),
        op!(0x56, op_invalid),
        op!(0x57, op_invalid),
        op!(0x58, op_invalid),
        op!(0x59, op_invalid),
        op!(0x5A, op_invalid),
        op!(0x5B, op_invalid),
        op!(0x5C, op_invalid),
        op!(0x5D, op_invalid),
        op!(0x5E, op_invalid),
        op!(0x5F, op_invalid),
        op!(0x60, op_invalid),
        op!(0x61, op_invalid),
        op!(0x62, op_invalid),
        op!(0x63, op_invalid),
        op!(0x64, op_invalid),
        op!(0x65, op_invalid),
        op!(0x66, op_invalid),
        op!(0x67, op_invalid),
        op!(0x68, op_invalid),
        op!(0x69, op_invalid),
        op!(0x6A, op_invalid),
        op!(0x6B, op_invalid),
        op!(0x6C, op_invalid),
        op!(0x6D, op_invalid),
        op!(0x6E, op_invalid),
        op!(0x6F, op_invalid),
        op!(0x70, op_jo_imm8),
        op!(0x71, op_jno_imm8),
        op!(0x72, op_jb_imm8),
        op!(0x73, op_jnb_imm8),
        op!(0x74, op_je_imm8),
        op!(0x75, op_jne_imm8),
        op!(0x76, op_jbe_imm8),
        op!(0x77, op_jnbe_imm8),
        op!(0x78, op_js_imm8),
        op!(0x79, op_jns_imm8),
        op!(0x7A, op_jp_imm8),
        op!(0x7B, op_jnp_imm8),
        op!(0x7C, op_jl_imm8),
        op!(0x7D, op_jnl_imm8),
        op!(0x7E, op_jle_imm8),
        op!(0x7F, op_jnle_imm8),
        op!(0x80, op_invalid),
        op!(0x81, op_invalid),
        op!(0x82, op_invalid),
        op!(0x83, op_invalid),
        op!(0x84, op_invalid),
        op!(0x85, op_invalid),
        op!(0x86, op_invalid),
        op!(0x87, op_invalid),
        op!(0x88, op_invalid),
        op!(0x89, op_invalid),
        op!(0x8A, op_invalid),
        op!(0x8B, op_mov_reg16_rm16),
        op!(0x8C, op_mov_rm16_seg),
        op!(0x8D, op_invalid),
        op!(0x8E, op_mov_seg_rm16),
        op!(0x8F, op_invalid),
        op!(0x90, op_invalid),
        op!(0x91, op_invalid),
        op!(0x92, op_invalid),
        op!(0x93, op_invalid),
        op!(0x94, op_invalid),
        op!(0x95, op_invalid),
        op!(0x96, op_invalid),
        op!(0x97, op_invalid),
        op!(0x98, op_invalid),
        op!(0x99, op_invalid),
        op!(0x9A, op_invalid),
        op!(0x9B, op_invalid),
        op!(0x9C, op_invalid),
        op!(0x9D, op_invalid),
        op!(0x9E, op_sahf),
        op!(0x9F, op_lahf),
        op!(0xA0, op_invalid),
        op!(0xA1, op_invalid),
        op!(0xA2, op_invalid),
        op!(0xA3, op_invalid),
        op!(0xA4, op_invalid),
        op!(0xA5, op_invalid),
        op!(0xA6, op_invalid),
        op!(0xA7, op_invalid),
        op!(0xA8, op_invalid),
        op!(0xA9, op_invalid),
        op!(0xAA, op_invalid),
        op!(0xAB, op_invalid),
        op!(0xAC, op_invalid),
        op!(0xAD, op_invalid),
        op!(0xAE, op_invalid),
        op!(0xAF, op_invalid),
        op!(0xB0, op_mov_al_imm8),
        op!(0xB1, op_mov_cl_imm8),
        op!(0xB2, op_mov_dl_imm8),
        op!(0xB3, op_mov_bl_imm8),
        op!(0xB4, op_mov_ah_imm8),
        op!(0xB5, op_mov_ch_imm8),
        op!(0xB6, op_mov_dh_imm8),
        op!(0xB7, op_mov_bh_imm8),
        op!(0xB8, op_mov_ax_imm16),
        op!(0xB9, op_mov_cx_imm16),
        op!(0xBA, op_mov_dx_imm16),
        op!(0xBB, op_mov_bx_imm16),
        op!(0xBC, op_mov_sp_imm16),
        op!(0xBD, op_mov_bp_imm16),
        op!(0xBE, op_mov_si_imm16),
        op!(0xBF, op_mov_di_imm16),
        op!(0xC0, op_invalid),
        op!(0xC1, op_invalid),
        op!(0xC2, op_invalid),
        op!(0xC3, op_invalid),
        op!(0xC4, op_invalid),
        op!(0xC5, op_invalid),
        op!(0xC6, op_invalid),
        op!(0xC7, op_invalid),
        op!(0xC8, op_invalid),
        op!(0xC9, op_invalid),
        op!(0xCA, op_invalid),
        op!(0xCB, op_invalid),
        op!(0xCC, op_invalid),
        op!(0xCD, op_invalid),
        op!(0xCE, op_invalid),
        op!(0xCF, op_invalid),
        op!(0xD0, op_group_d0_rm8_1),
        op!(0xD1, op_invalid),
        op!(0xD2, op_group_d0_rm8_cl),
        op!(0xD3, op_invalid),
        op!(0xD4, op_invalid),
        op!(0xD5, op_invalid),
        op!(0xD6, op_invalid),
        op!(0xD7, op_invalid),
        op!(0xD8, op_invalid),
        op!(0xD9, op_invalid),
        op!(0xDA, op_invalid),
        op!(0xDB, op_invalid),
        op!(0xDC, op_invalid),
        op!(0xDD, op_invalid),
        op!(0xDE, op_invalid),
        op!(0xDF, op_invalid),
        op!(0xE0, op_invalid),
        op!(0xE1, op_invalid),
        op!(0xE2, op_invalid),
        op!(0xE3, op_invalid),
        op!(0xE4, op_invalid),
        op!(0xE5, op_invalid),
        op!(0xE6, op_invalid),
        op!(0xE7, op_invalid),
        op!(0xE8, op_invalid),
        op!(0xE9, op_invalid),
        op!(0xEA, op_jmp_seg_off),
        op!(0xEB, op_invalid),
        op!(0xEC, op_invalid),
        op!(0xED, op_invalid),
        op!(0xEE, op_invalid),
        op!(0xEF, op_invalid),
        op!(0xF0, op_invalid),
        op!(0xF1, op_invalid),
        op!(0xF2, op_invalid),
        op!(0xF3, op_invalid),
        op!(0xF4, op_hlt),
        op!(0xF5, op_invalid),
        op!(0xF6, op_invalid),
        op!(0xF7, op_invalid),
        op!(0xF8, op_clc),
        op!(0xF9, op_stc),
        op!(0xFA, op_cli),
        op!(0xFB, op_invalid),
        op!(0xFC, op_invalid),
        op!(0xFD, op_invalid),
        op!(0xFE, op_invalid),
        op!(0xFF, op_invalid),
    ];

    // 00
    fn op_add_rm8_reg8(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (src_op, dst_op) = self.mod_reg_rm_to_operands(mrrm);

        let dst = self.read_operand_byte(dst_op);
        let src = self.read_operand_byte(src_op);

        let dst = Self::add_byte(dst, src, &mut self.flags);
        self.write_operand_byte(dst_op, dst);
    }

    // 01
    fn op_add_rm16_reg16(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (src_op, dst_op) = self.mod_reg_rm_to_operands(mrrm);

        let dst = self.read_operand_word(dst_op);
        let src = self.read_operand_word(src_op);

        let dst = Self::add_word(dst, src, &mut self.flags);
        self.write_operand_word(dst_op, dst);
    }

    // 02
    fn op_add_reg8_rm8(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (dst_op, src_op) = self.mod_reg_rm_to_operands(mrrm);

        let dst = self.read_operand_byte(dst_op);
        let src = self.read_operand_byte(src_op);

        let dst = Self::add_byte(dst, src, &mut self.flags);
        self.write_operand_byte(dst_op, dst);
    }

    // 03
    fn op_add_reg16_rm16(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (dst_op, src_op) = self.mod_reg_rm_to_operands(mrrm);

        let dst = self.read_operand_word(dst_op);
        let src = self.read_operand_word(src_op);

        let dst = Self::add_word(dst, src, &mut self.flags);
        self.write_operand_word(dst_op, dst);
    }

    // 32
    fn op_xor_reg8_rm8(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (dst_op, src_op) = self.mod_reg_rm_to_operands(mrrm);

        let dst = self.read_operand_byte(dst_op);
        let src = self.read_operand_byte(src_op);

        let result = dst ^ src;

        self.write_operand_byte(dst_op, result);

        self.flags.set(Flags::CARRY, false);
        self.flags.set(Flags::OVERFLOW, false);
        calc::flags_from_value_byte(result, &mut self.flags);

        self.consume_cycles(match src_op {
            Operand::Register(..) => 3,
            Operand::Memory(..) => 9,
        } as usize);
    }

    // 33
    fn op_xor_reg16_rm16(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (dst_op, src_op) = self.mod_reg_rm_to_operands(mrrm);

        let dst = self.read_operand_word(dst_op);
        let src = self.read_operand_word(src_op);

        let result = dst ^ src;

        self.write_operand_word(dst_op, result);

        self.flags.set(Flags::CARRY, false);
        self.flags.set(Flags::OVERFLOW, false);
        calc::flags_from_value_word(result, &mut self.flags);

        self.consume_cycles(match src_op {
            Operand::Register(..) => 3,
            Operand::Memory(..) => 9,
        } as usize);
    }

    // 70
    fn op_jo_imm8(&mut self) {
        self.jump_if(|f| f.contains(Flags::OVERFLOW));
    }

    // 71
    fn op_jno_imm8(&mut self) {
        self.jump_if(|f| !f.contains(Flags::OVERFLOW));
    }

    // 72
    fn op_jb_imm8(&mut self) {
        self.jump_if(|f| f.contains(Flags::CARRY));
    }

    // 73
    fn op_jnb_imm8(&mut self) {
        // CF=0
        self.jump_if(|f| !f.contains(Flags::CARRY));
    }

    // 74
    fn op_je_imm8(&mut self) {
        self.jump_if(|f| f.contains(Flags::ZERO));
    }

    // 75
    fn op_jne_imm8(&mut self) {
        self.jump_if(|f| !f.contains(Flags::ZERO));
    }

    // 76
    fn op_jbe_imm8(&mut self) {
        self.jump_if(|f| f.contains(Flags::CARRY) || f.contains(Flags::ZERO));
    }

    // 77
    fn op_jnbe_imm8(&mut self) {
        // CF=0 and ZF=0
        self.jump_if(|f| !f.contains(Flags::CARRY) && !f.contains(Flags::ZERO));
    }

    // 78
    fn op_js_imm8(&mut self) {
        self.jump_if(|f| f.contains(Flags::SIGN));
    }

    // 79
    fn op_jns_imm8(&mut self) {
        self.jump_if(|f| !f.contains(Flags::SIGN));
    }

    // 7A
    fn op_jp_imm8(&mut self) {
        self.jump_if(|f| f.contains(Flags::PARITY));
    }

    // 7B
    fn op_jnp_imm8(&mut self) {
        self.jump_if(|f| !f.contains(Flags::PARITY));
    }

    // 7C
    fn op_jl_imm8(&mut self) {
        self.jump_if(|f| f.contains(Flags::SIGN) != f.contains(Flags::OVERFLOW));
    }

    // 7D
    fn op_jnl_imm8(&mut self) {
        self.jump_if(|f| f.contains(Flags::SIGN) == f.contains(Flags::OVERFLOW));
    }

    // 7E
    fn op_jle_imm8(&mut self) {
        self.jump_if(|f| {
            f.contains(Flags::ZERO) || (f.contains(Flags::SIGN) != f.contains(Flags::OVERFLOW))
        });
    }

    // 7F
    fn op_jnle_imm8(&mut self) {
        self.jump_if(|f| {
            !f.contains(Flags::ZERO) && (f.contains(Flags::SIGN) == f.contains(Flags::OVERFLOW))
        });
    }

    fn op_mov_reg16_rm16(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (dst_op, src_op) = self.mod_reg_rm_to_operands(mrrm);

        let src = self.read_operand_word(src_op);
        self.write_operand_word(dst_op, src);

        self.consume_cycles(match src_op {
            Operand::Register(..) => 2,
            Operand::Memory(..) => 8,
        });
    }

    // 8C
    fn op_mov_rm16_seg(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (dst_op, src_op) = self.mod_reg_rm_to_operands(mrrm);

        let seg = if let Operand::Register(encoding) = src_op {
            encoding as usize
        } else {
            unreachable!("Invalid segment encoding");
        };

        let src = self.segments[seg];

        self.write_operand_word(dst_op, src);

        self.consume_cycles(match src_op {
            Operand::Register(..) => 2,
            Operand::Memory(..) => 9,
        });
    }

    // 8E
    fn op_mov_seg_rm16(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (dst_op, src_op) = self.mod_reg_rm_to_operands(mrrm);

        let seg = if let Operand::Register(encoding) = dst_op {
            encoding as usize
        } else {
            unreachable!("Invalid segment encoding");
        };

        let src = self.read_operand_word(src_op);

        self.segments[seg] = src;

        self.consume_cycles(match src_op {
            Operand::Register(..) => 2,
            Operand::Memory(..) => 8,
        });
    }

    // 9E
    fn op_sahf(&mut self) {
        let flags = Flags::from_bits_truncate(self.registers[AX] >> 8);

        self.flags.set(Flags::CARRY, flags.contains(Flags::CARRY));
        self.flags.set(Flags::PARITY, flags.contains(Flags::PARITY));
        self.flags
            .set(Flags::AUX_CARRY, flags.contains(Flags::AUX_CARRY));
        self.flags.set(Flags::ZERO, flags.contains(Flags::ZERO));
        self.flags.set(Flags::SIGN, flags.contains(Flags::SIGN));

        self.consume_cycles(4);
    }

    // 9F
    fn op_lahf(&mut self) {
        let flags = self.flags
            & (Flags::CARRY | Flags::PARITY | Flags::AUX_CARRY | Flags::ZERO | Flags::SIGN);

        self.write_register_byte(0x04, flags.bits as u8);

        self.consume_cycles(4);
    }

    // B0
    fn op_mov_al_imm8(&mut self) {
        self.mov_reg8_imm8(0x00);
    }

    // B1
    fn op_mov_cl_imm8(&mut self) {
        self.mov_reg8_imm8(0x01);
    }

    // B2
    fn op_mov_dl_imm8(&mut self) {
        self.mov_reg8_imm8(0x02);
    }

    // B3
    fn op_mov_bl_imm8(&mut self) {
        self.mov_reg8_imm8(0x03);
    }

    // B4
    fn op_mov_ah_imm8(&mut self) {
        self.mov_reg8_imm8(0x04);
    }

    // B5
    fn op_mov_ch_imm8(&mut self) {
        self.mov_reg8_imm8(0x05);
    }

    // B6
    fn op_mov_dh_imm8(&mut self) {
        self.mov_reg8_imm8(0x06);
    }

    // B7
    fn op_mov_bh_imm8(&mut self) {
        self.mov_reg8_imm8(0x07);
    }

    // B8
    fn op_mov_ax_imm16(&mut self) {
        self.mov_reg16_imm16(0x00);
    }

    // B9
    fn op_mov_cx_imm16(&mut self) {
        self.mov_reg16_imm16(0x01);
    }

    // BA
    fn op_mov_dx_imm16(&mut self) {
        self.mov_reg16_imm16(0x02);
    }

    // BB
    fn op_mov_bx_imm16(&mut self) {
        self.mov_reg16_imm16(0x03);
    }

    // BC
    fn op_mov_sp_imm16(&mut self) {
        self.mov_reg16_imm16(0x04);
    }

    // BD
    fn op_mov_bp_imm16(&mut self) {
        self.mov_reg16_imm16(0x05);
    }

    // BE
    fn op_mov_si_imm16(&mut self) {
        self.mov_reg16_imm16(0x06);
    }

    // BF
    fn op_mov_di_imm16(&mut self) {
        self.mov_reg16_imm16(0x07);
    }

    // D0
    fn op_group_d0_rm8_1(&mut self) {
        self.op_group_d0_byte(1);
    }

    // D2
    fn op_group_d0_rm8_cl(&mut self) {
        let cl = self.registers[CX] as u8;
        self.op_group_d0_byte(cl);
    }

    // EA
    fn op_jmp_seg_off(&mut self) {
        let lo = self.fetch().unwrap();
        let hi = self.fetch().unwrap();
        let offset = u16::from_le_bytes([lo, hi]);

        let lo = self.fetch().unwrap();
        let hi = self.fetch().unwrap();
        let segment = u16::from_le_bytes([lo, hi]);

        self.ip = offset;
        self.segments[CS] = segment;

        self.consume_cycles(15);
    }

    // F4
    fn op_hlt(&mut self) {
        self.consume_cycles(2);
        self.halted = true;
    }

    // F8
    fn op_clc(&mut self) {
        self.flags.set(Flags::CARRY, false);

        self.consume_cycles(2);
    }

    // F9
    fn op_stc(&mut self) {
        self.flags.set(Flags::CARRY, true);

        self.consume_cycles(2);
    }

    // FA
    fn op_cli(&mut self) {
        self.flags.set(Flags::INTERRUPT, false);

        self.consume_cycles(2);
    }

    fn op_invalid(&mut self) {
        if cfg!(debug_assertions) {
            todo!("invalid op_code {:02X}", self.last_op_code)
        }
    }
}

// Helpers
impl<D: Bus<Address>, I: Bus<Port>> Intel8088<D, I> {
    // 70..7F
    fn jump_if(&mut self, condition: fn(Flags) -> bool) {
        let rel = self.fetch().unwrap() as i8;

        if condition(self.flags) {
            self.ip = self.ip.wrapping_add(rel as u16);
            self.consume_cycles(16);
        } else {
            self.consume_cycles(4);
        }
    }

    // B0..B7
    fn mov_reg8_imm8(&mut self, reg: u8) {
        let value = self.fetch().unwrap();

        self.write_register_byte(reg, value);

        self.consume_cycles(4);
    }

    // B8..BF
    fn mov_reg16_imm16(&mut self, reg: u8) {
        let lo = self.fetch().unwrap();
        let hi = self.fetch().unwrap();
        let value = u16::from_le_bytes([lo, hi]);

        self.write_register_word(reg, value);

        self.consume_cycles(4);
    }

    // D0, D2
    fn op_group_d0_byte(&mut self, count: u8) {
        let mrrm = self.fetch().unwrap();
        let (op, src) = self.mod_rm_to_operands(mrrm);

        if count == 0 {
            // 8 for register
            // 20 for mem
            self.consume_cycles(20);
            return;
        }

        let value = self.read_operand_byte(src);

        let result = match op {
            0b000 => calc::rol(value, count, &mut self.flags),
            0b001 => calc::ror(value, count, &mut self.flags),
            0b010 => calc::rcl(value, count, &mut self.flags),
            0b011 => calc::rcr(value, count, &mut self.flags),
            0b100 => calc::shl(value, count, &mut self.flags),
            0b101 => calc::shr(value, count, &mut self.flags),
            // 0b110,
            0b111 => calc::sar(value, count, &mut self.flags),

            _ => unreachable!(),
        };

        self.flags
            .set(Flags::OVERFLOW, ((result ^ value) & 0x80) != 0);

        self.write_operand_byte(src, result);

        self.consume_cycles(match src {
            Operand::Register(..) => 8 + 4 * count,
            Operand::Memory(..) => 20 + 4 * count,
        } as usize);
    }

    // D1, D3
    fn _op_group_d0_word(&mut self, _count: u8) {
        todo!()
    }
}
