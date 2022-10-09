use super::calc::arithmetic::{arithmetic, Add, Subtract};
use super::{Flags, Intel8088, Operand, AX, BX, CS, CX, DX};
use crate::cpu2::calc::arithmetic::{
    And, ExclusiveOr, Or, RotateLeft, RotateLeftWithCarry, RotateRight, RotateRightWithCarry,
    ShiftArithmeticRight, ShiftLeft, ShiftRight,
};
use crate::cpu2::calc::logic::{logic, Compare, Test};
use crate::cpu2::mrrm::ModRegRMDirection::{RegFirst, RegMemFirst};
use crate::cpu2::{BP, DI, DS, ES, SI, SP, SS};
use crate::{segment_and_offset, Address, Bus, Port};
use std::ops::Not;

macro_rules! op {
    ($op_code:literal, $exec:ident) => {{
        OpCodeEntry { exec: Self::$exec }
    }};
}

type OpFunc<D, I> = fn(&mut Intel8088<D, I>);

struct OpCodeEntry<D: Bus<Address>, I: Bus<Port>> {
    exec: OpFunc<D, I>,
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
        op!(0x04, op_add_al_imm8),
        op!(0x05, op_invalid),
        op!(0x06, op_invalid),
        op!(0x07, op_invalid),
        op!(0x08, op_invalid),
        op!(0x09, op_invalid),
        op!(0x0A, op_or_reg8_rm8),
        op!(0x0B, op_or_reg16_rm16),
        op!(0x0C, op_invalid),
        op!(0x0D, op_invalid),
        op!(0x0E, op_push_cs),
        op!(0x0F, op_invalid),
        op!(0x10, op_invalid),
        op!(0x11, op_invalid),
        op!(0x12, op_invalid),
        op!(0x13, op_invalid),
        op!(0x14, op_invalid),
        op!(0x15, op_invalid),
        op!(0x16, op_invalid),
        op!(0x17, op_pop_ss),
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
        op!(0x22, op_and_reg8_rm8),
        op!(0x23, op_invalid),
        op!(0x24, op_and_al_imm8),
        op!(0x25, op_invalid),
        op!(0x26, op_invalid),
        op!(0x27, op_invalid),
        op!(0x28, op_invalid),
        op!(0x29, op_invalid),
        op!(0x2A, op_sub_reg8_rm8),
        op!(0x2B, op_sub_reg16_rm16),
        op!(0x2C, op_invalid),
        op!(0x2D, op_invalid),
        op!(0x2E, op_invalid), // CS segment prefix
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
        op!(0x3B, op_cmp_reg16_rm16),
        op!(0x3C, op_invalid),
        op!(0x3D, op_invalid),
        op!(0x3E, op_invalid),
        op!(0x3F, op_invalid),
        op!(0x40, op_inc_ax),
        op!(0x41, op_inc_cx),
        op!(0x42, op_inc_dx),
        op!(0x43, op_inc_bx),
        op!(0x44, op_inc_sp),
        op!(0x45, op_inc_bp),
        op!(0x46, op_inc_si),
        op!(0x47, op_inc_di),
        op!(0x48, op_dec_ax),
        op!(0x49, op_dec_cx),
        op!(0x4A, op_dec_dx),
        op!(0x4B, op_dec_bx),
        op!(0x4C, op_dec_sp),
        op!(0x4D, op_dec_bp),
        op!(0x4E, op_dec_si),
        op!(0x4F, op_dec_di),
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
        op!(0x80, op_xxx_rm8_imm8),
        op!(0x81, op_xxx_rm16_imm16),
        op!(0x82, op_invalid),
        op!(0x83, op_xxx_rm16_imm8),
        op!(0x84, op_invalid),
        op!(0x85, op_invalid),
        op!(0x86, op_invalid),
        op!(0x87, op_invalid),
        op!(0x88, op_mov_rm8_reg8),
        op!(0x89, op_mov_rm16_reg16),
        op!(0x8A, op_mov_reg8_rm8),
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
        op!(0xAA, op_stosb),
        op!(0xAB, op_stosw),
        op!(0xAC, op_lodsb),
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
        op!(0xC3, op_ret),
        op!(0xC4, op_invalid),
        op!(0xC5, op_invalid),
        op!(0xC6, op_mov_rm8_imm8),
        op!(0xC7, op_mov_rm16_imm16),
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
        op!(0xD3, op_group_d0_rm16_cl),
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
        op!(0xE1, op_loopz_imm8),
        op!(0xE2, op_loop_imm8),
        op!(0xE3, op_invalid),
        op!(0xE4, op_in_al_imm8),
        op!(0xE5, op_invalid),
        op!(0xE6, op_out_imm8_al),
        op!(0xE7, op_invalid),
        op!(0xE8, op_call_imm16),
        op!(0xE9, op_jmp_imm16),
        op!(0xEA, op_jmp_seg_off),
        op!(0xEB, op_invalid),
        op!(0xEC, op_in_al_dx),
        op!(0xED, op_in_ax_dx),
        op!(0xEE, op_out_dx_al),
        op!(0xEF, op_invalid),
        op!(0xF0, op_invalid),
        op!(0xF1, op_invalid),
        op!(0xF2, op_invalid),
        op!(0xF3, op_rep),
        op!(0xF4, op_hlt),
        op!(0xF5, op_invalid),
        op!(0xF6, op_xxx_rm8),
        op!(0xF7, op_invalid),
        op!(0xF8, op_clc),
        op!(0xF9, op_stc),
        op!(0xFA, op_cli),
        op!(0xFB, op_sti),
        op!(0xFC, op_cld),
        op!(0xFD, op_std),
        op!(0xFE, op_inc_dec_rm8),
        op!(0xFF, op_invalid),
    ];

    // 00
    fn op_add_rm8_reg8(&mut self) {
        self.mod_reg_rm_arithmetic_byte(Add, RegMemFirst, 3, 16);
    }

    // 01
    fn op_add_rm16_reg16(&mut self) {
        self.mod_reg_rm_arithmetic_word(Add, RegMemFirst, 3, 16);
    }

    // 02
    fn op_add_reg8_rm8(&mut self) {
        self.mod_reg_rm_arithmetic_byte(Add, RegFirst, 3, 9);
    }

    // 03
    fn op_add_reg16_rm16(&mut self) {
        self.mod_reg_rm_arithmetic_word(Add, RegFirst, 3, 9);
    }

    // 04
    fn op_add_al_imm8(&mut self) {
        let al = self.read_register_byte(AX);
        let imm = self.fetch().unwrap();

        let result = arithmetic(Add, al, imm, &mut self.flags);

        self.write_register_byte(AX, result);

        self.consume_cycles(4);
    }

    // 0A
    fn op_or_reg8_rm8(&mut self) {
        self.mod_reg_rm_arithmetic_byte(Or, RegFirst, 3, 9);
    }

    // 0B
    fn op_or_reg16_rm16(&mut self) {
        self.mod_reg_rm_arithmetic_word(Or, RegFirst, 3, 9);
    }

    // 0E
    fn op_push_cs(&mut self) {
        self._push(self.segments[CS]);
        self.consume_cycles(10);
    }

    // 17
    fn op_pop_ss(&mut self) {
        self.segments[SS] = self.pop();

        self.consume_cycles(8);
    }

    // 22
    fn op_and_reg8_rm8(&mut self) {
        self.mod_reg_rm_arithmetic_byte(And, RegFirst, 3, 9);
    }

    // 24
    fn op_and_al_imm8(&mut self) {
        let al = self.read_register_byte(AX);
        let imm = self.fetch().unwrap();

        let result = arithmetic(And, al, imm, &mut self.flags);

        self.write_register_byte(AX, result);

        self.consume_cycles(4);
    }

    // 2A
    fn op_sub_reg8_rm8(&mut self) {
        self.mod_reg_rm_arithmetic_byte(Subtract, RegFirst, 3, 9);
    }

    // 2B
    fn op_sub_reg16_rm16(&mut self) {
        self.mod_reg_rm_arithmetic_word(Subtract, RegFirst, 3, 9);
    }

    // 32
    fn op_xor_reg8_rm8(&mut self) {
        self.mod_reg_rm_arithmetic_byte(ExclusiveOr, RegFirst, 3, 9);
    }

    // 33
    fn op_xor_reg16_rm16(&mut self) {
        self.mod_reg_rm_arithmetic_word(ExclusiveOr, RegFirst, 3, 9);
    }

    // 3B
    fn op_cmp_reg16_rm16(&mut self) {
        self.mod_reg_rm_logic_word(Compare, RegFirst, 3, 9);
    }

    // 40
    fn op_inc_ax(&mut self) {
        self.inc_reg16(AX);
    }

    // 41
    fn op_inc_cx(&mut self) {
        self.inc_reg16(CX);
    }

    // 42
    fn op_inc_dx(&mut self) {
        self.inc_reg16(DX);
    }

    // 43
    fn op_inc_bx(&mut self) {
        self.inc_reg16(BX);
    }

    // 44
    fn op_inc_sp(&mut self) {
        self.inc_reg16(SP);
    }

    // 45
    fn op_inc_bp(&mut self) {
        self.inc_reg16(BP);
    }

    // 46
    fn op_inc_si(&mut self) {
        self.inc_reg16(SI);
    }

    // 47
    fn op_inc_di(&mut self) {
        self.inc_reg16(DI);
    }

    // 48
    fn op_dec_ax(&mut self) {
        self.dec_reg16(DI);
    }

    // 49
    fn op_dec_cx(&mut self) {
        self.dec_reg16(DI);
    }

    // 4A
    fn op_dec_dx(&mut self) {
        self.dec_reg16(DI);
    }

    // 4B
    fn op_dec_bx(&mut self) {
        self.dec_reg16(DI);
    }

    // 4C
    fn op_dec_sp(&mut self) {
        self.dec_reg16(DI);
    }

    // 4D
    fn op_dec_bp(&mut self) {
        self.dec_reg16(DI);
    }

    // 4E
    fn op_dec_si(&mut self) {
        self.dec_reg16(DI);
    }

    // 4F
    fn op_dec_di(&mut self) {
        self.dec_reg16(DI);
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

    // 80
    fn op_xxx_rm8_imm8(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (xop, dst_op) = self.mod_reg_rm_to_operands::<u8>(mrrm);

        let dst = self.read_operand_byte(dst_op);

        // Sign extended byte into word.
        let src = self.fetch().unwrap();

        let (reg_cycles, mem_cycles) = if xop == 0b111 {
            logic(Compare, dst, src, &mut self.flags);
            (4, 10)
        } else {
            let (result, reg_cycles, mem_cycles) = match xop {
                0b000 => (arithmetic(Add, dst, src, &mut self.flags), 4, 17),
                0b001 => (arithmetic(Or, dst, src, &mut self.flags), 4, 17),
                0b010 => todo!(), // (cpu_arith::byte::adc(dst, src, &mut self.flags), 4, 17),
                0b011 => todo!(), // (cpu_arith::byte::sbb(dst, src, &mut self.flags), 4, 17),
                0b100 => todo!(), // (cpu_arith::byte::and(dst, src, &mut self.flags), 4, 17),
                0b101 => (arithmetic(Subtract, dst, src, &mut self.flags), 4, 17),
                0b110 => (arithmetic(ExclusiveOr, dst, src, &mut self.flags), 4, 17),
                // 0b111 - already hadled above
                _ => unreachable!(),
            };

            self.write_operand_byte(dst_op, result);

            (reg_cycles, mem_cycles)
        };

        self.consume_cycles_for_operand(dst_op, reg_cycles, mem_cycles);
    }

    // 81
    fn op_xxx_rm16_imm16(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (xop, dst_op) = self.mod_reg_rm_to_operands::<u8>(mrrm);

        let dst = self.read_operand_word(dst_op);

        let lo = self.fetch().unwrap();
        let hi = self.fetch().unwrap();
        let src = u16::from_le_bytes([lo, hi]);

        let (reg_cycles, mem_cycles) = if xop == 0b111 {
            logic(Compare, dst, src, &mut self.flags);
            (4, 10)
        } else {
            let (result, reg_cycles, mem_cycles) = match xop {
                0b000 => (arithmetic(Add, dst, src, &mut self.flags), 4, 17),
                0b001 => (arithmetic(Or, dst, src, &mut self.flags), 4, 17),
                0b010 => todo!(), // (cpu_arith::byte::adc(dst, src, &mut self.flags), 4, 17),
                0b011 => todo!(), // (cpu_arith::byte::sbb(dst, src, &mut self.flags), 4, 17),
                0b100 => todo!(), // (cpu_arith::byte::and(dst, src, &mut self.flags), 4, 17),
                0b101 => (arithmetic(Subtract, dst, src, &mut self.flags), 4, 17),
                0b110 => (arithmetic(ExclusiveOr, dst, src, &mut self.flags), 4, 17),
                // 0b111 - already hadled above
                _ => unreachable!(),
            };

            self.write_operand_word(dst_op, result);

            (reg_cycles, mem_cycles)
        };

        self.consume_cycles_for_operand(dst_op, reg_cycles, mem_cycles);
    }

    // 83
    fn op_xxx_rm16_imm8(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (xop, rm) = self.mod_reg_rm_to_operands::<u8>(mrrm);

        let dst = self.read_operand_word(rm);

        // Sign extended byte into word.
        let src = self.fetch().unwrap() as u16;
        let src = if src & 0x80 == 0x80 {
            src | 0xFF00
        } else {
            src & 0xFF
        };

        let (result, reg_cycles, mem_cycles) = match xop {
            0b000 => (arithmetic(Add, dst, src, &mut self.flags), 4, 17),
            0b001 => (arithmetic(Or, dst, src, &mut self.flags), 4, 17),
            0b010 => todo!(), // (cpu_arith::word::adc(dst, src, &mut self.flags), 4, 17),
            0b011 => todo!(), // (cpu_arith::word::sbb(dst, src, &mut self.flags), 4, 17),
            0b100 => todo!(), // (cpu_arith::word::and(dst, src, &mut self.flags), 4, 17),
            0b101 => (arithmetic(Subtract, dst, src, &mut self.flags), 4, 17),
            0b110 => (arithmetic(ExclusiveOr, dst, src, &mut self.flags), 4, 17),
            // 0b111 => (cpu_arith::word::cmp(dst, src, &mut self.flags), 4, 10),
            _ => unreachable!(),
        };

        self.write_operand_word(rm, result);

        self.consume_cycles_for_operand(rm, reg_cycles, mem_cycles);
    }

    // 88
    fn op_mov_rm8_reg8(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (reg, rm) = self.mod_reg_rm_to_operands(mrrm);

        let src = self.read_operand_byte(reg);
        self.write_operand_byte(rm, src);

        self.consume_cycles_for_operand(rm, 2, 9);
    }

    // 89
    fn op_mov_rm16_reg16(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (reg, rm) = self.mod_reg_rm_to_operands(mrrm);

        let src = self.read_operand_word(reg);
        self.write_operand_word(rm, src);

        self.consume_cycles_for_operand(rm, 2, 9);
    }

    // 8A
    fn op_mov_reg8_rm8(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (reg, rm) = self.mod_reg_rm_to_operands(mrrm);

        let src = self.read_operand_byte(rm);
        self.write_operand_byte(reg, src);
        self.consume_cycles_for_operand(rm, 2, 8);
    }

    // 8B
    fn op_mov_reg16_rm16(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (reg, rm) = self.mod_reg_rm_to_operands(mrrm);

        let src = self.read_operand_word(rm);
        self.write_operand_word(reg, src);
        self.consume_cycles_for_operand(rm, 2, 8);
    }

    // 8C
    fn op_mov_rm16_seg(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (seg, rm) = self.mod_reg_rm_to_operands::<usize>(mrrm);

        let src = self.segments[seg];
        self.write_operand_word(rm, src);
        self.consume_cycles_for_operand(rm, 2, 9);
    }

    // 8E
    fn op_mov_seg_rm16(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (seg, rm) = self.mod_reg_rm_to_operands::<usize>(mrrm);

        let src = self.read_operand_word(rm);
        self.segments[seg] = src;
        self.consume_cycles_for_operand(rm, 2, 8);
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

    // AA
    fn op_stosb(&mut self) {
        let es = self.segments[ES];
        let di = self.registers[DI];

        let al = self.read_register_byte(AX);

        self.data_bus.write(segment_and_offset(es, di), al).unwrap();

        let di = if self.flags.contains(Flags::DIRECTION) {
            di.wrapping_add(1)
        } else {
            di.wrapping_sub(1)
        };
        self.write_register_word(DI, di);

        self.consume_cycles(11);
    }

    // AB
    fn op_stosw(&mut self) {
        let es = self.segments[ES];
        let di = self.registers[DI];

        let ax = self.read_register_word(AX);

        let [lo, hi] = ax.to_le_bytes();
        self.data_bus.write(segment_and_offset(es, di), lo).unwrap();
        self.data_bus
            .write(segment_and_offset(es, di.wrapping_add(1)), hi)
            .unwrap();

        let di = if self.flags.contains(Flags::DIRECTION) {
            di.wrapping_add(2)
        } else {
            di.wrapping_sub(2)
        };
        self.write_register_word(DI, di);
    }

    // AC
    fn op_lodsb(&mut self) {
        let ds = self.segments[DS];
        let si = self.read_register_word(SI);
        let increment = if self.flags.contains(Flags::DIRECTION) {
            0x0001
        } else {
            0xFFFF
        };

        if self.repeat {
            let cx = self.read_register_word(CX);
            if cx != 0 {
                let src = self.data_bus.read(segment_and_offset(ds, si)).unwrap();
                self.write_register_byte(AX, src);
                self.write_register_word(SI, si.wrapping_add(increment));
                self.write_register_word(CX, cx.wrapping_sub(1));
            }

            self.consume_cycles(12);

            if self.read_register_word(CX) == 0 {
                self.repeat = false;
            } else {
                // Keep the repeat prefix.
            }
        } else {
            let src = self.data_bus.read(segment_and_offset(ds, si)).unwrap();
            self.write_register_byte(AX, src);
            self.write_register_word(SI, si.wrapping_add(increment));
            self.consume_cycles(12);
        }
    }

    // B0
    fn op_mov_al_imm8(&mut self) {
        self.mov_reg8_imm8(AX);
    }

    // B1
    fn op_mov_cl_imm8(&mut self) {
        self.mov_reg8_imm8(CX);
    }

    // B2
    fn op_mov_dl_imm8(&mut self) {
        self.mov_reg8_imm8(DX);
    }

    // B3
    fn op_mov_bl_imm8(&mut self) {
        self.mov_reg8_imm8(BX);
    }

    // B4
    fn op_mov_ah_imm8(&mut self) {
        self.mov_reg8_imm8(SP);
    }

    // B5
    fn op_mov_ch_imm8(&mut self) {
        self.mov_reg8_imm8(BP);
    }

    // B6
    fn op_mov_dh_imm8(&mut self) {
        self.mov_reg8_imm8(SI);
    }

    // B7
    fn op_mov_bh_imm8(&mut self) {
        self.mov_reg8_imm8(DI);
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

    // C3
    fn op_ret(&mut self) {
        self.ip = self.pop();
        self.consume_cycles(16);
    }

    // C6
    fn op_mov_rm8_imm8(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (_, rm) = self.mod_reg_rm_to_operands::<u8>(mrrm);

        let src = self.fetch().unwrap();
        self.write_operand_byte(rm, src);

        self.consume_cycles_for_operand(rm, 4, 10);
    }

    // C7
    fn op_mov_rm16_imm16(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (_, rm) = self.mod_reg_rm_to_operands::<u8>(mrrm);

        let lo = self.fetch().unwrap();
        let hi = self.fetch().unwrap();
        let src = u16::from_le_bytes([lo, hi]);
        self.write_operand_word(rm, src);

        self.consume_cycles_for_operand(rm, 4, 10);
    }

    // D0
    fn op_group_d0_rm8_1(&mut self) {
        self.op_group_d0_byte(1);
    }

    // D2
    fn op_group_d0_rm8_cl(&mut self) {
        let cl = self.read_register_byte(CX);
        self.op_group_d0_byte(cl);
    }

    // D3
    fn op_group_d0_rm16_cl(&mut self) {
        let cl = self.read_register_byte(CX);
        self.op_group_d0_word(cl as u16);
    }

    // E1
    fn op_loopz_imm8(&mut self) {
        let cx = self.read_register_word(CX).wrapping_sub(1);
        self.write_register_word(CX, cx);

        let rel = self.fetch().unwrap();

        if cx != 0 && self.flags.contains(Flags::ZERO) {
            self.ip = self.ip.wrapping_add(rel as i8 as i16 as u16);
            self.consume_cycles(18);
        } else {
            self.consume_cycles(5);
        }
    }

    // E2
    fn op_loop_imm8(&mut self) {
        let cx = self.read_register_word(CX).wrapping_sub(1);
        self.write_register_word(CX, cx);

        let rel = self.fetch().unwrap();

        if cx != 0 {
            self.ip = self.ip.wrapping_add(rel as i8 as i16 as u16);
            self.consume_cycles(18);
        } else {
            self.consume_cycles(5);
        }
    }

    // E4
    fn op_in_al_imm8(&mut self) {
        let port = self.fetch().unwrap();
        self.write_register_byte(AX, self.io_bus.read(port as Port).unwrap());
        self.consume_cycles(11);
    }

    // E6
    fn op_out_imm8_al(&mut self) {
        let port = self.fetch().unwrap();
        let al = self.read_register_byte(AX);

        self.io_bus.write(port.into(), al).unwrap();
        self.consume_cycles(11);
    }

    // E8
    fn op_call_imm16(&mut self) {
        self._push(self.ip);

        let lo = self.fetch().unwrap();
        let hi = self.fetch().unwrap();
        let new_ip = u16::from_le_bytes([lo, hi]);

        self.ip = self.ip.wrapping_add(new_ip);

        self.consume_cycles(19);
    }

    // E9
    fn op_jmp_imm16(&mut self) {
        let addr = self.read_data_bus_word(self.flat_address());
        self.ip = self.ip.wrapping_add(addr);
        self.consume_cycles(15);
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

    // EC
    fn op_in_al_dx(&mut self) {
        let port = self.read_register_word(DX);
        let value = self.io_bus.read(port).unwrap();
        self.write_register_byte(AX, value);

        self.consume_cycles(9);
    }

    // ED
    fn op_in_ax_dx(&mut self) {
        let port = self.read_register_word(DX);
        let lo = self.io_bus.read(port).unwrap();
        let hi = self.io_bus.read(port.wrapping_add(1)).unwrap();
        let value = u16::from_le_bytes([lo, hi]);
        self.write_register_word(AX, value);

        self.consume_cycles(13);
    }

    // EE
    fn op_out_dx_al(&mut self) {
        let dx = self.read_register_word(DX);
        let al = self.read_register_byte(AX);

        self.io_bus.write(dx, al).unwrap();

        self.consume_cycles(9);
    }

    // F3
    fn op_rep(&mut self) {
        self.repeat = true;
        self.consume_cycles(2);
    }

    // F4
    fn op_hlt(&mut self) {
        self.halted = true;
        self.consume_cycles(2);
    }

    // F6
    fn op_xxx_rm8(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (xop, dst_op) = self.mod_reg_rm_to_operands(mrrm);
        let xop = if let Operand::Register(xop) = xop {
            xop
        } else {
            unreachable!()
        };

        let dst = self.read_operand_byte(dst_op);

        match xop {
            0b000 => {
                let src = self.fetch().unwrap();
                logic(Test, dst, src, &mut self.flags);
                self.consume_cycles_for_operand(dst_op, 5, 11);
            }

            0b001 => unreachable!(),

            0b010 => {
                let result = dst.not();
                self.write_operand_byte(dst_op, result);
                self.consume_cycles_for_operand(dst_op, 3, 16);
            }

            0b011 => {
                // Test is just a sub that doesn't use the result.
                let _ = arithmetic(Subtract, 0, dst, &mut self.flags);
                self.consume_cycles_for_operand(dst_op, 3, 16);
            }

            0b100 => todo!(),

            0b101 => todo!(),

            0b110 => todo!(),

            0b111 => todo!(),

            _ => unreachable!(),
        };
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

    // FB
    fn op_sti(&mut self) {
        self.flags.set(Flags::INTERRUPT, true);

        self.consume_cycles(2);
    }

    // FC
    fn op_cld(&mut self) {
        self.flags.set(Flags::DIRECTION, false);

        self.consume_cycles(2);
    }

    // FD
    fn op_std(&mut self) {
        self.flags.set(Flags::DIRECTION, true);

        self.consume_cycles(2);
    }

    // FE
    fn op_inc_dec_rm8(&mut self) {
        let mrrm = self.fetch().unwrap();
        let (xop, rm) = self.mod_reg_rm_to_operands::<usize>(mrrm);

        let dst = self.read_operand_byte(rm);

        let result = match xop {
            0b0 => arithmetic(Add, dst, 1, &mut self.flags),
            0b1 => arithmetic(Subtract, dst, 1, &mut self.flags),
            _ => unreachable!(),
        };
        self.write_operand_byte(rm, result);

        self.consume_cycles_for_operand(rm, 3, 15);
    }

    fn op_invalid(&mut self) {
        if cfg!(debug_assertions) {
            todo!("invalid op_code {:02X}", self.last_op_code)
        }
    }
}

// Helpers
impl<D: Bus<Address>, I: Bus<Port>> Intel8088<D, I> {
    // 40..47
    fn inc_reg16(&mut self, encoding: usize) {
        let dst = self.read_register_word(encoding);
        let result = arithmetic(Add, dst, 1, &mut self.flags);
        self.write_register_word(encoding, result);
        self.consume_cycles(3);
    }

    // 48..4F
    fn dec_reg16(&mut self, encoding: usize) {
        let dst = self.read_register_word(encoding);
        let result = arithmetic(Subtract, dst, 1, &mut self.flags);
        self.write_register_word(encoding, result);
        self.consume_cycles(3);
    }

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
    fn mov_reg8_imm8(&mut self, reg: usize) {
        let value = self.fetch().unwrap();
        self.write_register_byte(reg, value);
        self.consume_cycles(4);
    }

    // B8..BF
    fn mov_reg16_imm16(&mut self, reg: u8) {
        let lo = self.fetch().unwrap();
        let hi = self.fetch().unwrap();
        let value = u16::from_le_bytes([lo, hi]);

        self.write_register_word(reg as usize, value);

        self.consume_cycles(4);
    }

    // D0, D2
    fn op_group_d0_byte(&mut self, count: u8) {
        let mrrm = self.fetch().unwrap();
        let (op, src_op) = self.mod_rm_to_operands(mrrm);

        if count == 0 {
            // 8 for register
            // 20 for mem
            self.consume_cycles(20);
            return;
        }

        let value = self.read_operand_byte(src_op);

        let result = match op {
            0b000 => arithmetic(RotateLeft, value, count, &mut self.flags),
            0b001 => arithmetic(RotateRight, value, count, &mut self.flags),
            0b010 => arithmetic(RotateLeftWithCarry, value, count, &mut self.flags),
            0b011 => arithmetic(RotateRightWithCarry, value, count, &mut self.flags),
            0b100 => arithmetic(ShiftLeft, value, count, &mut self.flags),
            0b101 => arithmetic(ShiftRight, value, count, &mut self.flags),
            // 0b110,
            0b111 => arithmetic(ShiftArithmeticRight, value, count, &mut self.flags),

            _ => unreachable!(),
        };

        self.flags
            .set(Flags::OVERFLOW, ((result ^ value) & 0x80) != 0);

        self.write_operand_byte(src_op, result);

        self.consume_cycles_for_operand(src_op, 8 + 4 * count as usize, 20 + 4 * count as usize);
    }

    // D1, D3
    fn op_group_d0_word(&mut self, count: u16) {
        let mrrm = self.fetch().unwrap();
        let (op, src_op) = self.mod_rm_to_operands(mrrm);

        if count == 0 {
            // 8 for register
            // 20 for mem
            self.consume_cycles(20);
            return;
        }

        let value = self.read_operand_word(src_op);

        let result = match op {
            0b000 => arithmetic(RotateLeft, value, count, &mut self.flags),
            0b001 => arithmetic(RotateRight, value, count, &mut self.flags),
            0b010 => arithmetic(RotateLeftWithCarry, value, count, &mut self.flags),
            0b011 => arithmetic(RotateRightWithCarry, value, count, &mut self.flags),
            0b100 => arithmetic(ShiftLeft, value, count, &mut self.flags),
            0b101 => arithmetic(ShiftRight, value, count, &mut self.flags),
            // 0b110,
            0b111 => arithmetic(ShiftArithmeticRight, value, count, &mut self.flags),

            _ => unreachable!(),
        };

        self.flags
            .set(Flags::OVERFLOW, ((result ^ value) & 0x8000) != 0);

        self.write_operand_word(src_op, result);

        self.consume_cycles_for_operand(src_op, 8 + 4 * count as usize, 20 + 4 * count as usize);
    }
}
