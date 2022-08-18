#![allow(unused)]

use super::ast;
use crate::ast::DataSize;
use mrc_instruction::Operation;

pub trait ByteEmitter {
    fn emit(&mut self, byte: u8);
}

impl ByteEmitter for Vec<u8> {
    fn emit(&mut self, byte: u8) {
        self.push(byte);
    }
}

impl ByteEmitter for u8 {
    fn emit(&mut self, byte: u8) {
        *self += 1;
    }
}

#[derive(Debug)]
pub enum EncodeError {
    NonConstantImmediateValue(ast::Span),

    InvalidOperands(ast::Span),
    OperandSizeNotSpecified(ast::Span),
    OperandSizesDoNotMatch(ast::Span),
    InvalidOperandSize(ast::Span),
    ImmediateOutOfRange(ast::Span),
    RelativeJumpOutOfRange(ast::Span),
}

pub fn encode(
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    use mrc_instruction::Operation::*;

    match insn.operation {
        ADD => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x00, insn, offset, emitter),
        OR => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x08, insn, offset, emitter),
        ADC => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x10, insn, offset, emitter),
        SBB => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x18, insn, offset, emitter),
        AND => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x20, insn, offset, emitter),
        SUB => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x28, insn, offset, emitter),
        XOR => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x30, insn, offset, emitter),
        CMP => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x38, insn, offset, emitter),

        NOT => encode_group_not_neg_mul_imul_div_idiv(0x02, insn, offset, emitter),
        NEG => encode_group_not_neg_mul_imul_div_idiv(0x03, insn, offset, emitter),
        MUL => encode_group_not_neg_mul_imul_div_idiv(0x04, insn, offset, emitter),
        IMUL => encode_group_not_neg_mul_imul_div_idiv(0x05, insn, offset, emitter),
        DIV => encode_group_not_neg_mul_imul_div_idiv(0x06, insn, offset, emitter),
        IDIV => encode_group_not_neg_mul_imul_div_idiv(0x07, insn, offset, emitter),

        MOV => encode_group_mov(0x00, insn, offset, emitter),

        TEST => encode_group_test(0x00, insn, offset, emitter),

        XCHG => encode_group_xchg(0x00, insn, offset, emitter),

        INC => encode_group_inc_dec(0x00, insn, offset, emitter),
        DEC => encode_group_inc_dec(0x01, insn, offset, emitter),

        PUSH => encode_group_push(0x00, insn, offset, emitter),
        POP => encode_group_pop(0x00, insn, offset, emitter),

        RET => encode_group_ret_retn_retf(0xC2, insn, offset, emitter),
        // RETF => encode_group_ret_retn_retf(0xCA, insn, offset, emitter),
        //
        LEA => encode_group_lea(0x00, insn, offset, emitter),

        LES => encode_group_les_lds(0xC4, insn, offset, emitter),
        LDS => encode_group_les_lds(0xC5, insn, offset, emitter),

        ROL => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x00, insn, offset, emitter),
        ROR => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x01, insn, offset, emitter),
        RCL => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x02, insn, offset, emitter),
        RCR => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x03, insn, offset, emitter),
        SHL => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x04, insn, offset, emitter),
        // SAL => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x04, insn, offset, emitter),
        SHR => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x05, insn, offset, emitter),
        SAR => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x07, insn, offset, emitter),

        CALL => encode_group_call(0x00, insn, offset, emitter),

        JMP => encode_group_jmp(0x00, insn, offset, emitter),

        JO => encode_group_jcc(0x70, insn, offset, emitter),
        JNO => encode_group_jcc(0x71, insn, offset, emitter),
        JB => encode_group_jcc(0x72, insn, offset, emitter),
        JNB => encode_group_jcc(0x73, insn, offset, emitter),
        JE => encode_group_jcc(0x74, insn, offset, emitter),
        JNE => encode_group_jcc(0x75, insn, offset, emitter),
        JBE => encode_group_jcc(0x76, insn, offset, emitter),
        JNBE => encode_group_jcc(0x77, insn, offset, emitter),
        JS => encode_group_jcc(0x78, insn, offset, emitter),
        JNS => encode_group_jcc(0x79, insn, offset, emitter),
        JP => encode_group_jcc(0x7A, insn, offset, emitter),
        JNP => encode_group_jcc(0x7B, insn, offset, emitter),
        JL => encode_group_jcc(0x7C, insn, offset, emitter),
        JNL => encode_group_jcc(0x7D, insn, offset, emitter),
        JLE => encode_group_jcc(0x7E, insn, offset, emitter),
        JNLE => encode_group_jcc(0x7F, insn, offset, emitter),

        LOOPNZ => encode_group_loopc(0xE0, insn, offset, emitter),
        LOOPZ => encode_group_loopc(0xE1, insn, offset, emitter),
        LOOP => encode_group_loopc(0xE2, insn, offset, emitter),
        JCXZ => encode_group_loopc(0xE3, insn, offset, emitter),

        DAA => encode_group_no_operands(0x27, insn, offset, emitter),
        DAS => encode_group_no_operands(0x2F, insn, offset, emitter),
        AAA => encode_group_no_operands(0x37, insn, offset, emitter),
        AAS => encode_group_no_operands(0x3F, insn, offset, emitter),
        NOP => encode_group_no_operands(0x90, insn, offset, emitter),
        CBW => encode_group_no_operands(0x98, insn, offset, emitter),
        CWD => encode_group_no_operands(0x99, insn, offset, emitter),
        INT3 => encode_group_no_operands(0xCC, insn, offset, emitter),
        INTO => encode_group_no_operands(0xCE, insn, offset, emitter),
        IRET => encode_group_no_operands(0xCF, insn, offset, emitter),
        SALC => encode_group_no_operands(0xD6, insn, offset, emitter),
        HLT => encode_group_no_operands(0xF4, insn, offset, emitter),
        CMC => encode_group_no_operands(0xF5, insn, offset, emitter),
        CLC => encode_group_no_operands(0xF8, insn, offset, emitter),
        STC => encode_group_no_operands(0xF9, insn, offset, emitter),
        CLI => encode_group_no_operands(0xFA, insn, offset, emitter),
        STI => encode_group_no_operands(0xFB, insn, offset, emitter),
        CLD => encode_group_no_operands(0xFC, insn, offset, emitter),
        STD => encode_group_no_operands(0xFD, insn, offset, emitter),
        PUSHF => encode_group_no_operands(0x9C, insn, offset, emitter),
        POPF => encode_group_no_operands(0x9D, insn, offset, emitter),
        SAHF => encode_group_no_operands(0x9E, insn, offset, emitter),
        LAHF => encode_group_no_operands(0x9F, insn, offset, emitter),
        MOVSB => encode_group_no_operands(0xA4, insn, offset, emitter),
        CMPSB => encode_group_no_operands(0xA6, insn, offset, emitter),
        STOSB => encode_group_no_operands(0xAA, insn, offset, emitter),
        LODSB => encode_group_no_operands(0xAC, insn, offset, emitter),
        SCASB => encode_group_no_operands(0xAE, insn, offset, emitter),
        // XLATB => encode_group_no_operands(0xD7, insn, offset, emitter),
        MOVSW => encode_group_no_operands(0xA5, insn, offset, emitter),
        CMPSW => encode_group_no_operands(0xA7, insn, offset, emitter),
        STOSW => encode_group_no_operands(0xAB, insn, offset, emitter),
        LODSW => encode_group_no_operands(0xAD, insn, offset, emitter),
        SCASW => encode_group_no_operands(0xAF, insn, offset, emitter),

        LOCK => encode_group_prefixes(0xF0, insn, offset, emitter),
        REPNE => encode_group_prefixes(0xF2, insn, offset, emitter),
        REP => encode_group_prefixes(0xF3, insn, offset, emitter),

        INT => encode_group_int(0x00, insn, offset, emitter),

        AAM => encode_group_aam(0x00, insn, offset, emitter),

        AAD => encode_group_aad(0x00, insn, offset, emitter),

        XLAT => encode_group_xlat(0x00, insn, offset, emitter),

        IN => encode_group_in(0x00, insn, offset, emitter),

        OUT => encode_group_out(0x00, insn, offset, emitter),

        op => todo!("{:?}", op),
    };

    Ok(())
}

fn encode_group_add_or_adc_sbb_and_sub_xor_cmp(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, src] = &insn.opers;

    if dst.size.is_unspecified() && src.size.is_unspecified() {
        return Err(EncodeError::OperandSizeNotSpecified(
            insn.opers_span.clone(),
        ));
    }

    if dst.size.is_specified() && src.size.is_specified() && dst.size != src.size {
        return Err(EncodeError::OperandSizesDoNotMatch(insn.opers_span.clone()));
    }

    let size = if dst.size.is_specified() {
        dst.size
    } else {
        src.size
    };

    match (src.kind, dst.kind) {
        (OperandKind::Reg, OperandKind::Reg | OperandKind::Mem) => {
            let op_code = match size {
                OperandSize::Word => base,
                OperandSize::Byte => base + 1,
                _ => unreachable!(),
            };
            store_instruction(op_code, &dst, src.rm, OperandSize::Unspecified, 0, emitter);
        }

        (OperandKind::Mem, OperandKind::Reg) => {
            let op_code = match size {
                OperandSize::Byte => base + 2 + 1,
                OperandSize::Word => base + 2,
                _ => unreachable!(),
            };
            store_instruction(op_code, &src, dst.rm, OperandSize::Unspecified, 0, emitter);
        }

        (OperandKind::Imm, OperandKind::Reg | OperandKind::Mem) => {
            match size {
                OperandSize::Word => {
                    if dst.kind == OperandKind::Reg && dst.rm == 0 {
                        // AX, imm
                        let op_code = base + 5;
                        emitter.emit(op_code);

                        if src.imm < 0 || src.imm >= 0x10000 {
                            return Err(EncodeError::ImmediateOutOfRange(src.span.clone()));
                        }

                        for byte in (src.imm as u16).to_le_bytes() {
                            emitter.emit(byte);
                        }
                    } else if src.imm < 0x80 && src.imm >= -0x80 {
                        // reg16, imm8
                        let rm = base >> 3;
                        store_instruction(0x83, &dst, rm, OperandSize::Byte, src.imm, emitter);
                    } else {
                        // reg16, imm16
                        let rm = base >> 3;
                        store_instruction(0x81, &dst, rm, size, src.imm, emitter);
                    }
                }

                OperandSize::Byte => {
                    if dst.kind == OperandKind::Reg && dst.rm == 0 {
                        // AL, imm
                        let op_code = base + 4;
                        emitter.emit(op_code);
                        emitter.emit(src.imm as u8);
                    } else {
                        // reg8, imm
                        let rm = base >> 3;
                        if src.imm < 0 || src.imm >= 0x100 {
                            return Err(EncodeError::ImmediateOutOfRange(src.span.clone()));
                        }
                        store_instruction(0x80, &dst, rm, OperandSize::Byte, src.imm, emitter);
                    }
                }

                _ => unreachable!(),
            }
        }

        _ => return Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }

    Ok(())
}

fn encode_group_not_neg_mul_imul_div_idiv(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    if dst.size.is_unspecified() {
        return Err(EncodeError::OperandSizeNotSpecified(
            insn.opers_span.clone(),
        ));
    }

    if !matches!(dst.kind, OperandKind::Mem | OperandKind::Reg) {
        return Err(EncodeError::InvalidOperands(insn.opers_span.clone()));
    }

    match dst.size {
        OperandSize::Byte => {
            store_instruction(0xF6, &dst, base, OperandSize::Unspecified, 0, emitter)
        }

        OperandSize::Word => {
            store_instruction(0xF7, &dst, base, OperandSize::Unspecified, 0, emitter)
        }

        _ => {}
    }

    Ok(())
}

fn encode_group_mov(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_test(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_xchg(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_inc_dec(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    if dst.size.is_unspecified() {
        return Err(EncodeError::InvalidOperandSize(dst.span.clone()));
    }

    match dst.kind {
        OperandKind::Reg if dst.size == OperandSize::Word => {
            let op_code = 0x40 + dst.rm + base << 3;
            emitter.emit(op_code);
        }

        OperandKind::Reg if dst.size == OperandSize::Byte => {
            store_instruction(0xFE, &dst, base, OperandSize::Unspecified, 0, emitter);
        }

        OperandKind::Mem => match dst.size {
            OperandSize::Byte => {
                store_instruction(0xFE, &dst, base, OperandSize::Unspecified, 0, emitter);
            }
            OperandSize::Word => {
                store_instruction(0xFF, &dst, base, OperandSize::Unspecified, 0, emitter)
            }
            _ => unreachable!(),
        },

        _ => unreachable!(),
    }

    Ok(())
}

fn encode_group_push(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    if dst.size != OperandSize::Word {
        return Err(EncodeError::InvalidOperandSize(dst.span.clone()));
    }

    match dst.kind {
        OperandKind::Mem => {
            todo!()
        }

        OperandKind::Reg => {
            let op_code = 0x50 + dst.rm;
            emitter.emit(op_code);
        }

        OperandKind::Seg => {
            let op_code = 6 + dst.rm << 3;
            emitter.emit(op_code);
        }

        _ => {
            return Err(EncodeError::InvalidOperandSize(dst.span.clone()));
        }
    }

    Ok(())
}

fn encode_group_pop(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    if dst.size != OperandSize::Word {
        return Err(EncodeError::InvalidOperandSize(dst.span.clone()));
    }

    match dst.kind {
        OperandKind::Mem => {
            todo!()
        }

        OperandKind::Reg => {
            let op_code = 0x58 + dst.rm;
            emitter.emit(op_code);
        }

        OperandKind::Seg => {
            let op_code = 7 + dst.rm << 3;
            emitter.emit(op_code);
        }

        _ => {
            return Err(EncodeError::InvalidOperandSize(dst.span.clone()));
        }
    }

    Ok(())
}

fn encode_group_ret_retn_retf(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    emitter.emit(base);
    Ok(())
}

fn encode_group_lea(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_les_lds(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, src] = &insn.opers;

    if dst.size != OperandSize::Word {
        return Err(EncodeError::InvalidOperandSize(dst.span.clone()));
    }

    if src.size != OperandSize::Word {
        return Err(EncodeError::InvalidOperandSize(src.span.clone()));
    }

    if dst.kind != OperandKind::Reg || src.kind != OperandKind::Mem {
        return Err(EncodeError::InvalidOperands(insn.opers_span.clone()));
    }

    store_instruction(base, &src, dst.rm, OperandSize::Unspecified, 0, emitter);

    Ok(())
}

fn encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_call(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_jmp(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    match dst.kind {
        OperandKind::Imm => {
            if dst.imm < u16::MIN as i32 || dst.imm > u16::MAX as i32 {
                return Err(EncodeError::ImmediateOutOfRange(dst.span.clone()));
            }

            match dst.jmp_kind {
                Some(JumpKind::Short) => {
                    todo!()
                }

                None | Some(JumpKind::Near) => {
                    emitter.emit(0xE9);
                    let rel = (dst.imm - (offset as i32 + 2)) as i16 as u16;
                    for byte in rel.to_le_bytes() {
                        emitter.emit(byte);
                    }
                }

                Some(JumpKind::Far) => {
                    todo!()
                }
            }
        }

        OperandKind::Mem | OperandKind::Reg => {
            if let OperandSize::Word = dst.size {
                store_instruction(0xFF, &dst, 0b100, OperandSize::Unspecified, 0, emitter);
            }
        }

        // OperandKind::Far => {}
        _ => return Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }

    Ok(())
}

fn encode_group_jcc(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    if dst.kind != OperandKind::Imm && (!matches!(dst.jmp_kind, Some(JumpKind::Short))) {
        return Err(EncodeError::InvalidOperands(dst.span.clone()));
    }

    if dst.imm < u16::MIN as i32 || dst.imm >= u16::MAX as i32 {
        return Err(EncodeError::RelativeJumpOutOfRange(dst.span.clone()));
    }

    let jmp_dst = dst.imm - (offset as i32 + 2);

    if jmp_dst < i8::MIN as i32 || jmp_dst > i8::MAX as i32 {
        return Err(EncodeError::RelativeJumpOutOfRange(dst.span.clone()));
    }

    emitter.emit(base);
    emitter.emit(jmp_dst as i8 as u8);

    Ok(())
}

fn encode_group_loopc(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    match dst.kind {
        OperandKind::Imm
            if matches!(dst.jmp_kind, Some(JumpKind::Short)) || dst.jmp_kind.is_none() =>
        {
            emitter.emit(base);

            let rel = dst.imm as i32 - (offset as i32 + 1);

            if rel >= i8::MIN as i32 && rel <= i8::MAX as i32 {
                emitter.emit(rel as i8 as u8);
            // } else if jmp_dst >= i16::MIN as i32 && jmp_dst <= i16::MAX as i32 {
            //     for byte in (jmp_dst as i16 as u16).to_le_bytes() {
            //         emitter.emit(byte);
            //     }
            } else {
                return Err(EncodeError::RelativeJumpOutOfRange(dst.span.clone()));
            }
        }

        _ => return Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }

    Ok(())
}

fn encode_group_no_operands(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    emitter.emit(base);
    Ok(())
}

fn encode_group_prefixes(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    emitter.emit(base);
    Ok(())
}

fn encode_group_int(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    match dst.kind {
        OperandKind::Imm => {
            emitter.emit(0xCD);

            if dst.imm >= u8::MIN as i32 && dst.imm <= u8::MAX as i32 {
                emitter.emit(dst.imm as u8);
            } else {
                return Err(EncodeError::ImmediateOutOfRange(dst.span.clone()));
            }
        }

        _ => return Err(EncodeError::InvalidOperands(dst.span.clone())),
    }

    Ok(())
}

fn encode_group_aam(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    emitter.emit(0xD4);
    emitter.emit(0xA0);
    Ok(())
}

fn encode_group_aad(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    emitter.emit(0xD5);
    emitter.emit(0xA0);
    Ok(())
}

fn encode_group_movs(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_cmps(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_stos(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_lods(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_scas(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_xlat(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    if dst.size != OperandSize::Word {
        return Err(EncodeError::InvalidOperandSize(dst.span.clone()));
    }

    match dst.kind {
        OperandKind::Mem if dst.mode == 0 && dst.rm == 7 => {
            if dst.segment_prefix != 0 && dst.segment_prefix != 0x3E {
                emitter.emit(dst.segment_prefix);
            }
            emitter.emit(0xD7);
        }

        _ => return Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }

    Ok(())
}

fn encode_group_in(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, src] = &insn.opers;

    match (dst.kind, src.kind) {
        (OperandKind::Reg, OperandKind::Reg)
            if src.size == OperandSize::Word
                && src.rm == 2
                && dst.size.is_specified()
                && dst.rm == 0 =>
        {
            match dst.size {
                // al, dx
                OperandSize::Byte => emitter.emit(0xEC),

                // ax, dx
                OperandSize::Word => emitter.emit(0xED),

                OperandSize::Unspecified => unreachable!(),
            }
        }

        (OperandKind::Reg, OperandKind::Imm) if dst.size.is_specified() && dst.rm == 0 => {
            match dst.size {
                // al, imm
                OperandSize::Byte => {
                    emitter.emit(0xE4);
                    emitter.emit(src.imm as u8);
                }

                // ax, imm
                OperandSize::Word => {
                    emitter.emit(0xE5);
                    emitter.emit(src.imm as u8);
                }

                OperandSize::Unspecified => unreachable!(),
            }
        }

        _ => return Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }

    Ok(())
}

fn encode_group_out(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, src] = &insn.opers;

    match (dst.kind, src.kind) {
        (OperandKind::Reg, OperandKind::Reg)
            if dst.size == OperandSize::Word
                && dst.rm == 2
                && src.size.is_specified()
                && src.rm == 0 =>
        {
            match src.size {
                // dx, al
                OperandSize::Byte => emitter.emit(0xEE),

                // dx, ax
                OperandSize::Word => emitter.emit(0xEF),

                _ => unreachable!(),
            }
        }

        (OperandKind::Imm, OperandKind::Reg) if src.size.is_specified() && src.rm == 0 => {
            match src.size {
                // imm, al
                OperandSize::Byte => {
                    emitter.emit(0xE6);
                    emitter.emit(dst.imm as u8);
                }

                // imm, ax
                OperandSize::Word => {
                    emitter.emit(0xE7);
                    emitter.emit(dst.imm as u8);
                }

                _ => unreachable!(),
            }
        }

        _ => return Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }

    Ok(())
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OperandKind {
    Imm,
    Reg,
    Seg,
    Mem,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OperandSize {
    Unspecified = 0,
    Byte = 1,
    Word = 2,
}

impl OperandSize {
    fn size_in_bytes(&self) -> u8 {
        *self as u8
    }

    fn is_specified(&self) -> bool {
        match self {
            OperandSize::Unspecified => false,
            _ => true,
        }
    }

    fn is_unspecified(&self) -> bool {
        match self {
            OperandSize::Unspecified => true,
            _ => false,
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum JumpKind {
    Far,
    Near,
    Short,
}

#[derive(Debug)]
pub struct OperandData {
    pub span: ast::Span,
    pub size: OperandSize,
    pub kind: OperandKind,
    pub segment_prefix: u8,
    pub imm: i32,
    // pubunresolved: u8,
    pub displacement: i32,
    pub displacement_size: OperandSize,
    // pubaddress: u8,
    // pubaddress_registers: u8,
    // pubsegment: u8,
    // puboffset: u8,
    pub jmp_kind: Option<JumpKind>,
    pub mode: u8,
    pub rm: u8,
}

impl OperandData {
    pub fn empty() -> Self {
        Self {
            span: 0..0,
            size: OperandSize::Unspecified,
            kind: OperandKind::Imm,
            segment_prefix: 0,
            imm: 0,
            displacement: 0,
            displacement_size: OperandSize::Unspecified,
            jmp_kind: None,
            mode: 0,
            rm: 0,
        }
    }
}

pub fn operand_size_from_data_size(data_size: &Option<DataSize>) -> OperandSize {
    if let Some(data_size) = data_size {
        match data_size {
            ast::DataSize::Byte => OperandSize::Byte,
            ast::DataSize::Word => OperandSize::Word,
        }
    } else {
        OperandSize::Unspecified
    }
}

#[derive(Debug)]
pub struct InstructionData {
    pub operation: Operation,
    pub num_opers: u8,
    pub opers: [OperandData; 2],
    pub opers_span: ast::Span,
}

fn expr_to_u8(expr: &ast::Expression) -> Result<u8, EncodeError> {
    match expr {
        ast::Expression::Value(_, ast::Value::Constant(value)) => {
            let value = *value;

            if value >= u8::MIN as i32 && value <= u8::MAX as i32 {
                Ok(value as u8)
            } else {
                Err(EncodeError::ImmediateOutOfRange(expr.span().clone()))
            }
        }
        _ => panic!("Variable expression found! {:?}", expr),
    }
}

fn store_instruction(
    op_code: u8,
    rm: &OperandData,
    reg: u8,
    imm_size: OperandSize,
    immediate: i32,
    emitter: &mut impl ByteEmitter,
) {
    if rm.segment_prefix != 0 {
        emitter.emit(rm.segment_prefix);
    }

    let modrm = (rm.mode << 6) + (reg << 3) + rm.rm;

    emitter.emit(op_code);
    emitter.emit(modrm);

    match rm.displacement_size {
        OperandSize::Byte => {
            emitter.emit(rm.displacement as i8 as u8);
        }

        OperandSize::Word => {
            for byte in (rm.displacement as i16 as u16).to_le_bytes() {
                emitter.emit(byte);
            }
        }

        _ => {}
    }

    match imm_size {
        OperandSize::Byte => emitter.emit(immediate as u8),
        OperandSize::Word => {
            for byte in (immediate as u16).to_le_bytes() {
                emitter.emit(byte);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mrc_instruction::Operation;
    use std::io::Write;

    fn print_bytes(bytes: &Vec<u8>) {
        for byte in bytes.iter() {
            print!("{:02X} ", byte);
        }
    }

    macro_rules! insn {
        ($operation:expr $(,)?) => {{
            ast::Instruction {
                span: 0..0,
                operation: $operation,
                operands: ast::Operands::None(0..0),
            }
        }};

        ($operation:expr, $dst:expr $(,)?) => {{
            ast::Instruction {
                span: 0..0,
                operation: $operation,
                operands: ast::Operands::Destination(0..0, $dst),
            }
        }};

        ($operation:expr, $dst:expr, $src:expr $(,)?) => {{
            ast::Instruction {
                span: 0..0,
                operation: $operation,
                operands: ast::Operands::DestinationAndSource(0..0, $dst, $src),
            }
        }};
    }

    macro_rules! imm {
        ($value:expr) => {{
            ast::Operand::Immediate(
                0..0,
                ast::Expression::Value(0..0, ast::Value::Constant($value)),
            )
        }};
    }

    macro_rules! reg {
        (al) => {{
            ast::Operand::Register(0..0, ast::Register::Byte(ast::ByteRegister::Al))
        }};
        (bl) => {{
            ast::Operand::Register(0..0, ast::Register::Byte(ast::ByteRegister::Bl))
        }};
        (cl) => {{
            ast::Operand::Register(0..0, ast::Register::Byte(ast::ByteRegister::Cl))
        }};
        (dl) => {{
            ast::Operand::Register(0..0, ast::Register::Byte(ast::ByteRegister::Dl))
        }};

        (ax) => {{
            ast::Operand::Register(0..0, ast::Register::Word(ast::WordRegister::Ax))
        }};
        (bx) => {{
            ast::Operand::Register(0..0, ast::Register::Word(ast::WordRegister::Bx))
        }};
        (cx) => {{
            ast::Operand::Register(0..0, ast::Register::Word(ast::WordRegister::Cx))
        }};
        (dx) => {{
            ast::Operand::Register(0..0, ast::Register::Word(ast::WordRegister::Dx))
        }};
    }

    macro_rules! direct {
        (es: $addr:expr) => {{
            direct!($addr, None, Some(ast::Segment::ES))
        }};
        (cs: $addr:expr) => {{
            direct!($addr, None, Some(ast::Segment::CS))
        }};
        (ss: $addr:expr) => {{
            direct!($addr, None, Some(ast::Segment::SS))
        }};
        (ds: $addr:expr) => {{
            direct!($addr, None, Some(ast::Segment::DS))
        }};

        ($addr:expr) => {{
            direct!($addr, None, None)
        }};

        ($addr:expr, $data_size:expr, $segment_override:expr) => {{
            ast::Operand::Direct(
                0..0,
                ast::Expression::Value(0..0, ast::Value::Constant($addr)),
                $data_size,
                $segment_override,
            )
        }};
    }

    macro_rules! single_indirect_encoding {
        (bx) => {{
            ast::IndirectEncoding::Bx
        }};
    }

    #[test]
    fn group_add_or_adc_sbb_and_sub_xor_cmp() {
        fn group_add_or_adc_sbb_and_sub_xor_cmp_inner(op: Operation, base: u8) {
            let instruction = insn!(op, reg!(bx), reg!(cx));
            assert_eq!(vec![base, 0xCB], encode(&instruction, 0x100).unwrap());

            let instruction = insn!(op, reg!(bl), reg!(cl));
            assert_eq!(
                vec![base + 0x01, 0xCB],
                encode(&instruction, 0x100).unwrap()
            );

            let instruction = insn!(op, reg!(bl), direct!(0x200));
            assert_eq!(
                vec![base + 0x03, 0x1E, 0x00, 0x02],
                encode(&instruction, 0x100).unwrap()
            );

            let instruction = insn!(op, direct!(cs:0x200), reg!(bl));
            assert_eq!(
                vec![0x2E, base + 0x01, 0x1E, 0x00, 0x02],
                encode(&instruction, 0x100).unwrap()
            );

            let instruction = insn!(op, reg!(bx), imm!(0x10));
            assert_eq!(
                vec![0x83, 0xC3 + base, 0x10],
                encode(&instruction, 0x100).unwrap()
            );

            let instruction = insn!(op, reg!(al), imm!(0x10));
            assert_eq!(
                vec![base + 0x04, 0x10],
                encode(&instruction, 0x100).unwrap()
            );
            let instruction = insn!(op, reg!(ax), imm!(0x10));
            assert_eq!(
                vec![base + 0x05, 0x10, 0x00],
                encode(&instruction, 0x100).unwrap()
            );
            let instruction = insn!(op, reg!(bx), imm!(0x100));
            assert_eq!(
                vec![0x81, 0xC3 + base, 0x00, 0x01],
                encode(&instruction, 0x100).unwrap()
            );
        }

        group_add_or_adc_sbb_and_sub_xor_cmp_inner(Operation::ADD, 0x00);
        group_add_or_adc_sbb_and_sub_xor_cmp_inner(Operation::OR, 0x08);
        group_add_or_adc_sbb_and_sub_xor_cmp_inner(Operation::ADC, 0x10);
        group_add_or_adc_sbb_and_sub_xor_cmp_inner(Operation::SBB, 0x18);
        group_add_or_adc_sbb_and_sub_xor_cmp_inner(Operation::AND, 0x20);
        group_add_or_adc_sbb_and_sub_xor_cmp_inner(Operation::SUB, 0x28);
        group_add_or_adc_sbb_and_sub_xor_cmp_inner(Operation::XOR, 0x30);
        group_add_or_adc_sbb_and_sub_xor_cmp_inner(Operation::CMP, 0x38);
    }

    #[test]
    fn group_not_neg_mul_imul_div_idiv() {
        fn group_not_neg_mul_imul_div_idiv_inner(op: Operation, post_byte: u8) {
            let instruction = insn!(op, reg!(bl));
            assert_eq!(
                vec![0xF6, (0b11 << 6) + (post_byte << 3) + 0b011],
                encode(&instruction, 0x100).unwrap()
            );
            let instruction = insn!(op, reg!(bx));
            assert_eq!(
                vec![0xF7, (0b11 << 6) + (post_byte << 3) + 0b011],
                encode(&instruction, 0x100).unwrap()
            );
        }

        group_not_neg_mul_imul_div_idiv_inner(Operation::NOT, 0x02);
        group_not_neg_mul_imul_div_idiv_inner(Operation::NEG, 0x03);
        group_not_neg_mul_imul_div_idiv_inner(Operation::MUL, 0x04);
        group_not_neg_mul_imul_div_idiv_inner(Operation::IMUL, 0x05);
        group_not_neg_mul_imul_div_idiv_inner(Operation::DIV, 0x06);
        group_not_neg_mul_imul_div_idiv_inner(Operation::IDIV, 0x07);
    }

    #[test]
    fn group_inc_dec() {
        let insn = insn!(Operation::INC, reg!(bx));
        assert_eq!(vec![0x18], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::INC, reg!(bl));
        assert_eq!(vec![0xFE, 0xC3], encode(&insn, 0x100).unwrap());

        let insn = insn!(
            Operation::INC,
            ast::Operand::Direct(
                0..0,
                ast::Expression::Value(0..0, ast::Value::Constant(0x2000)),
                Some(ast::DataSize::Word),
                None,
            )
        );
        assert_eq!(vec![0xFF, 0x06, 0x00, 0x20], encode(&insn, 0x100).unwrap());

        let insn = insn!(
            Operation::INC,
            ast::Operand::Direct(
                0..0,
                ast::Expression::Value(0..0, ast::Value::Constant(0x2000)),
                Some(ast::DataSize::Byte),
                None,
            )
        );
        assert_eq!(vec![0xFE, 0x06, 0x00, 0x20], encode(&insn, 0x100).unwrap());
    }

    #[test]
    fn group_jmp() {
        let insn = insn!(Operation::JMP, imm!(0x110));
        assert_eq!(vec![0xE9, 0x0E, 0x00], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JMP, imm!(0x70));
        assert_eq!(vec![0xE9, 0x6E, 0xFF], encode(&insn, 0x100).unwrap());

        let insn = insn!(Operation::JMP, reg!(bx));
        assert_eq!(vec![0xFF, 0xE3], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JMP, direct!(0x2000));
        assert_eq!(vec![0xFF, 0xE3], encode(&insn, 0x100).unwrap());
    }

    #[test]
    fn group_jcc() {
        let insn = insn!(Operation::JO, imm!(0x100));
        assert_eq!(vec![0x70, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JNO, imm!(0x100));
        assert_eq!(vec![0x71, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JB, imm!(0x100));
        assert_eq!(vec![0x72, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JNB, imm!(0x100));
        assert_eq!(vec![0x73, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JE, imm!(0x100));
        assert_eq!(vec![0x74, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JNE, imm!(0x100));
        assert_eq!(vec![0x75, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JBE, imm!(0x100));
        assert_eq!(vec![0x76, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JNBE, imm!(0x100));
        assert_eq!(vec![0x77, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JS, imm!(0x100));
        assert_eq!(vec![0x78, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JNS, imm!(0x100));
        assert_eq!(vec![0x79, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JP, imm!(0x100));
        assert_eq!(vec![0x7A, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JNP, imm!(0x100));
        assert_eq!(vec![0x7B, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JL, imm!(0x100));
        assert_eq!(vec![0x7C, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JNL, imm!(0x100));
        assert_eq!(vec![0x7D, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JLE, imm!(0x100));
        assert_eq!(vec![0x7E, 0xFE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::JNLE, imm!(0x100));
        assert_eq!(vec![0x7F, 0xFE], encode(&insn, 0x100).unwrap());
    }

    #[test]
    fn group_loopc() {
        let insn = insn!(Operation::LOOPNZ, imm!(0x100));
        assert_eq!(vec![0xE0, 0xFF], encode(&insn, 0x100).unwrap());

        let insn = insn!(Operation::LOOPZ, imm!(0x100));
        assert_eq!(vec![0xE1, 0xFF], encode(&insn, 0x100).unwrap());

        let insn = insn!(Operation::LOOP, imm!(0x100));
        assert_eq!(vec![0xE2, 0xFF], encode(&insn, 0x100).unwrap());

        let insn = insn!(Operation::JCXZ, imm!(0x100));
        assert_eq!(vec![0xE3, 0xFF], encode(&insn, 0x100).unwrap());
    }

    #[test]
    fn group_no_operands() {
        let tests: &[(Operation, u8)] = &[
            (Operation::DAA, 0x27),
            (Operation::DAS, 0x2F),
            (Operation::AAA, 0x37),
            (Operation::AAS, 0x3F),
            (Operation::NOP, 0x90),
            (Operation::CBW, 0x98),
            (Operation::CWD, 0x99),
            (Operation::INT3, 0xCC),
            (Operation::INTO, 0xCE),
            (Operation::IRET, 0xCF),
            (Operation::SALC, 0xD6),
            (Operation::HLT, 0xF4),
            (Operation::CMC, 0xF5),
            (Operation::CLC, 0xF8),
            (Operation::STC, 0xF9),
            (Operation::CLI, 0xFA),
            (Operation::STI, 0xFB),
            (Operation::CLD, 0xFC),
            (Operation::STD, 0xFD),
            (Operation::PUSHF, 0x9C),
            (Operation::POPF, 0x9D),
            (Operation::SAHF, 0x9E),
            (Operation::LAHF, 0x9F),
            (Operation::MOVSB, 0xA4),
            (Operation::CMPSB, 0xA6),
            (Operation::STOSB, 0xAA),
            (Operation::LODSB, 0xAC),
            (Operation::SCASB, 0xAE),
            // (Operation::XLATB, 0xD7),
            (Operation::MOVSW, 0xA5),
            (Operation::CMPSW, 0xA7),
            (Operation::STOSW, 0xAB),
            (Operation::LODSW, 0xAD),
            (Operation::SCASW, 0xAF),
        ];

        for (op, op_code) in tests.iter() {
            let insn = insn!(*op);
            assert_eq!(vec![*op_code], encode(&insn, 0x100).unwrap());
        }
    }

    #[test]
    fn group_prefixes() {
        let insn = insn!(Operation::LOCK);
        assert_eq!(vec![0xF0], encode(&insn, 0x100).unwrap());

        let insn = insn!(Operation::REPNE);
        assert_eq!(vec![0xF2], encode(&insn, 0x100).unwrap());

        let insn = insn!(Operation::REP);
        assert_eq!(vec![0xF3], encode(&insn, 0x100).unwrap());
    }

    #[test]
    fn group_int() {
        let insn = insn!(Operation::INT, imm!(0x21));
        assert_eq!(vec![0xCD, 0x21], encode(&insn, 0x100).unwrap());
    }

    #[test]
    fn group_aam() {
        let insn = insn!(Operation::AAM);
        assert_eq!(vec![0xD4, 0xA0], encode(&insn, 0x100).unwrap());
    }

    #[test]
    fn group_aad() {
        let insn = insn!(Operation::AAD);
        assert_eq!(vec![0xD5, 0xA0], encode(&insn, 0x100).unwrap());
    }

    #[test]
    fn group_xlat() {
        let insn = insn!(
            Operation::XLAT,
            ast::Operand::Indirect(
                0..0,
                ast::IndirectEncoding::Bx,
                None,
                Some(ast::DataSize::Word),
                None
            )
        );
        assert_eq!(vec![0xD7], encode(&insn, 0x100).unwrap());

        let insn = insn!(
            Operation::XLAT,
            ast::Operand::Indirect(
                0..0,
                ast::IndirectEncoding::Bx,
                None,
                Some(ast::DataSize::Word),
                Some(ast::Segment::CS),
            )
        );
        assert_eq!(vec![0x2E, 0xD7], encode(&insn, 0x100).unwrap());

        // DS does not get a segment override.
        let insn = insn!(
            Operation::XLAT,
            ast::Operand::Indirect(
                0..0,
                ast::IndirectEncoding::Bx,
                None,
                Some(ast::DataSize::Word),
                Some(ast::Segment::DS),
            )
        );
        assert_eq!(vec![0xD7], encode(&insn, 0x100).unwrap());
    }

    #[test]
    fn group_in() {
        let insn = insn!(Operation::IN, reg!(al), imm!(0x10));
        assert_eq!(vec![0xE4, 0x10], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::IN, reg!(ax), imm!(0x10));
        assert_eq!(vec![0xE5, 0x10], encode(&insn, 0x100).unwrap());

        let insn = insn!(Operation::IN, reg!(al), reg!(dx));
        assert_eq!(vec![0xEC], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::IN, reg!(ax), reg!(dx));
        assert_eq!(vec![0xED], encode(&insn, 0x100).unwrap());
    }

    #[test]
    fn group_out() {
        let insn = insn!(Operation::OUT, imm!(0x10), reg!(al));
        assert_eq!(vec![0xE6, 0x10], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::OUT, imm!(0x10), reg!(ax));
        assert_eq!(vec![0xE7, 0x10], encode(&insn, 0x100).unwrap());

        let insn = insn!(Operation::OUT, reg!(dx), reg!(al));
        assert_eq!(vec![0xEE], encode(&insn, 0x100).unwrap());
        let insn = insn!(Operation::OUT, reg!(dx), reg!(ax));
        assert_eq!(vec![0xEF], encode(&insn, 0x100).unwrap());
    }
}
