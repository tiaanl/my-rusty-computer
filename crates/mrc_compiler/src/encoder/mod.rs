#![allow(unused)]

use super::ast;
use crate::ast::DataSize;

trait ByteEmitter {
    fn emit(&mut self, byte: u8);
}

impl ByteEmitter for Vec<u8> {
    fn emit(&mut self, byte: u8) {
        self.push(byte);
    }
}

#[derive(Debug)]
enum EncodeError {
    InvalidOperands(ast::Span),
    OperandSizeNotSpecified(ast::Span),
    OperandSizesDoNotMatch(ast::Span),
    InvalidOperandSize(ast::Span),
    ImmediateOutOfRange(ast::Span),
    RelativeJumpOutOfRange(ast::Span),
}

fn encode(instruction: &ast::Instruction, offset: u16) -> Result<Vec<u8>, EncodeError> {
    use mrc_instruction::Operation::*;

    let mut result = vec![];

    match instruction.operation {
        ADD => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x00, instruction, offset, &mut result),
        OR => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x08, instruction, offset, &mut result),
        ADC => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x10, instruction, offset, &mut result),
        SBB => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x18, instruction, offset, &mut result),
        AND => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x20, instruction, offset, &mut result),
        SUB => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x28, instruction, offset, &mut result),
        XOR => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x30, instruction, offset, &mut result),
        CMP => encode_group_add_or_adc_sbb_and_sub_xor_cmp(0x38, instruction, offset, &mut result),

        NOT => encode_group_not_neg_mul_imul_div_idiv(0x02, instruction, offset, &mut result),
        NEG => encode_group_not_neg_mul_imul_div_idiv(0x03, instruction, offset, &mut result),
        MUL => encode_group_not_neg_mul_imul_div_idiv(0x04, instruction, offset, &mut result),
        IMUL => encode_group_not_neg_mul_imul_div_idiv(0x05, instruction, offset, &mut result),
        DIV => encode_group_not_neg_mul_imul_div_idiv(0x06, instruction, offset, &mut result),
        IDIV => encode_group_not_neg_mul_imul_div_idiv(0x07, instruction, offset, &mut result),

        MOV => encode_group_mov(0x00, instruction, offset, &mut result),

        TEST => encode_group_test(0x00, instruction, offset, &mut result),

        XCHG => encode_group_xchg(0x00, instruction, offset, &mut result),

        INC => encode_group_inc_dec(0x00, instruction, offset, &mut result),
        DEC => encode_group_inc_dec(0x01, instruction, offset, &mut result),

        PUSH => encode_group_push_pop(0x00, instruction, offset, &mut result),
        POP => encode_group_push_pop(0x00, instruction, offset, &mut result),

        RET => encode_group_ret_retn_retf(0xC2, instruction, offset, &mut result),
        // RETN => encode_group_ret_retn_retf(0xC2, instruction, offset, &mut result),
        // RETF => encode_group_ret_retn_retf(0x0CA, instruction, offset, &mut result),
        //
        LEA => encode_group_lea(0x00, instruction, offset, &mut result),

        LES => encode_group_les_lds(0xC4, instruction, offset, &mut result),
        LDS => encode_group_les_lds(0xC5, instruction, offset, &mut result),

        ROL => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x00, instruction, offset, &mut result),
        ROR => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x01, instruction, offset, &mut result),
        RCL => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x02, instruction, offset, &mut result),
        RCR => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x03, instruction, offset, &mut result),
        SHL => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x04, instruction, offset, &mut result),
        // SAL => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x04, instruction, offset, &mut result),
        SHR => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x05, instruction, offset, &mut result),
        SAR => encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(0x07, instruction, offset, &mut result),

        CALL => encode_group_call(0x00, instruction, offset, &mut result),

        JMP => encode_group_jmp(0x00, instruction, offset, &mut result),

        JO => encode_group_jcc(0x70, instruction, offset, &mut result),
        JNO => encode_group_jcc(0x71, instruction, offset, &mut result),
        JB => encode_group_jcc(0x72, instruction, offset, &mut result),
        JNB => encode_group_jcc(0x73, instruction, offset, &mut result),
        JE => encode_group_jcc(0x74, instruction, offset, &mut result),
        JNE => encode_group_jcc(0x75, instruction, offset, &mut result),
        JBE => encode_group_jcc(0x76, instruction, offset, &mut result),
        JNBE => encode_group_jcc(0x77, instruction, offset, &mut result),
        JS => encode_group_jcc(0x78, instruction, offset, &mut result),
        JNS => encode_group_jcc(0x79, instruction, offset, &mut result),
        JP => encode_group_jcc(0x7A, instruction, offset, &mut result),
        JNP => encode_group_jcc(0x7B, instruction, offset, &mut result),
        JL => encode_group_jcc(0x7C, instruction, offset, &mut result),
        JNL => encode_group_jcc(0x7D, instruction, offset, &mut result),
        JLE => encode_group_jcc(0x7E, instruction, offset, &mut result),
        JNLE => encode_group_jcc(0x7F, instruction, offset, &mut result),

        LOOPNZ => encode_group_loopc(0xE0, instruction, offset, &mut result),
        LOOPZ => encode_group_loopc(0xE1, instruction, offset, &mut result),
        LOOP => encode_group_loopc(0xE2, instruction, offset, &mut result),
        JCXZ => encode_group_loopc(0xE3, instruction, offset, &mut result),

        DAA => encode_group_no_operands(0x27, instruction, offset, &mut result),
        DAS => encode_group_no_operands(0x2F, instruction, offset, &mut result),
        AAA => encode_group_no_operands(0x37, instruction, offset, &mut result),
        AAS => encode_group_no_operands(0x3F, instruction, offset, &mut result),
        NOP => encode_group_no_operands(0x90, instruction, offset, &mut result),
        CBW => encode_group_no_operands(0x98, instruction, offset, &mut result),
        CWD => encode_group_no_operands(0x99, instruction, offset, &mut result),
        INT3 => encode_group_no_operands(0xCC, instruction, offset, &mut result),
        INTO => encode_group_no_operands(0xCE, instruction, offset, &mut result),
        IRET => encode_group_no_operands(0xCF, instruction, offset, &mut result),
        SALC => encode_group_no_operands(0xD6, instruction, offset, &mut result),
        HLT => encode_group_no_operands(0xF4, instruction, offset, &mut result),
        CMC => encode_group_no_operands(0xF5, instruction, offset, &mut result),
        CLC => encode_group_no_operands(0xF8, instruction, offset, &mut result),
        STC => encode_group_no_operands(0xF9, instruction, offset, &mut result),
        CLI => encode_group_no_operands(0xFA, instruction, offset, &mut result),
        STI => encode_group_no_operands(0xFB, instruction, offset, &mut result),
        CLD => encode_group_no_operands(0xFC, instruction, offset, &mut result),
        STD => encode_group_no_operands(0xFD, instruction, offset, &mut result),
        PUSHF => encode_group_no_operands(0x9C, instruction, offset, &mut result),
        POPF => encode_group_no_operands(0x9D, instruction, offset, &mut result),
        SAHF => encode_group_no_operands(0x9E, instruction, offset, &mut result),
        LAHF => encode_group_no_operands(0x9F, instruction, offset, &mut result),
        MOVSB => encode_group_no_operands(0xA4, instruction, offset, &mut result),
        CMPSB => encode_group_no_operands(0xA6, instruction, offset, &mut result),
        STOSB => encode_group_no_operands(0xAA, instruction, offset, &mut result),
        LODSB => encode_group_no_operands(0xAC, instruction, offset, &mut result),
        SCASB => encode_group_no_operands(0xAE, instruction, offset, &mut result),
        // XLATB => encode_group_no_operands(0xD7, instruction, offset, &mut result),
        MOVSW => encode_group_no_operands(0xA5, instruction, offset, &mut result),
        CMPSW => encode_group_no_operands(0xA7, instruction, offset, &mut result),
        STOSW => encode_group_no_operands(0xAB, instruction, offset, &mut result),
        LODSW => encode_group_no_operands(0xAD, instruction, offset, &mut result),
        SCASW => encode_group_no_operands(0xAF, instruction, offset, &mut result),

        LOCK => encode_group_prefixes(0xF0, instruction, offset, &mut result),
        REPNE => encode_group_prefixes(0xF2, instruction, offset, &mut result),
        REP => encode_group_prefixes(0xF3, instruction, offset, &mut result),

        INT => encode_group_int(0x00, instruction, offset, &mut result),

        AAM => encode_group_aam(0x00, instruction, offset, &mut result),

        AAD => encode_group_aad(0x00, instruction, offset, &mut result),

        XLAT => encode_group_xlat(0x00, instruction, offset, &mut result),

        IN => encode_group_in(0x00, instruction, offset, &mut result),

        OUT => encode_group_out(0x00, instruction, offset, &mut result),

        op => todo!("{:?}", op),
    };

    Ok(result)
}

fn parse_dst(operands: &ast::Operands) -> Result<(OperandData, ast::Span), EncodeError> {
    Ok(if let ast::Operands::Destination(_, dst) = &operands {
        (OperandData::from(dst), dst.span().clone())
    } else {
        return Err(EncodeError::InvalidOperands(operands.span().clone()));
    })
}

fn parse_dst_and_src(
    operands: &ast::Operands,
) -> Result<(OperandData, ast::Span, OperandData, ast::Span), EncodeError> {
    Ok(
        if let ast::Operands::DestinationAndSource(_, dst, src) = &operands {
            (
                OperandData::from(dst),
                dst.span().clone(),
                OperandData::from(src),
                src.span().clone(),
            )
        } else {
            return Err(EncodeError::InvalidOperands(operands.span().clone()));
        },
    )
}

fn encode_group_add_or_adc_sbb_and_sub_xor_cmp(
    base: u8,
    insn: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let (dst, dst_span, src, src_span) = parse_dst_and_src(&insn.operands)?;
    let oprs_span = insn.operands.span().clone();

    if dst.size.is_unspecified() && src.size.is_unspecified() {
        return Err(EncodeError::OperandSizeNotSpecified(oprs_span));
    }

    if dst.size.is_specified() && src.size.is_specified() && dst.size != src.size {
        return Err(EncodeError::OperandSizesDoNotMatch(oprs_span));
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
                            return Err(EncodeError::ImmediateOutOfRange(src_span));
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
                            return Err(EncodeError::ImmediateOutOfRange(src_span));
                        }
                        store_instruction(0x80, &dst, rm, OperandSize::Byte, src.imm, emitter);
                    }
                }

                _ => unreachable!(),
            }
        }

        _ => return Err(EncodeError::InvalidOperands(oprs_span)),
    }

    Ok(())
}

fn encode_group_not_neg_mul_imul_div_idiv(
    base: u8,
    insn: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let (dst, dst_span) = parse_dst(&insn.operands)?;
    let oprs_span = insn.operands.span().clone();

    if dst.size.is_unspecified() {
        return Err(EncodeError::OperandSizeNotSpecified(oprs_span));
    }

    if !matches!(dst.kind, OperandKind::Mem | OperandKind::Reg) {
        return Err(EncodeError::InvalidOperands(oprs_span));
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
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_test(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_xchg(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_inc_dec(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_push_pop(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_ret_retn_retf(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_lea(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_les_lds(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_call(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_jmp(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_jcc(
    base: u8,
    insn: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let (dst, dst_span) = parse_dst(&insn.operands)?;

    if dst.kind != OperandKind::Imm && (!matches!(dst.jmp_kind, Some(JumpKind::Short))) {
        return Err(EncodeError::InvalidOperands(dst_span));
    }

    if dst.imm < u16::MIN as i32 || dst.imm >= u16::MAX as i32 {
        return Err(EncodeError::RelativeJumpOutOfRange(dst_span));
    }

    let jmp_dst = dst.imm - (offset as i32 + 2);

    if jmp_dst < i8::MIN as i32 || jmp_dst > i8::MAX as i32 {
        return Err(EncodeError::RelativeJumpOutOfRange(dst_span));
    }

    emitter.emit(base);
    emitter.emit(jmp_dst as i8 as u8);

    Ok(())
}

fn encode_group_loopc(
    base: u8,
    insn: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let (dst, dst_span) = parse_dst(&insn.operands)?;

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
                return Err(EncodeError::RelativeJumpOutOfRange(dst_span));
            }
        }

        _ => return Err(EncodeError::InvalidOperands(insn.operands.span().clone())),
    }

    Ok(())
}

fn encode_group_no_operands(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    emitter.emit(base);
    Ok(())
}

fn encode_group_prefixes(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    emitter.emit(base);
    Ok(())
}

fn encode_group_int(
    base: u8,
    insn: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let (dst, dst_span) = parse_dst(&insn.operands)?;

    match dst.kind {
        OperandKind::Imm => {
            emitter.emit(0xCD);

            if dst.imm >= u8::MIN as i32 && dst.imm <= u8::MAX as i32 {
                emitter.emit(dst.imm as u8);
            } else {
                return Err(EncodeError::ImmediateOutOfRange(dst_span));
            }
        }

        _ => return Err(EncodeError::InvalidOperands(dst_span)),
    }

    Ok(())
}

fn encode_group_aam(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    emitter.emit(0xD4);
    emitter.emit(0xA0);
    Ok(())
}

fn encode_group_aad(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    emitter.emit(0xD5);
    emitter.emit(0xA0);
    Ok(())
}

fn encode_group_movs(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_cmps(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_stos(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_lods(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_scas(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    todo!()
}

fn encode_group_xlat(
    base: u8,
    insn: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let (dst, dst_span) = parse_dst(&insn.operands)?;

    if dst.size != OperandSize::Word {
        return Err(EncodeError::InvalidOperandSize(dst_span));
    }

    match dst.kind {
        OperandKind::Mem if dst.mode == 0 && dst.rm == 7 => {
            if dst.segment_prefix != 0 && dst.segment_prefix != 0x3E {
                emitter.emit(dst.segment_prefix);
            }
            emitter.emit(0xD7);
        }

        _ => return Err(EncodeError::InvalidOperands(insn.operands.span().clone())),
    }

    Ok(())
}

fn encode_group_in(
    base: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let (dst, dst_span, src, src_span) = parse_dst_and_src(&instruction.operands)?;

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

        _ => {
            return Err(EncodeError::InvalidOperands(
                instruction.operands.span().clone(),
            ))
        }
    }

    Ok(())
}

fn encode_group_out(
    base: u8,
    insn: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let (dst, dst_span, src, src_span) = parse_dst_and_src(&insn.operands)?;

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

        _ => return Err(EncodeError::InvalidOperands(insn.operands.span().clone())),
    }

    Ok(())
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum OperandKind {
    Imm,
    Reg,
    Mem,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
enum OperandSize {
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
enum JumpKind {
    Far,
    Near,
    Short,
}

#[derive(Debug)]
struct OperandData {
    size: OperandSize,
    kind: OperandKind,
    segment_prefix: u8,
    imm: i32,
    // unresolved: u8,
    displacement: i32,
    displacement_size: OperandSize,
    // address: u8,
    // address_registers: u8,
    // segment: u8,
    // offset: u8,
    jmp_kind: Option<JumpKind>,
    mode: u8,
    rm: u8,
}

impl OperandData {
    fn from(operand: &ast::Operand) -> Self {
        match operand {
            ast::Operand::Immediate(_, expr) => {
                if let ast::Expression::Value(span, ast::Value::Constant(imm)) = expr {
                    Self {
                        size: OperandSize::Unspecified,
                        kind: OperandKind::Imm,
                        segment_prefix: 0,
                        imm: *imm,
                        displacement: 0,
                        displacement_size: OperandSize::Unspecified,
                        jmp_kind: None,
                        mode: 0,
                        rm: 0,
                    }
                } else {
                    panic!("Expression value is not a constant");
                }
            }

            ast::Operand::Register(span, reg) => Self {
                size: match reg {
                    ast::Register::Byte(_) => OperandSize::Byte,
                    ast::Register::Word(_) => OperandSize::Word,
                },
                kind: OperandKind::Reg,
                segment_prefix: 0,
                imm: 0,
                displacement: 0,
                displacement_size: OperandSize::Unspecified,
                jmp_kind: None,
                mode: 0b11,
                rm: reg.encoding(),
            },

            ast::Operand::Address(span, expr, data_size, seg) => {
                let address = if let ast::Expression::Value(_, ast::Value::Constant(value)) = expr {
                    *value
                } else {
                    panic!("Expression value is not a constant");
                };

                let size = Self::operand_size_from_data_size(data_size);

                let segment_prefix = if let Some(sp) = seg {
                    0x26 + (sp.encoding() << 3)
                } else {
                    0
                };

                Self {
                    size,
                    kind: OperandKind::Mem,
                    segment_prefix,
                    imm: 0,
                    displacement: address,
                    displacement_size: OperandSize::Word,
                    jmp_kind: None,
                    mode: 0b00,
                    rm: 0b110,
                }
            }

            ast::Operand::Indirect(_, indirect_encoding, expr, data_size, seg) => {
                let size = Self::operand_size_from_data_size(data_size);

                let segment_prefix = if let Some(seg) = seg {
                    0x26 + (seg.encoding() << 3)
                } else {
                    0
                };

                let (mode, displacement, displacement_size) = match expr {
                    Some(ast::Expression::Value(_, ast::Value::Constant(value))) => {
                        if *value >= i8::MIN as i32 && *value <= i8::MAX as i32 {
                            (0b01, *value, OperandSize::Byte)
                        } else if *value >= i16::MIN as i32 && *value <= i16::MAX as i32 {
                            (0b10, *value, OperandSize::Word)
                        } else {
                            panic!("immediate value out of range");
                        }
                    }

                    Some(_) => panic!("Expression does not contain a constant value"),

                    None => (0b00_u8, 0_i32, OperandSize::Unspecified),
                };

                Self {
                    size,
                    kind: OperandKind::Mem,
                    segment_prefix,
                    imm: 0,
                    displacement,
                    displacement_size,
                    jmp_kind: None,
                    mode,
                    rm: indirect_encoding.encoding(),
                }
            }

            todo => todo!("{:?}", todo),
        }
    }

    fn operand_size_from_data_size(data_size: &Option<DataSize>) -> OperandSize {
        if let Some(data_size) = data_size {
            match data_size {
                ast::DataSize::Byte => OperandSize::Byte,
                ast::DataSize::Word => OperandSize::Word,
            }
        } else {
            OperandSize::Unspecified
        }
    }
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
            ast::Operand::Address(
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
