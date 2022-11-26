mod gen;

use super::ast;
use crate::operations::Operation;
use gen::{emit_codes, Code, FIRST_OPER_DST, FIRST_OPER_SRC};
use std::fmt::{Display, Formatter};

const EMIT_DEFAULT_SEGMENTS: bool = true;

macro_rules! value_ops {
    ($is_name:ident, $require_name:ident, $typ:ty) => {
        #[allow(unused)]
        #[inline(always)]
        pub fn $is_name(value: i32) -> bool {
            value >= <$typ>::MIN as i32 && value <= <$typ>::MAX as i32
        }

        #[allow(unused)]
        #[inline(always)]
        pub fn $require_name(value: i32, span: &ast::Span) -> Result<$typ, EncodeError> {
            if $is_name(value) {
                Ok(value as $typ)
            } else {
                Err(EncodeError::ImmediateOutOfRange(
                    span.clone(),
                    value,
                    <$typ>::MIN as i32,
                    <$typ>::MAX as i32,
                ))
            }
        }
    };
}

value_ops!(value_is_byte, require_value_is_byte, u8);
value_ops!(value_is_signed_byte, require_value_is_signed_byte, i8);
value_ops!(value_is_word, require_value_is_word, u16);
value_ops!(value_is_signed_word, require_value_is_signed_word, i16);

pub trait ByteEmitter {
    fn emit(&mut self, byte: u8);
}

impl ByteEmitter for Vec<u8> {
    fn emit(&mut self, byte: u8) {
        self.push(byte);
    }
}

impl ByteEmitter for u16 {
    fn emit(&mut self, _byte: u8) {
        *self += 1;
    }
}

#[derive(Debug)]
pub enum EncodeError {
    InvalidOperands(ast::Span),
    OperandSizeNotSpecified(ast::Span),
    OperandSizesDoNotMatch(ast::Span),
    InvalidOperandSize(ast::Span),
    ImmediateOutOfRange(ast::Span, i32, i32, i32),
    RelativeJumpOutOfRange(ast::Span, OperandSize, bool, i32),
}

impl EncodeError {
    pub fn span(&self) -> &ast::Span {
        match self {
            EncodeError::InvalidOperands(span)
            | EncodeError::OperandSizeNotSpecified(span)
            | EncodeError::OperandSizesDoNotMatch(span)
            | EncodeError::InvalidOperandSize(span)
            | EncodeError::ImmediateOutOfRange(span, _, _, _)
            | EncodeError::RelativeJumpOutOfRange(span, _, _, _) => span,
        }
    }
}

impl Display for EncodeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EncodeError::InvalidOperands(_) => write!(f, "Invalid operands."),

            EncodeError::OperandSizeNotSpecified(_) => write!(f, "Operand size not specified."),

            EncodeError::OperandSizesDoNotMatch(_) => write!(f, "Operand sizes does not match."),

            EncodeError::InvalidOperandSize(_) => write!(f, "Invalid operand size."),

            EncodeError::ImmediateOutOfRange(_, value, min, max) => write!(
                f,
                "Value out of range. ({} should be in range {}..{})",
                value, min, max
            ),

            EncodeError::RelativeJumpOutOfRange(_, size, signed, value) => {
                write!(
                    f,
                    "Relative jump out of range. ({} does not fit into {} {})",
                    value,
                    if *signed { "signed" } else { "unsigned" },
                    match size {
                        OperandSize::Unspecified => "value with unspecified size",
                        OperandSize::Byte => "byte",
                        OperandSize::Word => "word",
                    }
                )
            }
        }
    }
}

const fn segment_prefix_for(seg: ast::Segment) -> u8 {
    0x26 + ((seg as u8) << 3)
}

pub fn encode(
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    use crate::operations::Operation::*;

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

        XLATB => encode_group_xlat(0x00, insn, offset, emitter),

        IN => encode_group_in(0x00, insn, offset, emitter),

        OUT => encode_group_out(0x00, insn, offset, emitter),

        op => todo!("{:?}", op),
    }
}

fn emit_segment_prefix(
    segment_prefix: u8,
    default_segment: ast::Segment,
    emitter: &mut impl ByteEmitter,
) {
    if segment_prefix != 0 {
        if segment_prefix == segment_prefix_for(default_segment) && !EMIT_DEFAULT_SEGMENTS {
        } else {
            emitter.emit(segment_prefix);
        }
    }
}

fn encode_group_add_or_adc_sbb_and_sub_xor_cmp(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, src] = &insn.opers;
    let size = common_operand_size(dst, src, &insn.opers_span)?;

    match (dst.kind, src.kind) {
        (OperandKind::Reg | OperandKind::Mem, OperandKind::Reg) if size == OperandSize::Byte => {
            emit_codes(emitter, insn, offset, &[Code::ModRegRM(0, base)])
        }

        (OperandKind::Reg | OperandKind::Mem, OperandKind::Reg) if size == OperandSize::Word => {
            emit_codes(emitter, insn, offset, &[Code::ModRegRM(0, base + 1)])
        }

        (OperandKind::Reg, OperandKind::Mem) if size == OperandSize::Byte => {
            emit_codes(emitter, insn, offset, &[Code::ModRegRM(1, base + 2)])
        }

        (OperandKind::Reg, OperandKind::Mem) if size == OperandSize::Word => {
            emit_codes(emitter, insn, offset, &[Code::ModRegRM(1, base + 2 + 1)])
        }

        (OperandKind::Reg | OperandKind::Mem, OperandKind::Imm) => {
            match size {
                OperandSize::Word => {
                    if value_is_signed_byte(src.imm) {
                        // ax, simm8
                        emit_codes(
                            emitter,
                            insn,
                            offset,
                            &[
                                Code::ModRM(0, 0x83, base >> 3),
                                Code::ImmByte(FIRST_OPER_SRC),
                            ],
                        )
                    } else if dst.kind == OperandKind::Reg && dst.rm == 0 {
                        // AX, imm
                        emit_codes(
                            emitter,
                            insn,
                            offset,
                            &[Code::Byte(base + 5), Code::ImmWord(FIRST_OPER_SRC)],
                        )
                    } else {
                        // reg16, imm16
                        emit_codes(
                            emitter,
                            insn,
                            offset,
                            &[
                                Code::ModRM(FIRST_OPER_DST, 0x81, base >> 3),
                                Code::ImmWord(FIRST_OPER_SRC),
                            ],
                        )
                    }
                }

                OperandSize::Byte => {
                    if dst.kind == OperandKind::Reg && dst.rm == 0 {
                        // AL, imm
                        emit_codes(
                            emitter,
                            insn,
                            offset,
                            &[Code::Byte(base + 4), Code::ImmByte(FIRST_OPER_SRC)],
                        )
                    } else {
                        // reg8, imm
                        emit_codes(
                            emitter,
                            insn,
                            offset,
                            &[
                                Code::ModRM(FIRST_OPER_DST, 0x80, base >> 3),
                                Code::ImmByte(FIRST_OPER_SRC),
                            ],
                        )
                    }
                }

                _ => unreachable!(),
            }
        }

        _ => Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }
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

    match dst.kind {
        OperandKind::Reg | OperandKind::Mem if dst.size == OperandSize::Byte => emit_codes(
            emitter,
            insn,
            offset,
            &[Code::ModRM(FIRST_OPER_DST, 0xF6, base)],
        ),

        OperandKind::Reg | OperandKind::Mem if dst.size == OperandSize::Word => emit_codes(
            emitter,
            insn,
            offset,
            &[Code::ModRM(FIRST_OPER_DST, 0xF7, base)],
        ),

        _ => Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }
}

fn encode_group_mov(
    _base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, src] = &insn.opers;
    let size = common_operand_size(dst, src, &insn.opers_span)?;

    match (dst.kind, src.kind) {
        (OperandKind::Reg | OperandKind::Mem, OperandKind::Reg)
            if size == OperandSize::Byte && dst.is_direct() && src.rm == 0 =>
        {
            // mem_offs, al
            emit_segment_prefix(dst.segment_prefix, ast::Segment::DS, emitter);
            emit_codes(
                emitter,
                insn,
                offset,
                &[Code::Byte(0xA2), Code::DispWord(FIRST_OPER_DST)],
            )
        }

        (OperandKind::Reg | OperandKind::Mem, OperandKind::Reg)
            if size == OperandSize::Word && dst.is_direct() && src.rm == 0 =>
        {
            // mem_offs, ax
            emit_segment_prefix(dst.segment_prefix, ast::Segment::DS, emitter);
            emit_codes(
                emitter,
                insn,
                offset,
                &[Code::Byte(0xA3), Code::DispWord(FIRST_OPER_DST)],
            )
        }

        (OperandKind::Reg | OperandKind::Mem, OperandKind::Reg) => match size {
            OperandSize::Byte => {
                emit_mod_reg_rm(0x88, dst, src.rm, OperandSize::Unspecified, 0, emitter);
                Ok(())
            }

            OperandSize::Word => {
                emit_mod_reg_rm(0x89, dst, src.rm, OperandSize::Unspecified, 0, emitter);
                Ok(())
            }

            OperandSize::Unspecified => unreachable!(),
        },

        (OperandKind::Reg, OperandKind::Mem) => {
            // reg, mem
            if dst.rm == 0 && src.is_direct() {
                // al, mem
                emit_segment_prefix(src.segment_prefix, ast::Segment::DS, emitter);

                match size {
                    OperandSize::Byte => {
                        emitter.emit(0xA0);
                    }

                    OperandSize::Word => {
                        emitter.emit(0xA1);
                    }

                    OperandSize::Unspecified => unreachable!(),
                }

                let value = require_value_is_word(src.displacement, &src.span)?;
                for byte in value.to_le_bytes() {
                    emitter.emit(byte);
                }

                Ok(())
            } else {
                match size {
                    OperandSize::Byte => {
                        emit_mod_reg_rm(0x8A, src, dst.rm, OperandSize::Unspecified, 0, emitter);
                        Ok(())
                    }

                    OperandSize::Word => {
                        emit_mod_reg_rm(0x8B, src, dst.rm, OperandSize::Unspecified, 0, emitter);
                        Ok(())
                    }

                    OperandSize::Unspecified => unreachable!(),
                }
            }
        }

        (OperandKind::Reg | OperandKind::Mem, OperandKind::Imm) => {
            // reg/mem, imm

            match dst.kind {
                OperandKind::Mem => match size {
                    OperandSize::Byte => {
                        emit_mod_reg_rm(0xC6, dst, 0, OperandSize::Byte, src.imm, emitter);
                        Ok(())
                    }

                    OperandSize::Word => {
                        emit_mod_reg_rm(0xC7, dst, 0, OperandSize::Word, src.imm, emitter);
                        Ok(())
                    }

                    OperandSize::Unspecified => unreachable!(),
                },

                OperandKind::Reg => match size {
                    OperandSize::Byte => {
                        let value = require_value_is_byte(src.imm, &src.span)?;
                        emitter.emit(0xB0 + dst.rm);
                        emitter.emit(value);
                        Ok(())
                    }

                    OperandSize::Word => {
                        let value = require_value_is_word(src.imm, &src.span)?;
                        emitter.emit(0xB8 + dst.rm);
                        for byte in value.to_le_bytes() {
                            emitter.emit(byte);
                        }
                        Ok(())
                    }

                    OperandSize::Unspecified => unreachable!(),
                },
                _ => unreachable!(),
            }
        }

        (OperandKind::Reg | OperandKind::Mem, OperandKind::Seg) => {
            // reg/mem, seg
            emit_mod_reg_rm(0x8C, dst, src.rm, OperandSize::Unspecified, 0, emitter);
            Ok(())
        }

        (OperandKind::Seg, OperandKind::Reg | OperandKind::Mem) if dst.rm != 1 => {
            // seg, reg/mem
            emit_mod_reg_rm(0x8E, src, dst.rm, OperandSize::Unspecified, 0, emitter);
            Ok(())
        }

        _ => Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }
}

fn common_operand_size(
    dst: &OperandData,
    src: &OperandData,
    opers_span: &ast::Span,
) -> Result<OperandSize, EncodeError> {
    if dst.size.is_unspecified() && src.size.is_unspecified() {
        return Err(EncodeError::OperandSizeNotSpecified(opers_span.clone()));
    }

    if dst.size.is_specified() && src.size.is_specified() && dst.size != src.size {
        return Err(EncodeError::OperandSizesDoNotMatch(opers_span.clone()));
    }

    let size = if dst.size.is_specified() {
        dst.size
    } else {
        src.size
    };

    Ok(size)
}

fn encode_group_test(
    _base: u8,
    insn: &InstructionData,
    _offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, src] = &insn.opers;
    let size = common_operand_size(dst, src, &insn.opers_span)?;

    match (dst.kind, src.kind) {
        (OperandKind::Reg | OperandKind::Mem, OperandKind::Reg) => {
            todo!()
        }

        (OperandKind::Reg, OperandKind::Mem) => {
            todo!()
        }

        (OperandKind::Reg | OperandKind::Mem, OperandKind::Imm) => {
            match size {
                OperandSize::Byte => {
                    if dst.kind == OperandKind::Reg && dst.rm == 0 {
                        let value = require_value_is_byte(dst.imm, &dst.span)?;
                        emitter.emit(0xA8);
                        emitter.emit(value);
                        Ok(())
                    } else {
                        // x86.store_instruction@dest, (0F6h),(0),byte,@src.imm
                        todo!()
                    }
                }

                OperandSize::Word => {
                    todo!()
                }

                OperandSize::Unspecified => unreachable!(),
            }
        }

        _ => Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }
}

fn encode_group_xchg(
    _base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, src] = &insn.opers;
    let size = common_operand_size(dst, src, &insn.opers_span)?;

    match (dst.kind, src.kind) {
        (OperandKind::Reg, OperandKind::Reg)
            if size == OperandSize::Word && dst.rm == 0 || src.rm == 0 =>
        {
            emit_codes(emitter, insn, offset, &[Code::Byte(0x90 + dst.rm + src.rm)])
        }

        (OperandKind::Reg, OperandKind::Reg) | (OperandKind::Reg, OperandKind::Mem)
            if size == OperandSize::Byte =>
        {
            emit_codes(
                emitter,
                insn,
                offset,
                &[Code::ModRegRM(FIRST_OPER_SRC, 0x86)],
            )
        }

        (OperandKind::Reg, OperandKind::Reg) | (OperandKind::Reg, OperandKind::Mem)
            if size == OperandSize::Word =>
        {
            emit_codes(
                emitter,
                insn,
                offset,
                &[Code::ModRegRM(FIRST_OPER_SRC, 0x87)],
            )
        }

        (OperandKind::Mem, OperandKind::Reg) if size == OperandSize::Byte => emit_codes(
            emitter,
            insn,
            offset,
            &[Code::ModRegRM(FIRST_OPER_DST, 0x86)],
        ),

        (OperandKind::Mem, OperandKind::Reg) if size == OperandSize::Word => emit_codes(
            emitter,
            insn,
            offset,
            &[Code::ModRegRM(FIRST_OPER_DST, 0x87)],
        ),

        _ => Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }
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
        OperandKind::Reg if dst.size == OperandSize::Byte => emit_codes(
            emitter,
            insn,
            offset,
            &[Code::ModRM(FIRST_OPER_DST, 0xFE, base)],
        ),

        OperandKind::Reg if dst.size == OperandSize::Word => emit_codes(
            emitter,
            insn,
            offset,
            &[Code::Byte(0x40 + dst.rm + (base << 3))],
        ),

        OperandKind::Mem if dst.size == OperandSize::Byte => emit_codes(
            emitter,
            insn,
            offset,
            &[Code::ModRM(FIRST_OPER_DST, 0xFE, base)],
        ),

        OperandKind::Mem if dst.size == OperandSize::Word => emit_codes(
            emitter,
            insn,
            offset,
            &[Code::ModRM(FIRST_OPER_DST, 0xFF, base)],
        ),

        _ => Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }
}

fn encode_group_push(
    _base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    match dst.kind {
        OperandKind::Mem if dst.size == OperandSize::Word => {
            todo!()
        }

        OperandKind::Reg if dst.size == OperandSize::Word => {
            emit_codes(emitter, insn, offset, &[Code::Byte(0x50 + dst.rm)])
        }

        OperandKind::Seg => emit_codes(emitter, insn, offset, &[Code::Byte(0b110 + (dst.rm << 3))]),

        _ => Err(EncodeError::InvalidOperands(dst.span.clone())),
    }
}

fn encode_group_pop(
    _base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    if dst.size != OperandSize::Word {
        return Err(EncodeError::InvalidOperandSize(dst.span.clone()));
    }

    match dst.kind {
        OperandKind::Mem if dst.size == OperandSize::Word => {
            todo!()
        }

        OperandKind::Reg if dst.size == OperandSize::Word => {
            emit_codes(emitter, insn, offset, &[Code::Byte(0x58 + dst.rm)])
        }

        OperandKind::Seg => emit_codes(emitter, insn, offset, &[Code::Byte(0b111 + (dst.rm << 3))]),

        _ => Err(EncodeError::InvalidOperandSize(dst.span.clone())),
    }
}

fn encode_group_ret_retn_retf(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    if insn.num_opers == 0 {
        emit_codes(emitter, insn, offset, &[Code::Byte(base + 1)])
    } else {
        // let [dst, _] = &insn.opers;

        // Other ret types.
        todo!()
    }
}

fn encode_group_lea(
    _base: u8,
    _insn: &InstructionData,
    _offset: u16,
    _emitter: &mut impl ByteEmitter,
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

    match (dst.kind, src.kind) {
        (OperandKind::Reg, OperandKind::Mem) if dst.size == OperandSize::Word => emit_codes(
            emitter,
            insn,
            offset,
            &[Code::ModRegRM(FIRST_OPER_SRC, base)],
        ),

        _ => return Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }
}

fn encode_group_rol_ror_rcl_rcr_shl_sal_shr_sar(
    base: u8,
    insn: &InstructionData,
    _offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, src] = &insn.opers;

    if dst.size.is_unspecified() {
        return Err(EncodeError::OperandSizeNotSpecified(dst.span.clone()));
    }

    match (dst.kind, src.kind) {
        (OperandKind::Reg | OperandKind::Mem, OperandKind::Reg)
            if src.rm == 1 && src.size == OperandSize::Byte =>
        {
            match dst.size {
                OperandSize::Byte => {
                    emit_mod_reg_rm(0xD2, dst, base, OperandSize::Unspecified, 0, emitter);
                    Ok(())
                }

                OperandSize::Word => {
                    emit_mod_reg_rm(0xD3, dst, base, OperandSize::Unspecified, 0, emitter);
                    Ok(())
                }

                OperandSize::Unspecified => unreachable!(),
            }
        }

        (OperandKind::Reg | OperandKind::Mem, OperandKind::Imm) if src.imm == 1 => match dst.size {
            OperandSize::Byte => {
                emit_mod_reg_rm(0xD0, dst, base, OperandSize::Unspecified, 0, emitter);
                Ok(())
            }

            OperandSize::Word => {
                emit_mod_reg_rm(0xD1, dst, base, OperandSize::Unspecified, 0, emitter);
                Ok(())
            }

            OperandSize::Unspecified => unreachable!(),
        },

        _ => Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }
}

fn encode_group_call(
    _base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    match dst.kind {
        OperandKind::Imm if dst.jmp_kind.unwrap_or(JumpKind::Near) == JumpKind::Near => {
            let value = require_value_is_word(dst.imm, &dst.span)?;
            let jmp_dst = value.wrapping_sub(offset + 3);
            emitter.emit(0xE8);
            for byte in jmp_dst.to_le_bytes() {
                emitter.emit(byte);
            }
            Ok(())
        }

        OperandKind::Mem if dst.jmp_kind.unwrap_or(JumpKind::Near) == JumpKind::Far => {
            todo!()
            // call_direct_far:
            //     check	@dest.jump_type & @dest.jump_type <> 'far'
            // jyes	invalid_operand
            // check	@dest.size and not 4
            // jyes	invalid_operand
            // asmcmd	=db 9Ah
            // asmcmd	=dw =@dest.=offset,=@dest.=segment
            // exit
        }

        OperandKind::Mem | OperandKind::Reg => emit_codes(
            emitter,
            insn,
            offset,
            &[Code::ModRM(FIRST_OPER_DST, 0xFF, 0x02)],
        ),

        _ => Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }
}

fn encode_group_jmp(
    _base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    match dst.kind {
        OperandKind::Imm => {
            // The value in the dst.imm field is an absolute address within the same segment.
            let value = require_value_is_word(dst.imm, &dst.span)?;

            match dst.jmp_kind {
                Some(JumpKind::Short) => {
                    todo!()
                }

                None | Some(JumpKind::Near) => {
                    emitter.emit(0xE9);
                    let rel = value.wrapping_sub(offset + 3);
                    for byte in rel.to_le_bytes() {
                        emitter.emit(byte);
                    }
                }

                Some(JumpKind::Far) => {
                    let segment = require_value_is_word(dst.displacement, &dst.span)?;

                    emitter.emit(0xEA);
                    for byte in value.to_le_bytes() {
                        emitter.emit(byte);
                    }
                    for byte in segment.to_le_bytes() {
                        emitter.emit(byte);
                    }
                }
            }
        }

        OperandKind::Mem | OperandKind::Reg => {
            // FIXME: We are assuming the operand size to be word, but should we actually check if
            //        that is true?
            if let JumpKind::Near = dst.jmp_kind.unwrap_or(JumpKind::Near) {
                emit_mod_reg_rm(0xFF, dst, 0b100, OperandSize::Unspecified, 0, emitter);
            } else {
                return Err(EncodeError::InvalidOperands(dst.span.clone()));
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

    match dst.kind {
        OperandKind::Imm if dst.jmp_kind.unwrap_or(JumpKind::Short) == JumpKind::Short => {
            emit_codes(
                emitter,
                insn,
                offset,
                &[Code::Byte(base), Code::RelByte(FIRST_OPER_DST, 2)],
            )
        }

        _ => Err(EncodeError::InvalidOperands(dst.span.clone())),
    }
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
            emit_codes(
                emitter,
                insn,
                offset,
                &[Code::Byte(base), Code::RelByte(0, 2)],
            )?;
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
    emit_codes(emitter, insn, offset, &[Code::Byte(base)])
}

fn encode_group_prefixes(
    base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    emit_codes(emitter, insn, offset, &[Code::Byte(base)])
}

fn encode_group_int(
    _base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    match dst.kind {
        OperandKind::Imm => emit_codes(
            emitter,
            insn,
            offset,
            &[Code::Byte(0xCD), Code::ImmByte(FIRST_OPER_DST)],
        ),

        _ => Err(EncodeError::InvalidOperands(dst.span.clone())),
    }
}

fn encode_group_aam(
    _base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    emit_codes(emitter, insn, offset, &[Code::Byte(0xD4), Code::Byte(0xA0)])
}

fn encode_group_aad(
    _base: u8,
    insn: &InstructionData,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    emit_codes(emitter, insn, offset, &[Code::Byte(0xD5), Code::Byte(0xA0)])
}

fn encode_group_xlat(
    _base: u8,
    insn: &InstructionData,
    _offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let [dst, _] = &insn.opers;

    if dst.size != OperandSize::Word {
        return Err(EncodeError::InvalidOperandSize(dst.span.clone()));
    }

    match dst.kind {
        OperandKind::Mem if dst.mode == 0 && dst.rm == 7 => {
            emit_segment_prefix(dst.segment_prefix, ast::Segment::DS, emitter);
            emitter.emit(0xD7);
        }

        _ => return Err(EncodeError::InvalidOperands(insn.opers_span.clone())),
    }

    Ok(())
}

fn encode_group_in(
    _base: u8,
    insn: &InstructionData,
    _offset: u16,
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
    _base: u8,
    insn: &InstructionData,
    _offset: u16,
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

#[allow(dead_code)]
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum JumpKind {
    Far,
    Near,
    Short,
}

#[derive(Debug)]
pub struct OperandData {
    /// The span of the operand in the AST.
    pub span: ast::Span,

    /// The auto-detected size of the operand.
    pub size: OperandSize,

    /// What kind of operand?  Immediate? Register? Address?
    pub kind: OperandKind,

    /// Encoding for a segment prefix.  0 means NO prefix.
    pub segment_prefix: u8,

    /// An immediate value on the operand.
    pub imm: i32,

    /// Indirect displacement.
    pub displacement: i32,

    /// Indirect displacement size.  This is relative to the [mode] field.
    pub displacement_size: OperandSize,

    /// The kind of jump requested in the AST if any.
    pub jmp_kind: Option<JumpKind>,

    /// The mode used for mod reg r/m encoding.
    pub mode: u8,

    /// The encoding for the r/m field in the mod reg r/m encoding.
    pub rm: u8,
}

impl OperandData {
    pub fn empty(span: ast::Span) -> Self {
        Self {
            span,
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

    pub fn immediate(span: ast::Span, value: i32) -> Self {
        Self {
            span,
            size: OperandSize::Unspecified,
            kind: OperandKind::Imm,
            segment_prefix: 0,
            imm: value,
            displacement: 0,
            displacement_size: OperandSize::Unspecified,
            jmp_kind: None,
            mode: 0,
            rm: 0,
        }
    }

    pub fn register(span: ast::Span, encoding: u8, size: OperandSize) -> Self {
        Self {
            span,
            size,
            kind: OperandKind::Reg,
            segment_prefix: 0,
            imm: 0,
            displacement: 0,
            displacement_size: OperandSize::Unspecified,
            jmp_kind: None,
            mode: 0b11,
            rm: encoding,
        }
    }

    pub fn segment(span: ast::Span, encoding: u8) -> Self {
        Self {
            span: span.clone(),
            size: OperandSize::Word,
            kind: OperandKind::Seg,
            segment_prefix: 0,
            imm: 0,
            displacement: 0,
            displacement_size: OperandSize::Unspecified,
            jmp_kind: None,
            mode: 0b11,
            rm: encoding,
        }
    }

    pub fn direct(
        span: ast::Span,
        address: i32,
        data_size: &Option<ast::DataSize>,
        seg_override: &Option<ast::Segment>,
    ) -> Self {
        let size = operand_size_from_data_size(data_size);

        let segment_prefix = if let Some(sp) = seg_override {
            0x26 + (sp.encoding() << 3)
        } else {
            0
        };

        OperandData {
            span: span.clone(),
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

    pub fn indirect(
        span: ast::Span,
        addr_mode: u8,
        displacement: i16,
        data_size: &Option<ast::DataSize>,
        seg_override: &Option<ast::Segment>,
    ) -> Self {
        let size = operand_size_from_data_size(data_size);

        let segment_prefix = if let Some(seg) = seg_override {
            0x26 + (seg.encoding() << 3)
        } else {
            0
        };

        let (mode, displacement, displacement_size) = if displacement == 0 {
            (0b00_u8, 0_i16, OperandSize::Unspecified)
        } else if value_is_signed_byte(displacement as i32) {
            (0b01, displacement, OperandSize::Byte)
        } else {
            (0b10, displacement, OperandSize::Word)
        };

        OperandData {
            span: span.clone(),
            size,
            kind: OperandKind::Mem,
            segment_prefix,
            imm: 0,
            displacement: displacement as i32,
            displacement_size,
            jmp_kind: None,
            mode,
            rm: addr_mode,
        }
    }

    pub fn far(span: ast::Span, offset: i32, segment: i32) -> Self {
        OperandData {
            span: span.clone(),
            size: OperandSize::Unspecified,
            kind: OperandKind::Imm,
            segment_prefix: 0,
            imm: offset,
            displacement: segment,
            displacement_size: OperandSize::Unspecified,
            jmp_kind: Some(JumpKind::Far),
            mode: 0,
            rm: 0,
        }
    }

    pub fn is_direct(&self) -> bool {
        self.kind == OperandKind::Mem && self.mode == 0b00 && self.rm == 0b110
    }
}

pub fn operand_size_from_data_size(data_size: &Option<ast::DataSize>) -> OperandSize {
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

impl InstructionData {
    pub fn none(span: ast::Span, op: Operation) -> Self {
        Self {
            operation: op,
            num_opers: 0,
            opers: [
                OperandData::empty(span.clone()),
                OperandData::empty(span.clone()),
            ],
            opers_span: span,
        }
    }

    pub fn dst(span: ast::Span, op: Operation, dst: OperandData) -> Self {
        Self {
            operation: op,
            num_opers: 1,
            opers: [dst, OperandData::empty(span.clone())],
            opers_span: span,
        }
    }

    pub fn dst_and_src(span: ast::Span, op: Operation, dst: OperandData, src: OperandData) -> Self {
        Self {
            operation: op,
            num_opers: 2,
            opers: [dst, src],
            opers_span: span,
        }
    }
}

fn emit_mod_reg_rm(
    op_code: u8,
    rm: &OperandData,
    reg: u8,
    imm_size: OperandSize,
    immediate: i32,
    emitter: &mut impl ByteEmitter,
) {
    // Any indirect address referencing BP uses SS as the default segment, all others use DS.
    emit_segment_prefix(
        rm.segment_prefix,
        if rm.rm == 2 || rm.rm == 3 || (rm.mode > 0 && rm.rm == 6) {
            ast::Segment::SS
        } else {
            ast::Segment::DS
        },
        emitter,
    );

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
    use crate::operations::Operation;
    use std::fmt::{Debug, Formatter};

    #[derive(PartialEq)]
    struct DebugBytes<'a>(&'a [u8]);

    impl Debug for DebugBytes<'_> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "[")?;
            let mut first = true;
            for value in self.0.iter() {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                write!(f, "{:#04X}", value)?;
            }
            write!(f, "]")?;
            Ok(())
        }
    }

    #[test]
    fn value_classification() {
        assert!(!value_is_byte(-1));
        assert!(value_is_byte(0));
        assert!(value_is_byte(255));
        assert!(!value_is_byte(256));

        assert!(!value_is_signed_byte(-129));
        assert!(value_is_signed_byte(-128));
        assert!(value_is_signed_byte(0));
        assert!(value_is_signed_byte(127));
        assert!(!value_is_signed_byte(128));

        assert!(!value_is_word(-1));
        assert!(value_is_word(0));
        assert!(value_is_word(65535));
        assert!(!value_is_word(65536));

        assert!(!value_is_signed_word(-32769));
        assert!(value_is_signed_word(-32768));
        assert!(value_is_signed_word(0));
        assert!(value_is_signed_word(32767));
        assert!(!value_is_signed_word(32768));
    }

    macro_rules! insn {
        ($operation:expr $(,)?) => {{
            InstructionData::none(0..0, $operation)
        }};

        ($operation:expr, $dst:expr $(,)?) => {{
            InstructionData::dst(0..0, $operation, $dst)
        }};

        ($operation:expr, $dst:expr, $src:expr $(,)?) => {{
            InstructionData::dst_and_src(0..0, $operation, $dst, $src)
        }};
    }

    macro_rules! imm {
        ($value:expr) => {{
            OperandData::immediate(0..0, $value)
        }};
    }

    macro_rules! reg {
        (al) => {{
            OperandData::register(0..0, ast::ByteRegister::Al.encoding(), OperandSize::Byte)
        }};
        (bl) => {{
            OperandData::register(0..0, ast::ByteRegister::Bl.encoding(), OperandSize::Byte)
        }};
        (cl) => {{
            OperandData::register(0..0, ast::ByteRegister::Cl.encoding(), OperandSize::Byte)
        }};
        (dl) => {{
            OperandData::register(0..0, ast::ByteRegister::Dl.encoding(), OperandSize::Byte)
        }};

        (ah) => {{
            OperandData::register(0..0, ast::ByteRegister::Ah.encoding(), OperandSize::Byte)
        }};
        (bh) => {{
            OperandData::register(0..0, ast::ByteRegister::Bh.encoding(), OperandSize::Byte)
        }};
        (ch) => {{
            OperandData::register(0..0, ast::ByteRegister::Ch.encoding(), OperandSize::Byte)
        }};
        (dh) => {{
            OperandData::register(0..0, ast::ByteRegister::Dh.encoding(), OperandSize::Byte)
        }};

        (ax) => {{
            OperandData::register(0..0, ast::WordRegister::Ax.encoding(), OperandSize::Word)
        }};
        (bx) => {{
            OperandData::register(0..0, ast::WordRegister::Bx.encoding(), OperandSize::Word)
        }};
        (cx) => {{
            OperandData::register(0..0, ast::WordRegister::Cx.encoding(), OperandSize::Word)
        }};
        (dx) => {{
            OperandData::register(0..0, ast::WordRegister::Dx.encoding(), OperandSize::Word)
        }};

        (sp) => {{
            OperandData::register(0..0, ast::WordRegister::Sp.encoding(), OperandSize::Word)
        }};
        (bp) => {{
            OperandData::register(0..0, ast::WordRegister::Bp.encoding(), OperandSize::Word)
        }};
        (si) => {{
            OperandData::register(0..0, ast::WordRegister::Si.encoding(), OperandSize::Word)
        }};
        (di) => {{
            OperandData::register(0..0, ast::WordRegister::Di.encoding(), OperandSize::Word)
        }};
    }

    macro_rules! seg {
        (es) => {{
            OperandData::segment(0..0, ast::Segment::ES.encoding())
        }};
        (cs) => {{
            OperandData::segment(0..0, ast::Segment::CS.encoding())
        }};
        (ss) => {{
            OperandData::segment(0..0, ast::Segment::SS.encoding())
        }};
        (ds) => {{
            OperandData::segment(0..0, ast::Segment::DS.encoding())
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
            OperandData::direct(0..0, $addr, &$data_size, &$segment_override)
        }};
    }

    macro_rules! assert_encode {
        ($bytes:expr, $insn:expr $(, $msg:expr)*) => {{
            let mut bytes = vec![];
            let instruction = $insn;
            encode(&instruction, 0x100, &mut bytes).unwrap();
            assert_eq!(DebugBytes($bytes), DebugBytes(&bytes[..]), $($msg)*);
        }};
    }

    #[test]
    fn group_add_or_adc_sbb_and_sub_xor_cmp() {
        assert_encode!(
            &[0x01, 0xCB],
            insn!(Operation::ADD, reg!(bx), reg!(cx)),
            "add bx, cx"
        );
        assert_encode!(
            &[0x00, 0xCB],
            insn!(Operation::ADD, reg!(bl), reg!(cl)),
            "add bl, cl"
        );

        assert_encode!(
            &[0x02, 0x1E, 0x00, 0x02],
            insn!(Operation::ADD, reg!(bl), direct!(0x200)),
            "add bl, [0x200]"
        );

        assert_encode!(
            &[0x2E, 0x00, 0x1E, 0x00, 0x02],
            insn!(Operation::ADD, direct!(cs:0x200), reg!(bl)),
            "add [cs:0x200], bl"
        );

        assert_encode!(
            &[0x83, 0xC3 + 0x00, 0x10],
            insn!(Operation::ADD, reg!(bx), imm!(0x10))
        );

        assert_encode!(
            &[0x04, 0x10],
            insn!(Operation::ADD, reg!(al), imm!(0x10)),
            "add al, 0x10"
        );

        assert_encode!(
            &[0x83, 0xC0, 0x10],
            insn!(Operation::ADD, reg!(ax), imm!(0x10)),
            "add ax, 0x10"
        );

        assert_encode!(
            &[0x80, 0xC3, 0x10],
            insn!(Operation::ADD, reg!(bl), imm!(0x10)),
            "add bl, 0x10"
        );

        assert_encode!(
            &[0x81, 0xC3, 0x00, 0x01],
            insn!(Operation::ADD, reg!(bx), imm!(0x100)),
            "add bx, 0x100"
        );
    }

    #[test]
    fn group_not_neg_mul_imul_div_idiv() {
        fn group_not_neg_mul_imul_div_idiv_inner(op: Operation, post_byte: u8) {
            assert_encode!(
                &[0xF6, (0b11 << 6) + (post_byte << 3) + 0b011],
                insn!(op, reg!(bl))
            );
            assert_encode!(
                &[0xF7, (0b11 << 6) + (post_byte << 3) + 0b011],
                insn!(op, reg!(bx))
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
    fn group_mov() {
        // Samples

        assert_encode!(
            &[0x26, 0x88, 0x05],
            insn!(
                Operation::MOV,
                OperandData::indirect(
                    0..0,
                    ast::IndirectEncoding::Di.encoding(),
                    0,
                    &None,
                    &Some(ast::Segment::ES),
                ),
                reg!(al)
            ),
            "mov [es:di], al"
        );

        // mov al, 0x20
        assert_encode!(&[0xB0, 0x20], insn!(Operation::MOV, reg!(al), imm!(0x20)));

        // mov bh, 0x20
        assert_encode!(&[0xB7, 0x20], insn!(Operation::MOV, reg!(bh), imm!(0x20)));

        // mov ax, 0x2000
        assert_encode!(
            &[0xB8, 0x00, 0x20],
            insn!(Operation::MOV, reg!(ax), imm!(0x2000))
        );

        // mov di, 0x2000
        assert_encode!(
            &[0xBF, 0x00, 0x20],
            insn!(Operation::MOV, reg!(di), imm!(0x2000))
        );

        // mov al, [0x2000]
        assert_encode!(
            &[0xA0, 0x00, 0x20],
            insn!(Operation::MOV, reg!(al), direct!(0x2000))
        );

        // mov ax, [0x2000]
        assert_encode!(
            &[0xA1, 0x00, 0x20],
            insn!(Operation::MOV, reg!(ax), direct!(0x2000))
        );

        if EMIT_DEFAULT_SEGMENTS {
            // mov ax, ds:[0x2000]
            assert_encode!(
                &[0x3E, 0xA1, 0x00, 0x20],
                insn!(Operation::MOV, reg!(ax), direct!(ds:0x2000))
            );
        } else {
            // mov ax, ds:[0x2000]
            assert_encode!(
                &[0xA1, 0x00, 0x20],
                insn!(Operation::MOV, reg!(ax), direct!(ds:0x2000))
            );
        }

        // mov ax, cs:[0x2000]
        assert_encode!(
            &[0x2E, 0xA1, 0x00, 0x20],
            insn!(Operation::MOV, reg!(ax), direct!(cs:0x2000))
        );

        // mov bl, [0x2000]
        assert_encode!(
            &[0x8A, 0b00_011_110, 0x00, 0x20],
            insn!(Operation::MOV, reg!(bl), direct!(0x2000))
        );

        // mov bx, [0x2000]
        assert_encode!(
            &[0x8B, 0b00_011_110, 0x00, 0x20],
            insn!(Operation::MOV, reg!(bx), direct!(0x2000))
        );

        // mov bx, cs:[0x2000]
        assert_encode!(
            &[0x2E, 0x8B, 0b00_011_110, 0x00, 0x20],
            insn!(Operation::MOV, reg!(bx), direct!(cs:0x2000))
        );

        // mov bl, cl
        assert_encode!(
            &[0x88, 0b11_001_011],
            insn!(Operation::MOV, reg!(bl), reg!(cl))
        );

        // mov al, cl
        assert_encode!(
            &[0x88, 0b11_001_000],
            insn!(Operation::MOV, reg!(al), reg!(cl))
        );

        // mov bx, cx
        assert_encode!(
            &[0x89, 0b11_001_011],
            insn!(Operation::MOV, reg!(bx), reg!(cx))
        );

        // mov ax, cx
        assert_encode!(
            &[0x89, 0b11_001_000],
            insn!(Operation::MOV, reg!(ax), reg!(cx))
        );

        // mov [0x2000], al
        assert_encode!(
            &[0xA2, 0x00, 0x20],
            insn!(Operation::MOV, direct!(0x2000), reg!(al))
        );

        // mov [0x2000], ax
        assert_encode!(
            &[0xA3, 0x00, 0x20],
            insn!(Operation::MOV, direct!(0x2000), reg!(ax))
        );

        // mov cs:[0x2000], al
        assert_encode!(
            &[0x2E, 0xA2, 0x00, 0x20],
            insn!(Operation::MOV, direct!(cs:0x2000), reg!(al))
        );

        // mov cs:[0x2000], ax
        assert_encode!(
            &[0x2E, 0xA3, 0x00, 0x20],
            insn!(Operation::MOV, direct!(cs:0x2000), reg!(ax))
        );

        // mov [0x2000], bl
        assert_encode!(
            &[0x88, 0b00_011_110, 0x00, 0x20],
            insn!(Operation::MOV, direct!(0x2000), reg!(bl))
        );

        // mov [0x2000], bx
        assert_encode!(
            &[0x89, 0b00_011_110, 0x00, 0x20],
            insn!(Operation::MOV, direct!(0x2000), reg!(bx))
        );

        // mov cs:[0x2000], bl
        assert_encode!(
            &[0x2E, 0x88, 0b00_011_110, 0x00, 0x20],
            insn!(Operation::MOV, direct!(cs:0x2000), reg!(bl))
        );

        // mov cs:[0x2000], bx
        assert_encode!(
            &[0x2E, 0x89, 0b00_011_110, 0x00, 0x20],
            insn!(Operation::MOV, direct!(cs:0x2000), reg!(bx))
        );
    }

    #[test]
    fn group_xchg() {
        // xchg al, cl
        assert_encode!(
            &[0x86, 0b11_000_001],
            insn!(Operation::XCHG, reg!(al), reg!(cl))
        );

        // xchg al, cl
        assert_encode!(&[0x91], insn!(Operation::XCHG, reg!(ax), reg!(cx)));

        // xchg ax, ax
        // nop
        assert_encode!(&[0x90], insn!(Operation::XCHG, reg!(ax), reg!(ax)));

        // xchg bl, dl
        assert_encode!(
            &[0x86, 0b11_011_010],
            insn!(Operation::XCHG, reg!(bl), reg!(dl))
        );

        // xchg bx, dx
        assert_encode!(
            &[0x87, 0b11_011_010],
            insn!(Operation::XCHG, reg!(bx), reg!(dx))
        );

        // xchg bl, [0x2000]
        assert_encode!(
            &[0x86, 0b00_011_110, 0x00, 0x20],
            insn!(Operation::XCHG, reg!(bl), direct!(0x2000))
        );

        // xchg bx, [0x2000]
        assert_encode!(
            &[0x87, 0b00_011_110, 0x00, 0x20],
            insn!(Operation::XCHG, reg!(bx), direct!(0x2000))
        );
    }

    #[test]
    fn group_inc_dec() {
        // inc bx
        assert_encode!(&[0x43], insn!(Operation::INC, reg!(bx)));
        // inc bl
        assert_encode!(&[0xFE, 0xC3], insn!(Operation::INC, reg!(bl)));
        // inc word [0x2000]
        assert_encode!(
            &[0xFF, 0x06, 0x00, 0x20],
            insn!(
                Operation::INC,
                OperandData::direct(0..0, 0x2000, &Some(ast::DataSize::Word), &None)
            )
        );
        // inc byte [0x2000]
        assert_encode!(
            &[0xFE, 0x06, 0x00, 0x20],
            insn!(
                Operation::INC,
                OperandData::direct(0..0, 0x2000, &Some(ast::DataSize::Byte), &None)
            )
        );
    }

    #[test]
    fn group_push() {
        // push ax
        assert_encode!(&[0x50], insn!(Operation::PUSH, reg!(ax)));
        // push si
        assert_encode!(&[0x56], insn!(Operation::PUSH, reg!(si)));
        // push ds
        assert_encode!(&[0x1E], insn!(Operation::PUSH, seg!(ds)));
        // push ss
        assert_encode!(&[0x16], insn!(Operation::PUSH, seg!(ss)));
    }

    #[test]
    fn group_pop() {
        // push ax
        assert_encode!(&[0x58], insn!(Operation::POP, reg!(ax)));
        // push si
        assert_encode!(&[0x5E], insn!(Operation::POP, reg!(si)));
        // push ds
        assert_encode!(&[0x1F], insn!(Operation::POP, seg!(ds)));
        // push ss
        assert_encode!(&[0x17], insn!(Operation::POP, seg!(ss)));
    }

    #[test]
    fn group_jmp() {
        // jmp 0x110
        assert_encode!(&[0xE9, 0x0D, 0x00], insn!(Operation::JMP, imm!(0x110)));
        // jmp 0x70
        assert_encode!(&[0xE9, 0x6D, 0xFF], insn!(Operation::JMP, imm!(0x70)));

        // jmp bx
        assert_encode!(&[0xFF, 0xE3], insn!(Operation::JMP, reg!(bx)));
        // jmp [0x2000]
        assert_encode!(
            &[0xFF, 0x26, 0x00, 0x20],
            insn!(Operation::JMP, direct!(0x2000))
        );
    }

    #[test]
    fn group_les_lds() {
        // lds dx, [0x2000]
        assert_encode!(
            &[0xC5, 0b00_010_110, 0x00, 0x20],
            insn!(Operation::LDS, reg!(dx), direct!(0x2000))
        );
        // lds dx, [0x2000]
        assert_encode!(
            &[0x2E, 0xC5, 0b00_010_110, 0x00, 0x20],
            insn!(Operation::LDS, reg!(dx), direct!(cs:0x2000))
        );

        // les dx, [0x2000]
        assert_encode!(
            &[0xC4, 0b00_010_110, 0x00, 0x20],
            insn!(Operation::LES, reg!(dx), direct!(0x2000))
        );
        // les dx, [0x2000]
        assert_encode!(
            &[0x2E, 0xC4, 0b00_010_110, 0x00, 0x20],
            insn!(Operation::LES, reg!(dx), direct!(cs:0x2000))
        );
    }

    #[test]
    fn group_rol_ror_rcl_rcr_shl_sal_shr_sar() {
        // shl al, cl
        assert_encode!(
            &[0xD2, 0b11_100_000],
            insn!(Operation::SHL, reg!(al), reg!(cl))
        );

        // shl ax, cl
        assert_encode!(
            &[0xD3, 0b11_100_000],
            insn!(Operation::SHL, reg!(ax), reg!(cl))
        );

        // shl al, 1
        assert_encode!(
            &[0xD0, 0b11_100_000],
            insn!(Operation::SHL, reg!(al), imm!(0x01))
        );

        // shl ax, 1
        assert_encode!(
            &[0xD1, 0b11_100_000],
            insn!(Operation::SHL, reg!(ax), imm!(0x01))
        );
    }

    #[test]
    fn group_jcc() {
        assert_encode!(&[0x70, 0xFE], insn!(Operation::JO, imm!(0x100)));
        assert_encode!(&[0x71, 0xFE], insn!(Operation::JNO, imm!(0x100)));
        assert_encode!(&[0x72, 0xFE], insn!(Operation::JB, imm!(0x100)));
        assert_encode!(&[0x73, 0xFE], insn!(Operation::JNB, imm!(0x100)));
        assert_encode!(&[0x74, 0xFE], insn!(Operation::JE, imm!(0x100)));
        assert_encode!(&[0x75, 0xFE], insn!(Operation::JNE, imm!(0x100)));
        assert_encode!(&[0x76, 0xFE], insn!(Operation::JBE, imm!(0x100)));
        assert_encode!(&[0x77, 0xFE], insn!(Operation::JNBE, imm!(0x100)));
        assert_encode!(&[0x78, 0xFE], insn!(Operation::JS, imm!(0x100)));
        assert_encode!(&[0x79, 0xFE], insn!(Operation::JNS, imm!(0x100)));
        assert_encode!(&[0x7A, 0xFE], insn!(Operation::JP, imm!(0x100)));
        assert_encode!(&[0x7B, 0xFE], insn!(Operation::JNP, imm!(0x100)));
        assert_encode!(&[0x7C, 0xFE], insn!(Operation::JL, imm!(0x100)));
        assert_encode!(&[0x7D, 0xFE], insn!(Operation::JNL, imm!(0x100)));
        assert_encode!(&[0x7E, 0xFE], insn!(Operation::JLE, imm!(0x100)));
        assert_encode!(&[0x7F, 0xFE], insn!(Operation::JNLE, imm!(0x100)));
    }

    #[test]
    fn group_loopc() {
        assert_encode!(&[0xE0, 0xFE], insn!(Operation::LOOPNZ, imm!(0x100)));
        assert_encode!(&[0xE1, 0xFE], insn!(Operation::LOOPZ, imm!(0x100)));
        assert_encode!(&[0xE2, 0xFE], insn!(Operation::LOOP, imm!(0x100)));
        assert_encode!(&[0xE3, 0xFE], insn!(Operation::JCXZ, imm!(0x100)));
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
            assert_encode!(&[*op_code], insn!(*op));
        }
    }

    #[test]
    fn group_prefixes() {
        // lock
        assert_encode!(&[0xF0], insn!(Operation::LOCK));
        // repne
        assert_encode!(&[0xF2], insn!(Operation::REPNE));
        // rep
        assert_encode!(&[0xF3], insn!(Operation::REP));
    }

    #[test]
    fn group_int() {
        // int 0x21
        assert_encode!(&[0xCD, 0x21], insn!(Operation::INT, imm!(0x21)));
    }

    #[test]
    fn group_aam() {
        // aam
        assert_encode!(&[0xD4, 0xA0], insn!(Operation::AAM));
    }

    #[test]
    fn group_aad() {
        // aad
        assert_encode!(&[0xD5, 0xA0], insn!(Operation::AAD));
    }

    #[test]
    fn group_xlat() {
        // xlat word [bx]
        assert_encode!(
            &[0xD7],
            insn!(
                Operation::XLATB,
                OperandData::indirect(
                    0..0,
                    ast::IndirectEncoding::Bx.encoding(),
                    0,
                    &Some(ast::DataSize::Word),
                    &None
                )
            )
        );

        // xlat word cs:[bx]
        assert_encode!(
            &[0x2E, 0xD7],
            insn!(
                Operation::XLATB,
                OperandData::indirect(
                    0..0,
                    ast::IndirectEncoding::Bx.encoding(),
                    0,
                    &Some(ast::DataSize::Word),
                    &Some(ast::Segment::CS),
                )
            )
        );

        if EMIT_DEFAULT_SEGMENTS {
            assert_encode!(
                &[0x3E, 0xD7],
                insn!(
                    Operation::XLATB,
                    OperandData::indirect(
                        0..0,
                        ast::IndirectEncoding::Bx.encoding(),
                        0,
                        &Some(ast::DataSize::Word),
                        &Some(ast::Segment::DS),
                    )
                ),
                "xlat word [ds:bx]"
            );
        } else {
            assert_encode!(
                &[0xD7],
                insn!(
                    Operation::XLATB,
                    OperandData::indirect(
                        0..0,
                        ast::IndirectEncoding::Bx.encoding(),
                        0,
                        &Some(ast::DataSize::Word),
                        &Some(ast::Segment::DS),
                    )
                ),
                "xlat word [ds:bx]"
            );
        }
    }

    #[test]
    fn group_in() {
        // in al, 0x10
        assert_encode!(&[0xE4, 0x10], insn!(Operation::IN, reg!(al), imm!(0x10)));
        // in ax, 0x10
        assert_encode!(&[0xE5, 0x10], insn!(Operation::IN, reg!(ax), imm!(0x10)));

        // in al, dx
        assert_encode!(&[0xEC], insn!(Operation::IN, reg!(al), reg!(dx)));
        // in ax, dx
        assert_encode!(&[0xED], insn!(Operation::IN, reg!(ax), reg!(dx)));
    }

    #[test]
    fn group_out() {
        // out 0x10, al
        assert_encode!(&[0xE6, 0x10], insn!(Operation::OUT, imm!(0x10), reg!(al)));
        // out 0x10, ax
        assert_encode!(&[0xE7, 0x10], insn!(Operation::OUT, imm!(0x10), reg!(ax)));

        // out dx, al
        assert_encode!(&[0xEE], insn!(Operation::OUT, reg!(dx), reg!(al)));
        // out dx, ax
        assert_encode!(&[0xEF], insn!(Operation::OUT, reg!(dx), reg!(ax)));
    }
}
