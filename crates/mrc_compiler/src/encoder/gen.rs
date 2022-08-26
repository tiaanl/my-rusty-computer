use super::{
    require_value_is_byte, require_value_is_signed_byte, ByteEmitter, EncodeError, InstructionData,
    OperandData, OperandSize,
};
use crate::ast;
use crate::encoder::{emit_segment_prefix, require_value_is_word};

type FirstOperand = u8;
type InsnSize = u8;
type OpCode = u8;

pub const FIRST_OPER_DST: FirstOperand = 0;
pub const FIRST_OPER_SRC: FirstOperand = 1;

pub enum Code {
    /// Emits the given byte.
    Byte(u8),

    /// Emits an unsigned byte/word from the [imm] value of the specified operand.
    ImmByte(FirstOperand),
    ImmWord(FirstOperand),

    /// Emites an unsigned byte/word from the [disp] value of the specified operand.
    DispWord(FirstOperand),

    /// Emits a relative offset between the given operand's [imm] value and the current offset +
    /// the size of the instruction in the 2nd value.
    RelByte(FirstOperand, InsnSize),

    /// Emits a mod reg r/m encoded byte according the the given values.
    ModRegRM(FirstOperand, OpCode),

    /// Emits a mod reg r/m encoded byte according to the given values and the [reg] value
    /// statically set.
    ModRM(FirstOperand, OpCode, u8),
}

pub fn emit_codes(
    emitter: &mut impl ByteEmitter,
    insn: &InstructionData,
    offset: u16,
    codes: &[Code],
) -> Result<(), EncodeError> {
    for code in codes.iter() {
        match code {
            Code::Byte(value) => {
                emitter.emit(*value);
            }

            Code::ImmByte(first_operand) => {
                let oper = &insn.opers[*first_operand as usize];
                let value = require_value_is_byte(oper.imm, &oper.span)?;
                emitter.emit(value);
            }

            Code::ImmWord(first_operand) => {
                let oper = &insn.opers[*first_operand as usize];
                let value = require_value_is_word(oper.imm, &oper.span)?;
                for byte in value.to_le_bytes() {
                    emitter.emit(byte);
                }
            }

            Code::DispWord(first_operand) => {
                let oper = &insn.opers[*first_operand as usize];
                debug_assert_eq!(OperandSize::Word, oper.displacement_size);
                let value = require_value_is_word(oper.displacement, &oper.span)?;
                for byte in value.to_le_bytes() {
                    emitter.emit(byte);
                }
            }

            Code::RelByte(first_operand, insn_size) => {
                let oper = &insn.opers[*first_operand as usize];
                // println!(
                //     "oper.imm: {:04X}, offset: {:04X}, insn_size: {:02X}",
                //     oper.imm, offset, insn_size
                // );
                let rel = oper.imm as i32 - (offset as i32 + *insn_size as i32);
                let value = match require_value_is_signed_byte(rel, &oper.span) {
                    Ok(value) => value,
                    Err(err) => {
                        return Err(EncodeError::RelativeJumpOutOfRange(
                            err.span().clone(),
                            OperandSize::Byte,
                            true,
                            rel,
                        ))
                    }
                };
                emitter.emit(value as u8);
            }

            Code::ModRegRM(first_operand, op_code) => {
                let (reg, rm) = if *first_operand == 0 {
                    (insn.opers[1].rm, &insn.opers[0])
                } else {
                    (insn.opers[0].rm, &insn.opers[1])
                };

                emit_mod_reg_rm(*op_code, rm, reg, emitter);
            }

            Code::ModRM(first_operand, op_code, reg) => {
                let rm = &insn.opers[*first_operand as usize];
                emit_mod_reg_rm(*op_code, rm, *reg, emitter);
            }
        }
    }

    Ok(())
}

fn emit_mod_reg_rm(op_code: u8, rm: &OperandData, reg: u8, emitter: &mut impl ByteEmitter) {
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
}
