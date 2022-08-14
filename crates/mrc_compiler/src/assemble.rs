#![allow(dead_code)]

use crate::ast;
use crate::ast::IndirectEncoding;
use crate::compiler::CompileError;
use mrc_instruction::Operation;

struct Emitter(Vec<u8>);

pub fn assemble(ins: &ast::Instruction) -> Result<Vec<u8>, CompileError> {
    let mut out = Emitter(vec![]);

    match ins.operation {
        Operation::MOV => todo!(),
        Operation::PUSH => todo!(),
        Operation::POP => todo!(),
        Operation::XCHG => todo!(),
        Operation::IN => todo!(),
        Operation::OUT => todo!(),
        Operation::XLAT => out.byte(0xD7),
        Operation::LEA => todo!(),
        Operation::LDS => todo!(),
        Operation::LES => todo!(),
        Operation::LAHF => out.byte(0x9F),
        Operation::SAHF => out.byte(0x9E),
        Operation::PUSHF => out.byte(0x9C),
        Operation::POPF => out.byte(0x9D),
        Operation::ADD => todo!(),
        Operation::ADC => todo!(),
        Operation::INC => inc_dec(&mut out, ins, 0x40, 0)?,
        Operation::AAA => out.byte(0x37),
        Operation::DAA => out.byte(0x27),
        Operation::SUB => todo!(),
        Operation::SBB => todo!(),
        Operation::DEC => inc_dec(&mut out, ins, 0x48, 1)?,
        Operation::NEG => todo!(),
        Operation::CMP => todo!(),
        Operation::AAS => out.byte(0x3F),
        Operation::DAS => out.byte(0x2F),
        Operation::MUL => todo!(),
        Operation::IMUL => todo!(),
        Operation::AAM => todo!(),
        Operation::DIV => todo!(),
        Operation::IDIV => todo!(),
        Operation::AAD => todo!(),
        Operation::CBW => out.byte(0x98),
        Operation::CWD => out.byte(0x99),
        Operation::NOT => todo!(),
        Operation::SHL => todo!(),
        Operation::SHR => todo!(),
        Operation::SAR => todo!(),
        Operation::ROL => todo!(),
        Operation::ROR => todo!(),
        Operation::RCL => todo!(),
        Operation::RCR => todo!(),
        Operation::AND => todo!(),
        Operation::TEST => todo!(),
        Operation::OR => todo!(),
        Operation::XOR => todo!(),
        Operation::REP => todo!(),
        Operation::REPNE => todo!(),
        Operation::MOVSB => out.byte(0xA4),
        Operation::MOVSW => out.byte(0xA5),
        Operation::CMPSB => out.byte(0xA6),
        Operation::CMPSW => out.byte(0xA7),
        Operation::SCASB => out.byte(0xAE),
        Operation::SCASW => out.byte(0xAF),
        Operation::LODSB => out.byte(0xAC),
        Operation::LODSW => out.byte(0xAD),
        Operation::STOSB => out.byte(0xAA),
        Operation::STOSW => out.byte(0xAB),
        Operation::CALL => todo!(),
        Operation::JMP => todo!(),
        Operation::RET => todo!(),
        Operation::JE => todo!(),
        Operation::JL => todo!(),
        Operation::JLE => todo!(),
        Operation::JB => todo!(),
        Operation::JBE => todo!(),
        Operation::JP => todo!(),
        Operation::JO => todo!(),
        Operation::JS => todo!(),
        Operation::JNE => todo!(),
        Operation::JNL => todo!(),
        Operation::JNLE => todo!(),
        Operation::JNB => todo!(),
        Operation::JNBE => todo!(),
        Operation::JNP => todo!(),
        Operation::JNO => todo!(),
        Operation::JNS => todo!(),
        Operation::LOOP => todo!(),
        Operation::LOOPZ => todo!(),
        Operation::LOOPNZ => todo!(),
        Operation::JCXZ => todo!(),
        Operation::INT => todo!(),
        Operation::INT1 => todo!(),
        Operation::INT3 => out.byte(0xCC),
        Operation::INTO => out.byte(0xCE),
        Operation::IRET => out.byte(0xCF),
        Operation::CLC => out.byte(0xF8),
        Operation::CMC => out.byte(0xF5),
        Operation::STC => out.byte(0xF9),
        Operation::CLD => out.byte(0xFC),
        Operation::STD => out.byte(0xFD),
        Operation::CLI => out.byte(0xFA),
        Operation::STI => out.byte(0xFB),
        Operation::HLT => out.byte(0xF4),
        Operation::WAIT => todo!(),
        Operation::ESC => todo!(),
        Operation::LOCK => todo!(),
        Operation::NOP => out.byte(0x90),
        Operation::SALC => out.byte(0xD6),
    }

    Ok(out.0)
}

fn inc_dec(
    out: &mut Emitter,
    ins: &ast::Instruction,
    base_for_plus_reg: u8,
    reg_for_mrrm: u8,
) -> Result<(), CompileError> {
    match &ins.operands {
        ast::Operands::Destination(_, ast::Operand::Register(_, ast::Register::Word(reg))) => {
            out.byte(base_for_plus_reg + reg.encoding());
        }

        ast::Operands::Destination(
            _,
            op @ ast::Operand::Address(
                span,
                ast::Expression::Value(_, ast::Value::Constant(value)),
                data_size,
                _,
            ),
        ) => {
            out.segment_prefix(op);

            match data_size {
                Some(ast::DataSize::Byte) => {
                    out.byte(0xFE);
                    out.mod_rm_direct(reg_for_mrrm, *value);
                }

                Some(ast::DataSize::Word) => {
                    out.byte(0xFF);
                    out.mod_rm_direct(reg_for_mrrm, *value);
                }

                None => return Err(CompileError::DataSizeNotSpecified(span.clone())),
            }
        }

        ast::Operands::Destination(
            _,
            op @ ast::Operand::Indirect(span, ie, maybe_disp, data_size, _),
        ) => {
            out.segment_prefix(op);

            match data_size {
                Some(ast::DataSize::Byte) => {
                    out.byte(0xFE);
                    out.mod_rm_indirect(reg_for_mrrm, &ie, &maybe_disp);
                }

                Some(ast::DataSize::Word) => {
                    out.byte(0xFF);
                    out.mod_rm_indirect(reg_for_mrrm, &ie, &maybe_disp);
                }

                None => return Err(CompileError::DataSizeNotSpecified(span.clone())),
            }
        }

        _ => {
            return Err(CompileError::InvalidOperands(
                ins.operands.span().clone(),
                ins.clone(),
                vec![],
            ))
        }
    }

    Ok(())
}

impl Emitter {
    fn segment_prefix(&mut self, op: &ast::Operand) {
        let seg = match op {
            ast::Operand::Address(_, _, _, Some(seg)) => {
                if *seg != ast::Segment::DS {
                    Some(*seg)
                } else {
                    None
                }
            }

            ast::Operand::Indirect(_, ie, _, _, Some(seg)) => match ie {
                IndirectEncoding::BpSi | IndirectEncoding::BpDi | IndirectEncoding::Bp => {
                    if *seg != ast::Segment::SS {
                        Some(*seg)
                    } else {
                        None
                    }
                }
                IndirectEncoding::BxSi
                | IndirectEncoding::BxDi
                | IndirectEncoding::Si
                | IndirectEncoding::Di
                | IndirectEncoding::Bx => {
                    if *seg != ast::Segment::DS {
                        Some(*seg)
                    } else {
                        None
                    }
                }
            },

            _ => None,
        };

        if let Some(seg) = seg {
            match seg {
                ast::Segment::ES => self.byte(0x26),
                ast::Segment::CS => self.byte(0x2E),
                ast::Segment::SS => self.byte(0x36),
                ast::Segment::DS => self.byte(0x3E),
            }
        }
    }

    fn byte(&mut self, byte: u8) {
        self.0.push(byte);
    }

    fn immediate_byte(&mut self, value: i32) {
        debug_assert!(value >= u8::MIN as i32 && value <= u8::MAX as i32);

        self.byte(value as u8);
    }

    fn immediate_word(&mut self, value: i32) {
        debug_assert!(value >= u16::MIN as i32 && value <= u16::MAX as i32);

        for byte in value.to_le_bytes() {
            self.byte(byte);
        }
    }

    fn mod_reg_rm(&mut self, mode: u8, reg: u8, reg_mem: u8) {
        self.byte((mode << 6) | (reg << 3) | reg_mem);
    }

    fn mod_rm_direct(&mut self, reg: u8, address: i32) {
        let mode = 0b00;
        let rm = 0b110;

        self.mod_reg_rm(mode, reg, rm);
        for byte in (address as u16).to_le_bytes() {
            self.byte(byte);
        }
    }

    fn mod_rm_indirect(
        &mut self,
        reg: u8,
        ie: &IndirectEncoding,
        maybe_disp: &Option<ast::Expression>,
    ) {
        let disp = maybe_disp
            .as_ref()
            .map(|v| {
                if let ast::Expression::Value(_, ast::Value::Constant(value)) = v {
                    *value
                } else {
                    0
                }
            })
            .unwrap_or(0);

        let mode = match disp {
            _ if disp == 0 => 0b00,
            _ if disp >= i8::MIN as i32 && disp <= i8::MAX as i32 => 0b01,
            _ if disp >= i16::MIN as i32 && disp <= i16::MAX as i32 => 0b10,
            _ => unreachable!(),
        };

        self.mod_reg_rm(mode, reg, ie.encoding());

        if mode == 0b01 {
            self.byte(disp as i8 as u8);
        } else if mode == 0b10 {
            for byte in (disp as i16 as u16).to_le_bytes() {
                self.byte(byte);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ins_mov() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::MOV,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_push() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::PUSH,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_pop() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::POP,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_xchg() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::XCHG,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_in() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::IN,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_out() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::OUT,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_xlat() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::XLAT,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xD7]
        );
    }

    #[test]
    fn ins_lea() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::LEA,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_lds() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::LDS,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_les() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::LES,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_lahf() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::LAHF,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x9F]
        );
    }

    #[test]
    fn ins_sahf() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::SAHF,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x9E]
        );
    }

    #[test]
    fn ins_pushf() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::PUSHF,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x9C]
        );
    }

    #[test]
    fn ins_popf() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::POPF,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x9D]
        );
    }

    #[test]
    fn ins_add() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::ADD,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_adc() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::ADC,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_inc() {
        let regs = [
            (ast::WordRegister::Ax, 0x40),
            (ast::WordRegister::Cx, 0x41),
            (ast::WordRegister::Dx, 0x42),
            (ast::WordRegister::Bx, 0x43),
            (ast::WordRegister::Sp, 0x44),
            (ast::WordRegister::Bp, 0x45),
            (ast::WordRegister::Si, 0x46),
            (ast::WordRegister::Di, 0x47),
        ];

        for (reg, expected) in regs {
            assert_eq!(
                assemble(&ast::Instruction {
                    span: 0..0,
                    operation: Operation::INC,
                    operands: ast::Operands::Destination(
                        0..0,
                        ast::Operand::Register(0..0, ast::Register::Word(reg))
                    ),
                })
                .unwrap(),
                vec![expected]
            );
        }

        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::INC,
                operands: ast::Operands::Destination(
                    0..0,
                    ast::Operand::Address(
                        0..0,
                        ast::Expression::Value(0..0, ast::Value::Constant(12345)),
                        Some(ast::DataSize::Byte),
                        None
                    ),
                ),
            })
            .unwrap(),
            vec![0xFE, 0x06, 0x39, 0x30]
        );

        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::INC,
                operands: ast::Operands::Destination(
                    0..0,
                    ast::Operand::Address(
                        0..0,
                        ast::Expression::Value(0..0, ast::Value::Constant(12345)),
                        Some(ast::DataSize::Word),
                        None
                    ),
                ),
            })
            .unwrap(),
            vec![0xFF, 0x06, 0x39, 0x30]
        );

        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::INC,
                operands: ast::Operands::Destination(
                    0..0,
                    ast::Operand::Indirect(
                        0..0,
                        IndirectEncoding::BxSi,
                        Some(ast::Expression::Value(0..0, ast::Value::Constant(12345))),
                        Some(ast::DataSize::Byte),
                        None
                    ),
                ),
            })
            .unwrap(),
            vec![0xFE, 0x80, 0x39, 0x30]
        );

        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::INC,
                operands: ast::Operands::Destination(
                    0..0,
                    ast::Operand::Indirect(
                        0..0,
                        IndirectEncoding::Bp,
                        None,
                        Some(ast::DataSize::Word),
                        None
                    ),
                ),
            })
            .unwrap(),
            vec![0xFF, 0x06, 0x39, 0x30]
        );
    }

    #[test]
    fn ins_aaa() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::AAA,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x37]
        );
    }

    #[test]
    fn ins_daa() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::DAA,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x27]
        );
    }

    #[test]
    fn ins_sub() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::SUB,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_sbb() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::SBB,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_dec() {
        let regs = [
            (ast::WordRegister::Ax, 0x48),
            (ast::WordRegister::Cx, 0x49),
            (ast::WordRegister::Dx, 0x4A),
            (ast::WordRegister::Bx, 0x4B),
            (ast::WordRegister::Sp, 0x4C),
            (ast::WordRegister::Bp, 0x4D),
            (ast::WordRegister::Si, 0x4E),
            (ast::WordRegister::Di, 0x4F),
        ];

        for (reg, expected) in regs {
            assert_eq!(
                assemble(&ast::Instruction {
                    span: 0..0,
                    operation: Operation::DEC,
                    operands: ast::Operands::Destination(
                        0..0,
                        ast::Operand::Register(0..0, ast::Register::Word(reg))
                    ),
                })
                .unwrap(),
                vec![expected]
            );
        }

        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::DEC,
                operands: ast::Operands::Destination(
                    0..0,
                    ast::Operand::Address(
                        0..0,
                        ast::Expression::Value(0..0, ast::Value::Constant(12345)),
                        Some(ast::DataSize::Byte),
                        None
                    ),
                ),
            })
            .unwrap(),
            vec![0xFE, 0x0E, 0x39, 0x30]
        );

        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::DEC,
                operands: ast::Operands::Destination(
                    0..0,
                    ast::Operand::Address(
                        0..0,
                        ast::Expression::Value(0..0, ast::Value::Constant(12345)),
                        Some(ast::DataSize::Word),
                        None
                    ),
                ),
            })
            .unwrap(),
            vec![0xFF, 0x0E, 0x39, 0x30]
        );
    }

    #[test]
    fn ins_neg() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::NEG,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_cmp() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::CMP,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_aas() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::AAS,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x3F]
        );
    }

    #[test]
    fn ins_das() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::DAS,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x2F]
        );
    }

    #[test]
    fn ins_mul() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::MUL,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_imul() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::IMUL,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_aam() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::AAM,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_div() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::DIV,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_idiv() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::IDIV,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_aad() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::AAD,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_cbw() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::CBW,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x98]
        );
    }

    #[test]
    fn ins_cwd() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::CWD,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x99]
        );
    }

    #[test]
    fn ins_not() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::NOT,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_shl() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::SHL,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_shr() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::SHR,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_sar() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::SAR,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_rol() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::ROL,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_ror() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::ROR,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_rcl() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::RCL,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_rcr() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::RCR,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_and() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::AND,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_test() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::TEST,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_or() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::OR,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_xor() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::XOR,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_rep() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::REP,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_repne() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::REPNE,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_movsb() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::MOVSB,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xA4]
        );
    }

    #[test]
    fn ins_movsw() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::MOVSW,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xA5]
        );
    }

    #[test]
    fn ins_cmpsb() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::CMPSB,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xA6]
        );
    }

    #[test]
    fn ins_cmpsw() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::CMPSW,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xA7]
        );
    }

    #[test]
    fn ins_scasb() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::SCASB,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xAE]
        );
    }

    #[test]
    fn ins_scasw() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::SCASW,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xAF]
        );
    }

    #[test]
    fn ins_lodsb() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::LODSB,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xAC]
        );
    }

    #[test]
    fn ins_lodsw() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::LODSW,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xAD]
        );
    }

    #[test]
    fn ins_stosb() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::STOSB,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xAA]
        );
    }

    #[test]
    fn ins_stosw() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::STOSW,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xAB]
        );
    }

    #[test]
    fn ins_call() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::CALL,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jmp() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JMP,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_ret() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::RET,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_je() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JE,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jl() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JL,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jle() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JLE,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jb() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JB,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jbe() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JBE,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jp() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JP,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jo() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JO,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_js() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JS,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jne() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JNE,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jnl() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JNL,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jnle() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JNLE,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jnb() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JNB,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jnbe() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JNBE,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jnp() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JNP,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jno() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JNO,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jns() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JNS,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_loop() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::LOOP,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_loopz() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::LOOPZ,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_loopnz() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::LOOPNZ,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_jcxz() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::JCXZ,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_int() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::INT,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_int1() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::INT1,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_int3() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::INT3,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xCC]
        );
    }

    #[test]
    fn ins_into() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::INTO,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xCE]
        );
    }

    #[test]
    fn ins_iret() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::IRET,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xCF]
        );
    }

    #[test]
    fn ins_clc() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::CLC,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xF8]
        );
    }

    #[test]
    fn ins_cmc() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::CMC,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xF5]
        );
    }

    #[test]
    fn ins_stc() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::STC,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xF9]
        );
    }

    #[test]
    fn ins_cld() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::CLD,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xFC]
        );
    }

    #[test]
    fn ins_std() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::STD,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xFD]
        );
    }

    #[test]
    fn ins_cli() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::CLI,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xFA]
        );
    }

    #[test]
    fn ins_sti() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::STI,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xFB]
        );
    }

    #[test]
    fn ins_hlt() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::HLT,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xF4]
        );
    }

    #[test]
    fn ins_wait() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::WAIT,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_esc() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::ESC,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_lock() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::LOCK,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x00]
        );
    }

    #[test]
    fn ins_nop() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::NOP,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0x90]
        );
    }

    #[test]
    fn ins_salc() {
        assert_eq!(
            assemble(&ast::Instruction {
                span: 0..0,
                operation: Operation::SALC,
                operands: ast::Operands::None(0..0),
            })
            .unwrap(),
            vec![0xD6]
        );
    }
}
