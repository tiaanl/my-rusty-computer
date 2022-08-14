#![allow(unused)]

use super::ast;

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
    Unknown,
}

fn encode(instruction: &ast::Instruction, offset: u16) -> Result<Vec<u8>, EncodeError> {
    use mrc_instruction::Operation::*;

    let mut result = vec![];

    match instruction.operation {
        ADD => encode_group_1(0x00, instruction, offset, &mut result),
        OR => encode_group_1(0x08, instruction, offset, &mut result),
        ADC => encode_group_1(0x10, instruction, offset, &mut result),
        SBB => encode_group_1(0x18, instruction, offset, &mut result),
        AND => encode_group_1(0x20, instruction, offset, &mut result),
        SUB => encode_group_1(0x28, instruction, offset, &mut result),
        XOR => encode_group_1(0x30, instruction, offset, &mut result),
        CMP => encode_group_1(0x38, instruction, offset, &mut result),

        _ => todo!(),
    };

    Ok(result)
}

fn encode_group_1(
    op_code: u8,
    instruction: &ast::Instruction,
    offset: u16,
    emitter: &mut impl ByteEmitter,
) -> Result<(), EncodeError> {
    let (dst, src) = if let ast::Operands::DestinationAndSource(_, dst, src) = &instruction.operands
    {
        (OperandData::from(dst), OperandData::from(src))
    } else {
        return Err(EncodeError::Unknown);
    };

    if dst.size == 0 && src.size == 0 {
        return Err(EncodeError::OperandSizeNotSpecified(
            instruction.operands.span().clone(),
        ));
    }

    if dst.size != 0 && src.size != 0 && dst.size != src.size {
        return Err(EncodeError::OperandSizesDoNotMatch(
            instruction.operands.span().clone(),
        ));
    }

    let size = if dst.size != 0 { dst.size } else { src.size };
    if size > 2 {
        return Err(EncodeError::InvalidOperandSize(
            instruction.operands.span().clone(),
        ));
    }

    // dbg!(&dst, &src);

    match (src.kind, dst.kind) {
        (OperandKind::Reg, OperandKind::Reg | OperandKind::Mem) => {
            let op_code = if size == 2 { op_code } else { op_code + 1 };
            store_instruction(op_code, &dst, src.rm, 0, 0, emitter);
        }

        (OperandKind::Mem, OperandKind::Reg) => {
            let op_code = op_code + 2 + if size == 2 { 0 } else { 1 };
            store_instruction(op_code, &src, dst.rm, 0, 0, emitter);
        }

        (OperandKind::Imm, OperandKind::Reg | OperandKind::Mem) => {
            if size == 2 {
                if dst.kind == OperandKind::Reg && dst.rm == 0 {
                    // AX, imm
                    let op_code = op_code + 5;
                    emitter.emit(op_code);

                    if src.imm < 0 || src.imm >= 0x10000 {
                        return Err(EncodeError::ImmediateOutOfRange(
                            instruction.operands.span().clone(),
                        ));
                    }

                    for byte in (src.imm as u16).to_le_bytes() {
                        emitter.emit(byte);
                    }
                } else if src.imm < 0x80 && src.imm >= -0x80 {
                    // reg16, imm8
                    let rm = op_code >> 3;
                    store_instruction(0x83, &dst, rm, 1, src.imm, emitter);
                } else {
                    // reg16, imm16
                    let rm = op_code >> 3;
                    store_instruction(0x81, &dst, rm, size, src.imm, emitter);
                }
            } else {
                if dst.kind == OperandKind::Reg && dst.rm == 0 {
                    // AL, imm
                    let op_code = op_code + 4;
                    emitter.emit(op_code);
                    emitter.emit(src.imm as u8);
                } else {
                    // reg8, imm
                    let rm = op_code >> 3;
                    if src.imm < 0 || src.imm >= 0x100 {
                        return Err(EncodeError::ImmediateOutOfRange(
                            instruction.operands.span().clone(),
                        ));
                    }
                    store_instruction(0x80, &dst, rm, 1, src.imm, emitter);
                }
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

#[derive(Clone, Copy, Debug, PartialEq)]
enum OperandKind {
    Imm,
    Reg,
    Mem,
}

#[derive(Debug)]
struct OperandData {
    size: u8,
    kind: OperandKind,
    segment_prefix: u8,
    imm: i32,
    // unresolved: u8,
    displacement: i32,
    displacement_size: u8,
    // address: u8,
    // address_registers: u8,
    // segment: u8,
    // offset: u8,
    // jmp_type: u8,
    mode: u8,
    rm: u8,
}

impl OperandData {
    fn from(operand: &ast::Operand) -> Self {
        match operand {
            ast::Operand::Immediate(_, expr) => {
                if let ast::Expression::Value(_, ast::Value::Constant(imm)) = expr {
                    Self {
                        kind: OperandKind::Imm,
                        segment_prefix: 0,
                        imm: *imm,
                        displacement: 0,
                        displacement_size: 0,
                        size: 0,
                        rm: 0,
                        mode: 0,
                    }
                } else {
                    panic!("Expression value is not a constant");
                }
            }

            ast::Operand::Register(_, reg) => Self {
                kind: OperandKind::Reg,
                segment_prefix: 0,
                imm: 0,
                displacement: 0,
                displacement_size: 0,
                size: match reg {
                    ast::Register::Byte(_) => 1,
                    ast::Register::Word(_) => 2,
                },
                rm: reg.encoding(),
                mode: 0b11,
            },

            ast::Operand::Address(_, expr, data_size, seg) => {
                let address = if let ast::Expression::Value(_, ast::Value::Constant(value)) = expr {
                    *value
                } else {
                    panic!("Expression value is not a constant");
                };

                let size = if let Some(data_size) = data_size {
                    match data_size {
                        ast::DataSize::Byte => 1,
                        ast::DataSize::Word => 2,
                    }
                } else {
                    0
                };

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
                    displacement_size: 2,
                    mode: 0b00,
                    rm: 0b110,
                }
            }

            todo => todo!("{:?}", todo),
        }
    }
}

fn store_instruction(
    op_code: u8,
    rm: &OperandData,
    reg: u8,
    imm_size: u8,
    immediate: i32,
    emitter: &mut impl ByteEmitter,
) {
    if rm.segment_prefix != 0 {
        emitter.emit(rm.segment_prefix);
    }

    let modrm = (rm.mode << 6) + (reg << 3) + rm.rm;

    emitter.emit(op_code);
    emitter.emit(modrm);

    if rm.displacement_size == 1 {
        emitter.emit(rm.displacement as i8 as u8);
    } else if rm.displacement_size == 2 {
        for byte in (rm.displacement as i16 as u16).to_le_bytes() {
            emitter.emit(byte);
        }
    }

    if imm_size == 1 {
        emitter.emit(immediate as u8);
    } else if imm_size == 2 {
        for byte in (immediate as u16).to_le_bytes() {
            emitter.emit(byte);
        }
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
        ($operation:ident, $dst:expr, $src:expr $(,)?) => {{
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

    #[test]
    fn group_1() {
        fn group_1_op(op: Operation, base: u8) {
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

        group_1_op(Operation::ADD, 0x00);
        group_1_op(Operation::OR, 0x08);
        group_1_op(Operation::ADC, 0x10);
        group_1_op(Operation::SBB, 0x18);
        group_1_op(Operation::AND, 0x20);
        group_1_op(Operation::SUB, 0x28);
        group_1_op(Operation::XOR, 0x30);
        group_1_op(Operation::CMP, 0x38);
    }
}
