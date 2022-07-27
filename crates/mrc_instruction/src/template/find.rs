#![allow(dead_code, unused)]

use super::codes::*;
use super::templates::{Template, TEMPLATES};
use super::type_flags::*;
use crate::{
    Immediate, Instruction, Operand, OperandSet, OperandSize, Operation, RegisterEncoding, Segment,
    SizedRegisterEncoding,
};

fn find_internal(
    operation: Operation,
    dst: TypeFlags,
    src: TypeFlags,
) -> Option<&'static Template> {
    for temp in TEMPLATES {
        macro_rules! check_type_flags {
            ($tf:ident) => {{
                if $tf != T_NONE {
                    if !temp.$tf.contains(class::only($tf)) {
                        continue;
                    }

                    if !$tf.contains(sub_class::only(temp.$tf)) {
                        continue;
                    }

                    if !temp.$tf.contains(size::only($tf)) {
                        continue;
                    }
                }
            }};
        }

        if temp.operation != operation {
            continue;
        }

        // println!(
        //     "{:?} [{}] [{}] ==> [{}] [{}]",
        //     temp.operation,
        //     format_type_flags(temp.dst),
        //     format_type_flags(temp.src),
        //     format_type_flags(dst),
        //     format_type_flags(src),
        // );

        check_type_flags!(dst);
        check_type_flags!(src);

        return Some(temp);
    }
    None
}

fn operand_to_type(operand: &Operand) -> TypeFlags {
    match operand {
        Operand::Register(SizedRegisterEncoding(RegisterEncoding::AlAx, operand_size)) => {
            T_REG
                | T_REG_AL_AX
                | if let OperandSize::Byte = operand_size {
                    T_BITS_8
                } else {
                    T_BITS_16
                }
        }

        Operand::Register(SizedRegisterEncoding(_, operand_size)) => {
            T_REG
                | if let OperandSize::Byte = operand_size {
                    T_BITS_8
                } else {
                    T_BITS_16
                }
        }

        Operand::Immediate(imm) => {
            T_IMM
                | if let Immediate::Byte(_) = imm {
                    T_BITS_8
                } else {
                    T_BITS_16
                }
        }

        Operand::Direct(_, _, operand_size) => {
            T_MEM
                | if let OperandSize::Byte = operand_size {
                    T_BITS_8
                } else {
                    T_BITS_16
                }
        }

        _ => todo!("{:?}", operand),
    }
}

pub fn find(insn: &Instruction) -> Option<&'static Template> {
    let (dst, src) = match &insn.operands {
        OperandSet::DestinationAndSource(dst, src) => (operand_to_type(dst), operand_to_type(src)),
        OperandSet::Destination(dst) => (operand_to_type(dst), T_NONE),
        OperandSet::None => (T_NONE, T_NONE),
    };

    find_internal(insn.operation, dst, src)
}

macro_rules! imm8 {
    ($value:expr) => {{
        Operand::Immediate(Immediate::Byte($value))
    }};
}

macro_rules! imm16 {
    ($value:expr) => {{
        Operand::Immediate(Immediate::Word($value))
    }};
}

macro_rules! reg_internal {
    ($encoding:ident, $size:ident) => {{
        Operand::Register(SizedRegisterEncoding(
            RegisterEncoding::$encoding,
            OperandSize::$size,
        ))
    }};
}

macro_rules! reg8 {
    (al) => {{
        reg_internal!(AlAx, Byte)
    }};

    (bl) => {{
        reg_internal!(BlBx, Byte)
    }};

    (cl) => {{
        reg_internal!(ClCx, Byte)
    }};

    (dl) => {{
        reg_internal!(DlDx, Byte)
    }};
}

macro_rules! reg16 {
    (ax) => {{
        reg_internal!(AlAx, Word)
    }};

    (bx) => {{
        reg_internal!(BlBx, Word)
    }};

    (cx) => {{
        reg_internal!(ClCx, Word)
    }};

    (dx) => {{
        reg_internal!(DlDx, Word)
    }};
}

macro_rules! segment_internal {
    (es) => {{
        Segment::ES
    }};

    (cs) => {{
        Segment::CS
    }};

    (ss) => {{
        Segment::SS
    }};

    (ds) => {{
        Segment::DS
    }};
}

macro_rules! direct8 {
    ($segment:ident, $addr:literal) => {{
        Operand::Direct(segment_internal!($segment), $addr, OperandSize::Byte)
    }};
}

macro_rules! direct16 {
    ($segment:ident, $addr:literal) => {{
        Operand::Direct(segment_internal!($segment), $addr, OperandSize::Word)
    }};
}

macro_rules! insn {
    ($operation:ident) => {{
        Instruction {
            operation: Operation::$operation,
            operands: OperandSet::None,
            repeat: None,
        }
    }};

    ($operation:ident, $dst:expr) => {{
        Instruction {
            operation: Operation::$operation,
            operands: OperandSet::Destination($dst),
            repeat: None,
        }
    }};

    ($operation:ident, $dst:expr, $src:expr) => {{
        Instruction {
            operation: Operation::$operation,
            operands: OperandSet::DestinationAndSource($dst, $src),
            repeat: None,
        }
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        #[rustfmt::skip]
        let tests: &[(Instruction, &[Code])] = &[
            (insn!(AAA), &[C_BYTE, 0x37]),
            (insn!(AAD), &[C_BYTE, 0xD5, C_BYTE, 0x0A]),
            (insn!(AAM), &[C_BYTE, 0xD4, C_BYTE, 0x0A]),
            (insn!(AAS), &[C_BYTE, 0x3F]),
            (insn!(ADC, reg8!(al), imm8!(0x10)), &[C_BYTE, 0x14]),           // acc8,imm8
            (insn!(ADC, reg16!(ax), imm16!(0x100)), &[C_BYTE, 0x15]),        // acc16,imm16
            (insn!(ADC, reg8!(cl), imm8!(0x10)), &[C_BYTE, 0x80]),           // reg8,imm8
            (insn!(ADC, reg16!(cx), imm16!(0x100)), &[C_BYTE, 0x81]),        // reg16,imm16
            (insn!(ADC, reg8!(cl), reg8!(dl)), &[C_BYTE, 0x12]),             // reg8,reg8
            (insn!(ADC, reg16!(cx), reg16!(dx)), &[C_BYTE, 0x13]),           // reg16,reg16
            (insn!(ADC, reg8!(cl), direct8!(ds, 0x100)), &[C_BYTE, 0x12]),   // reg8,mem8
            (insn!(ADC, reg16!(cx), direct16!(ds, 0x100)), &[C_BYTE, 0x13]), // reg16,mem16
        ];

        for test in tests {
            let (instruction, codes) = test;
            let m = find(instruction);
            assert!(m.is_some(), "No match for [{}]", instruction);
            if let Some(m) = m {
                assert_eq!(
                    instruction.operation, m.operation,
                    "Incorrect operation for [{}]",
                    instruction
                );

                let max = codes.len();

                assert!(
                    m.codes.len() >= max,
                    "Not enough codes for [{}]",
                    instruction
                );

                assert_eq!(
                    codes[..max],
                    m.codes[..max],
                    "Incorrect codes for [{}] code:[{}] m.codes:[{}]",
                    instruction,
                    codes_to_string(codes),
                    codes_to_string(m.codes),
                );
            }
        }
    }
}
