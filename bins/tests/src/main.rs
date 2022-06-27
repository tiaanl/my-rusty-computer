use mrc_decoder::decode_instruction;
use mrc_instruction::{
    Address, AddressingMode, Displacement, Immediate, Instruction, Operand, OperandSet,
    OperandSize, Operation, RegisterEncoding, Segment, SizedRegisterEncoding,
};
use std::fs::File;
use std::io::{Read, Write};
use std::process::{Command, Stdio};

const OPERATIONS: &[Operation] = &[
    // Operation::MOV,
    // Operation::PUSH,
    // Operation::POP,
    // Operation::XCHG,
    // Operation::IN,
    // Operation::OUT,
    // Operation::XLAT,
    // Operation::LEA,
    // Operation::LDS,
    // Operation::LES,
    // Operation::LAHF,
    // Operation::SAHF,
    // Operation::PUSHF,
    // Operation::POPF,
    // Operation::ADD,
    // Operation::ADC,
    // Operation::INC,
    // Operation::AAA,
    // Operation::DAA,
    // Operation::SUB,
    // Operation::SBB,
    // Operation::DEC,
    // Operation::NEG,
    // Operation::CMP,
    // Operation::AAS,
    // Operation::DAS,
    // Operation::MUL,
    // Operation::IMUL,
    // Operation::AAM,
    // Operation::DIV,
    // Operation::IDIV,
    // Operation::AAD,
    // Operation::CBW,
    // Operation::CWD,
    // Operation::NOT,
    // Operation::SHL,
    // Operation::SHR,
    // Operation::SAR,
    // Operation::ROL,
    // Operation::ROR,
    // Operation::RCL,
    // Operation::RCR,
    // Operation::AND,
    // Operation::TEST,
    // Operation::OR,
    // Operation::XOR,
    // Operation::REP,
    // Operation::MOVSB,
    // Operation::MOVSW,
    // Operation::CMPSB,
    // Operation::CMPSW,
    // Operation::SCASB,
    // Operation::SCASW,
    // Operation::LODSB,
    // Operation::LODSW,
    // Operation::STOSB,
    // Operation::STOSW,
    // Operation::CALL,
    // Operation::JMP,
    // Operation::RET,
    // Operation::JE,
    // Operation::JL,
    // Operation::JLE,
    // Operation::JB,
    // Operation::JBE,
    // Operation::JP,
    // Operation::JO,
    // Operation::JS,
    // Operation::JNE,
    // Operation::JNL,
    // Operation::JNLE,
    // Operation::JNB,
    // Operation::JNBE,
    // Operation::JNP,
    // Operation::JNO,
    // Operation::JNS,
    // Operation::LOOP,
    // Operation::LOOPE,
    // Operation::LOOPNE,
    // Operation::JCXZ,
    // Operation::INT,
    // Operation::INT1,
    // Operation::INT3,
    // Operation::INTO,
    // Operation::IRET,
    // Operation::CLC,
    // Operation::CMC,
    // Operation::STC,
    // Operation::CLD,
    // Operation::STD,
    // Operation::CLI,
    // Operation::STI,
    // Operation::HLT,
    // Operation::WAIT,
    // Operation::ESC,
    // Operation::LOCK,
    // Operation::NOP,
    // Operation::SALC,
];

fn test_instruction(instruction: Instruction) {
    let mut asm_file = std::fs::File::create("test.asm").unwrap();
    asm_file
        .write_all(String::from("BITS 16\n").as_bytes())
        .unwrap();
    asm_file
        .write_all(format!("{}", instruction).as_bytes())
        .unwrap();
    drop(asm_file);

    let status = Command::new("nasm")
        .stderr(Stdio::null())
        .arg("test.asm")
        .arg("-w+error=number-overflow")
        .arg("-f")
        .arg("bin")
        .arg("-o")
        .arg("test.bin")
        .status()
        .unwrap();

    if !status.success() {
        return;
    }

    let mut v = Vec::with_capacity(64);
    File::open("test.bin").unwrap().read_to_end(&mut v).unwrap();

    if let Some(byte) = v.get(0) {
        if *byte == 0x0f {
            return;
        }
    }

    let mut it = v.clone().into_iter();
    match decode_instruction(&mut it) {
        Ok(new_instruction) => {
            if instruction == new_instruction {
                return;
            }

            if matches!(
                new_instruction.operands,
                OperandSet::DestinationAndSource(_, Operand::Immediate(Immediate::Word(_)))
            ) && matches!(
                instruction.operands,
                OperandSet::DestinationAndSource(_, Operand::Immediate(Immediate::Byte(_)))
            ) {
                return;
            }

            println!("No match for {}", instruction);
            print!("bytes: ");
            for byte in v.iter() {
                print!("{:02X} ", byte);
            }
            println!();
            println!("mrc:   {:?}", instruction);
            println!("nasm:  {:?}", new_instruction);
            // panic!()
        }
        Err(err) => eprintln!("Could not compile: {} {}", instruction, err),
    }
}

fn iterate_operand(f: impl Fn(Operand)) {
    // Immediate

    f(Operand::Immediate(Immediate::Byte(0x81)));
    f(Operand::Immediate(Immediate::Word(0x8001)));

    // Segment

    for segment in [Segment::DS, Segment::CS, Segment::SS, Segment::SS] {
        f(Operand::Segment(segment));
    }

    // Register

    for operand_size in [OperandSize::Byte, OperandSize::Word] {
        for register in [
            RegisterEncoding::AlAx,
            RegisterEncoding::ClCx,
            RegisterEncoding::DlDx,
            RegisterEncoding::BlBx,
            RegisterEncoding::AhSp,
            RegisterEncoding::ChBp,
            RegisterEncoding::DhSi,
            RegisterEncoding::BhDi,
        ] {
            f(Operand::Register(SizedRegisterEncoding(
                register,
                operand_size,
            )));
        }
    }

    // Direct

    let mut i = 0;
    for operand_size in [OperandSize::Byte, OperandSize::Word] {
        for segment in [Segment::DS, Segment::CS, Segment::SS, Segment::SS] {
            f(Operand::Direct(segment, i, operand_size));
            i += 3;
        }
    }

    // Indirect
    // let mut i_byte = 1;
    // let mut i_word = 1;
    for operand_size in [OperandSize::Byte, OperandSize::Word] {
        for segment in [Segment::DS, Segment::CS, Segment::SS, Segment::SS] {
            for addressing_mode in [
                AddressingMode::BxSi,
                AddressingMode::BxDi,
                AddressingMode::BpSi,
                AddressingMode::BpDi,
                AddressingMode::Si,
                AddressingMode::Di,
                AddressingMode::Bp,
                AddressingMode::Bx,
            ] {
                for displacement in [
                    Displacement::None,
                    // Displacement::Byte(i_byte),
                    // Displacement::Word(i_word),
                ] {
                    f(Operand::Indirect(
                        segment,
                        addressing_mode,
                        displacement,
                        operand_size,
                    ));
                    // i_byte = i_byte.wrapping_add(3);
                    // if i_byte == 0 {
                    //     i_byte = 1
                    // }
                    // i_word = i_word.wrapping_add(7);
                    // if i_word == 0 {
                    //     i_word = 1
                    // }
                }
            }
        }
    }
}

fn main() {
    for operation in OPERATIONS {
        // OperandSet::None
        test_instruction(Instruction::new(*operation, OperandSet::None));

        // OperandSet::SegmentAndOffset
        test_instruction(Instruction::new(
            *operation,
            OperandSet::Destination(Operand::SegmentAndOffset(Address::new(0, 0))),
        ));

        // OperandSet::Displacement
        test_instruction(Instruction::new(
            *operation,
            OperandSet::Destination(Operand::Displacement(Displacement::Byte(-1))),
        ));
        test_instruction(Instruction::new(
            *operation,
            OperandSet::Destination(Operand::Displacement(Displacement::Word(-1))),
        ));

        // OperandSet::Destination
        iterate_operand(|operand| {
            test_instruction(Instruction::new(
                *operation,
                OperandSet::Destination(operand),
            ));
        });

        // OperandSet::DestinationAndSource
        iterate_operand(|destination| {
            iterate_operand(|source| {
                test_instruction(Instruction::new(
                    *operation,
                    OperandSet::DestinationAndSource(destination, source),
                ));
            });
        });
    }
}
