use crate::decoder::mod_rm::RegisterOrMemory;
use crate::decoder::{ByteReader, DecodeError};
use crate::instructions::{
    DataSize, Instruction, Operand, OperandSet, Operation, RegisterEncoding, SegmentEncoding,
};

trait ByteAndExtra<'a> {
    fn byte_and_extra(&'a self) -> (u8, &'a [u8]);
}

impl<'a> ByteAndExtra<'a> for &[u8] {
    fn byte_and_extra(&'a self) -> (u8, &'a [u8]) {
        let (code, extra) = self.split_at(1);
        (code[0], extra)
    }
}

/// Holds the result for a call to [decode_instruction].
pub struct DecodeResult {
    pub bytes_read: usize,
    pub instruction: Instruction,
}

/// Takes a byte slice and tries to convert it into an [Instruction].
pub fn decode_instruction(data: &[u8]) -> Result<DecodeResult, DecodeError> {
    let (op_code, extra_bytes) = data.byte_and_extra();
    // println!("op_code, extra_bytes = {} {:?}", op_code, extra_bytes);

    match op_code {
        // Data transfer
        //

        // MOV -> Move
        //
        0xB0 | 0xB1 | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB6 | 0xB7 | 0xB8 | 0xB9 | 0xBA | 0xBB
        | 0xBC | 0xBD | 0xBE | 0xBF => {
            // 1 0 1 1 w reg => Immediate to register
            let data_size = DataSize::try_from_encoding(op_code >> 3 & 0b1)?;
            let destination =
                Operand::Register(RegisterEncoding::try_from_mod_rm_byte(op_code & 0b111)?);
            let source = Operand::Immediate(match data_size {
                DataSize::Byte => extra_bytes.read_u8()? as u16,
                DataSize::Word => extra_bytes.read_u16()?,
            });

            Ok(DecodeResult {
                bytes_read: 1 + data_size.in_bytes(),
                instruction: Instruction::new(
                    Operation::Mov,
                    OperandSet::DestinationAndSource(destination, source, data_size),
                ),
            })
        }
        0x8E => {
            // Register/memory to segment register
            // 1 0 0 0 1 1 1 0 | mod 0 segment r/m

            let (mod_rm_byte, extra_bytes) = extra_bytes.byte_and_extra();
            let segment_encoding = mod_rm_byte >> 3 & 0b11;
            let destination: Operand =
                Operand::Segment(SegmentEncoding::try_from_encoding(segment_encoding)?);
            let register_or_memory = RegisterOrMemory::try_from(mod_rm_byte, extra_bytes)?;

            Ok(DecodeResult {
                bytes_read: 2,
                instruction: Instruction::new(
                    Operation::Mov,
                    OperandSet::DestinationAndSource(
                        destination,
                        register_or_memory.into(),
                        DataSize::Word,
                    ),
                ),
            })
        }

        // JMP = Unconditional jump

        // 1 1 1 0 1 0 1 0 -> Direct intersegment
        0xEA => {
            let (offset, segment) = extra_bytes.split_at(2);
            let offset = offset.read_u16().unwrap();
            let segment = segment.read_u16().unwrap();

            Ok(DecodeResult {
                bytes_read: 5,
                instruction: Instruction::new(
                    Operation::Jmp,
                    OperandSet::SegmentAndOffset(segment, offset),
                ),
            })
        }

        // Processor control

        // CLI
        // 1 1 1 1 1 0 1 0 -> Clear interrupt
        0xFA => Ok(DecodeResult {
            bytes_read: 1,
            instruction: Instruction::new(Operation::Cli, OperandSet::None),
        }),
        _ => Err(DecodeError::InvalidOpCode(op_code)),
    }
}
