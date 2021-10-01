use byteorder::WriteBytesExt;
use rand::{Rng, SeedableRng};
use std::io::Write;

fn is_segment_override(op_code: u8) -> bool {
    op_code == 0x26 || op_code == 0x36 || op_code == 0x2e || op_code == 0x3e
}

fn main() -> Result<(), std::io::Error> {
    let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(10);
    println!("seed: {:?}", rng.get_seed());

    let mut file = std::fs::File::create("test.bin")?;

    let mut bytes = [0u8; 5];
    for op_code in 0x00..=0xFF {
        file.write_u8(op_code)?;

        if is_segment_override(op_code) {
            // For segment overrides, we write a nice predictable op_code next.
            file.write_u8(0x01)?;
            bytes = [0x93, 0x57, 0x4F, 0x9E, 0x15];
        } else {
            // Write 5 random bytes.
            for b in &mut bytes {
                *b = rng.gen();
            }
        }

        file.write_all(&bytes)?;
        file.write_all(&[0x90, 0x90, 0x90, 0x90])?;

        if op_code >= 0x26 {
            println!(
                "#[test]
                 fn test_{:02x}() {{
                     test_decoder!(",
                op_code
            );
            if is_segment_override(op_code) {
                println!(
                    "&[{:#04X}, {:#04X}, {:#04X}, {:#04X}, {:#04X}, {:#04X}, {:#04X}],",
                    op_code, 0x01, bytes[0], bytes[1], bytes[2], bytes[3], bytes[4]
                );
            } else {
                println!(
                    "&[{:#04X}, {:#04X}, {:#04X}, {:#04X}, {:#04X}, {:#04X}],",
                    op_code, bytes[0], bytes[1], bytes[2], bytes[3], bytes[4]
                );
            }
            println!(
                "Instruction::new(
                            Operation::Add,
                            OperandSet::DestinationAndSource(
                                Operand(OperandType::Register(Register::BlBx), OperandSize::Byte),
                                Operand(OperandType::Register(Register::AlAx), OperandSize::Byte),
                            )
                        )
                    );
                }}
            "
            );
        }
    }

    Ok(())
}
