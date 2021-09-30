use byteorder::WriteBytesExt;
use rand::{Rng, SeedableRng};
use std::io::Write;

fn main() -> Result<(), std::io::Error> {
    let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(10);
    println!("seed: {:?}", rng.get_seed());

    let mut file = std::fs::File::create("test.bin")?;

    let mut bytes = [0u8; 5];
    for op_code in 0x00..=0xFF {
        file.write_u8(op_code)?;
        // Write 5 random bytes.
        for i in 0..5 {
            bytes[i] = rng.gen();
        }
        file.write(&bytes)?;
        file.write(&[0x90, 0x90, 0x90, 0x90])?;

        println!(
            "#[test]
             fn test_{:02x}() {{
                 test_decoder!(
                    &[{:#04X}, {:#04X}, {:#04X}, {:#04X}, {:#04X}, {:#04X}],
                    Instruction::new(
                        Operation::Add,
                        OperandSet::DestinationAndSource(
                            Operand(OperandType::Register(Register::BlBx), OperandSize::Byte),
                            Operand(OperandType::Register(Register::AlAx), OperandSize::Byte),
                        )
                    )
                );
            }}
        ",
            op_code, op_code, bytes[0], bytes[1], bytes[2], bytes[3], bytes[4],
        );
    }

    Ok(())
}
