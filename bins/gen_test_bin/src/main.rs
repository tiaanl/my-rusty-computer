use byteorder::WriteBytesExt;
use rand::{Rng, SeedableRng};

fn main() -> Result<(), std::io::Error> {
    let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(10);
    println!("seed: {:?}", rng.get_seed());

    let mut file = std::fs::File::create("test.bin")?;

    for op_code in 0x00..0xFF {
        file.write_u8(op_code)?;
        // Write 5 random bytes.
        for _ in 0..5 {
            file.write_u8(rng.gen())?;
        }
    }

    Ok(())
}
