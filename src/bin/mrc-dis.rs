use mrc::decoder::{decode_instruction, DecodeResult};
use std::io::Read;

fn main() {
    let bios_path = std::env::current_dir()
        .unwrap()
        .join("data")
        .join("bios.bin");
    let mut file = std::fs::File::open(bios_path.to_str().unwrap()).unwrap();
    let mut buffer = vec![];
    file.read_to_end(&mut buffer).unwrap();

    // println!("{:?}", buffer);

    let mut current_address = 1010usize;
    while current_address < buffer.len() {
        match decode_instruction(&buffer[current_address..]) {
            Ok(DecodeResult {
                bytes_read,
                instruction,
            }) => {
                println!("{:#010x}   {}", current_address, instruction);
                current_address += bytes_read;
            }
            Err(message) => {
                println!("Error: {}", message);
                break;
            }
        }
    }
}
