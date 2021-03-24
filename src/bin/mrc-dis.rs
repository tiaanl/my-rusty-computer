use mrc::decoder::{decode_instruction, DecodeResult};
use std::io::Read;

fn main() {
    let mut file = std::fs::File::open("C:\\Code\\my-rusty-computer\\data\\bios.bin").unwrap();
    let mut buffer = vec![];
    file.read_to_end(&mut buffer).unwrap();

    // println!("{:?}", buffer);

    let mut current_address = 0usize;
    while current_address < buffer.len() {
        match decode_instruction(&buffer[current_address..]) {
            Ok(DecodeResult {
                bytes_read,
                instruction,
            }) => {
                println!(
                    "{:#06x}:{:#06x}   {}",
                    current_address & 0xffff0000,
                    current_address >> 32usize,
                    instruction
                );
                current_address += bytes_read;
            }
            Err(message) => {
                println!("Error: {}", message);
                break;
            }
        }
    }
}
