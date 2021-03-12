use mrc::decoder::decode_instruction;
use std::io::Read;

fn main() {
    let mut file = std::fs::File::open("test").unwrap();
    let mut buffer = vec![];
    file.read_to_end(&mut buffer).unwrap();

    println!("{:?}", buffer);

    /*
    let mut memory = vec![0; 1024];
    // memory[1] = 0b000001000;
    memory[0] = 0b10000000;
    */

    match decode_instruction(buffer.as_slice()) {
        Ok(instruction) => println!("instruction: {}", instruction),
        Err(message) => println!("Error: {}", message),
    }
}
