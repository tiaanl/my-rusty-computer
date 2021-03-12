use mrc::decoder::decode_instruction;

fn main() {
    let mut memory = vec![0; 1024];

    // memory[1] = 0b000001000;
    memory[0] = 0b10000000;

    match decode_instruction(memory.as_slice()) {
        Ok(instruction) => println!("instruction: {}", instruction),
        Err(message) => println!("Error: {}", message),
    }
}
