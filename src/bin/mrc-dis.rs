use clap::{App, Arg};
use mrc::decoder::{decode_instruction, Decoded};
use std::io::Read;

fn main() {
    let matches = App::new("mrc-dis")
        .version("0.1")
        .arg(
            Arg::with_name("binary")
                .value_name("BINARY")
                .help("The binary file to disassemble.")
                .takes_value(true),
        )
        .get_matches();

    let mut buffer = vec![];

    if let Some(binary) = matches.value_of("binary") {
        if let Ok(mut file) = std::fs::File::open(binary) {
            file.read_to_end(&mut buffer).unwrap();
        }
    }

    if buffer.len() == 0 {
        println!("Could not read binary file.");
        return;
    }

    // println!("{:?}", buffer);

    // let mut current_address = 0usize;
    let mut current_address: usize = 0x22;
    while current_address < buffer.len() {
        match decode_instruction(&buffer[current_address..]) {
            Ok(Decoded {
                bytes_read,
                instruction,
            }) => {
                println!("{:#010x}   {}", current_address, instruction);
                current_address += bytes_read;
            }
            Err(err) => {
                println!("Error: {} {:?}", err, err);
                break;
            }
        }
    }
}
