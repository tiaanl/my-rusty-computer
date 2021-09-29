use clap::{App, Arg};
use mrc_decoder::decode_instruction;
use std::io::Read;

struct Data {
    data: Vec<u8>,
    current_position: usize,
}

impl Data {
    fn new() -> Self {
        Self {
            data: vec![],
            current_position: 0,
        }
    }
}

impl Iterator for Data {
    type Item = u8;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.data.get(self.current_position) {
            Some(byte) => {
                self.current_position += 1;
                Some(*byte)
            }
            None => None,
        }
    }
}

fn main() {
    let matches = App::new("mrc-dis")
        .version("0.1")
        .arg(
            Arg::with_name("binary")
                .value_name("binary")
                .help("The binary file to disassemble.")
                .takes_value(true)
                .required(true),
        )
        .get_matches();

    let mut data = Data::new();

    if let Some(binary) = matches.value_of("binary") {
        if let Ok(mut file) = std::fs::File::open(binary) {
            file.read_to_end(&mut data.data).unwrap();
        }
    }

    if data.data.is_empty() {
        println!("Could not read binary file.");
        return;
    }

    // println!("{:?}", buffer);

    while data.current_position < data.data.len() {
        let start_index = data.current_position;
        match decode_instruction(&mut data) {
            Ok(instruction) => {
                let bytes_read = data.current_position - start_index;
                println!(
                    "({})    {:#010x}   {}",
                    bytes_read, data.current_position, instruction
                );
            }
            Err(err) => {
                println!("Error: {} {:?}", err, err);
                break;
            }
        }
    }
}
