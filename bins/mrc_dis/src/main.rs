use clap::{App, Arg};
use mrc_decoder::{decode_instruction, DataIterator};
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

impl DataIterator for Data {
    #[inline]
    fn peek(&self) -> u8 {
        *self.data.get(self.current_position).unwrap()
    }

    #[inline]
    fn consume(&mut self) -> u8 {
        let b = self.peek();
        self.advance();
        b
    }

    #[inline]
    fn advance(&mut self) {
        self.current_position += 1;
    }
}

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
