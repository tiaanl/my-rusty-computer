use mrc_decoder::{decode_instruction, DecodeError, DecodedInstruction};
use mrc_instruction::{Address, RelativeToAddress};
use std::{io::Read, str::FromStr};
use structopt::StructOpt;

struct Section<'a> {
    addr: Address,
    data: &'a [u8],
}

impl<'a> Section<'a> {
    fn new(addr: Address, data: &'a [u8]) -> Self {
        Self { addr, data }
    }
}

struct SectionIterator<'a> {
    section: &'a Section<'a>,
    position: u32,
}

impl<'a> Iterator for SectionIterator<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        match self.section.data.get(self.position as usize) {
            Some(byte) => {
                self.position += 1;
                Some(*byte)
            }
            None => None,
        }
    }
}

fn print_data_byte(addr: Address, byte: u8) {
    print!("{}  {:02X}", addr, byte);
    for _ in 0..7 {
        print!("   ");
    }
    println!("db {:02X}", byte);
}

fn print_section(section: &Section) {
    let mut it = SectionIterator {
        section,
        position: 0,
    };
    loop {
        let start = it.position;
        match decode_instruction(&mut it) {
            Ok(instruction) => {
                let bytes_used = it.position - start;
                let bytes = &section.data[(start as usize)..(start + bytes_used) as usize];

                let instruction = DecodedInstruction::new(
                    start.relative_to(&section.addr),
                    bytes,
                    instruction,
                    bytes_used as u8,
                );

                println!("{}", instruction);

                // print_instruction(start.relative_to(&section.addr), bytes, &instruction);
            }
            Err(err) => {
                if let DecodeError::EndOfInput = err {
                    break;
                } else {
                    print_data_byte(
                        start.relative_to(&section.addr),
                        section.data[start as usize],
                    );
                }
            }
        }
    }
}

fn load_data(binary: &str) -> Result<Vec<u8>, std::io::Error> {
    let mut file = std::fs::File::open(binary)?;
    let mut buffer: Vec<u8> = Vec::new();
    let _ = file.read_to_end(&mut buffer)?;

    Ok(buffer)
}

fn disassemble(
    binary: &str,
    format: BinaryFormat,
    origin: u16,
    entry: u32,
) -> Result<(), std::io::Error> {
    let data = load_data(binary)?;

    match format {
        BinaryFormat::Raw => {
            let section = Section::new(
                Address::new(0, origin),
                &data.as_slice()[(entry as usize)..],
            );

            print_section(&section);
        }
        BinaryFormat::Com => {
            let section = Section::new(
                Address::new(0x0000, 0x0100),
                &data.as_slice()[(entry as usize)..],
            );

            print_section(&section);
        }
        _ => todo!("Binary format not supported: {:?}", format),
    }

    Ok(())
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
enum BinaryFormat {
    Raw,
    Com,
    Exe,
}

impl FromStr for BinaryFormat {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "raw" => Ok(BinaryFormat::Raw),
            "com" => Ok(BinaryFormat::Com),
            "exe" => Ok(BinaryFormat::Exe),
            _ => Err("supported binary formats: raw, com, exe"),
        }
    }
}

#[derive(StructOpt)]
struct Opt {
    /// The binary file to disassemble
    binary: String,

    /// Starting point from the beginning of the file
    #[structopt(short, long)]
    entry: Option<u32>,

    /// The origin offset for the first instruction. Defaults to 0000:0000
    #[structopt(short, long)]
    origin: Option<u16>,

    /// The format of the binary file
    #[structopt(short, long)]
    format: Option<BinaryFormat>,
}

fn detect_format(path: &str) -> BinaryFormat {
    if let Some(ext) = std::path::Path::new(path)
        .extension()
        .and_then(std::ffi::OsStr::to_str)
        .map(|s| s.to_lowercase())
    {
        match ext.as_str() {
            "com" => BinaryFormat::Com,
            "exe" => BinaryFormat::Exe,
            _ => BinaryFormat::Raw,
        }
    } else {
        BinaryFormat::Raw
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_format() {
        let tests = [
            ("test.com", BinaryFormat::Com),    // Com
            ("program.exe", BinaryFormat::Exe), // Exe
            ("test.rom", BinaryFormat::Raw),    // Raw
            ("TEST.EXE", BinaryFormat::Exe),    // Case-sensitivity
            ("rom", BinaryFormat::Raw),         // no extension
        ];

        for (path, expected) in tests {
            assert_eq!(detect_format(path), expected);
        }
    }
}

fn main() {
    let opts = Opt::from_args();

    let format = if let Some(format) = opts.format {
        format
    } else {
        detect_format(opts.binary.as_str())
    };

    if let Err(err) = disassemble(
        opts.binary.as_str(),
        format,
        opts.origin.unwrap_or(0),
        opts.entry.unwrap_or(0),
    ) {
        eprintln!("{}", err);
    }
}
