use mrc_decoder::{decode_instruction, DecodeError};
use mrc_instruction::{Displacement, Instruction, OperandSet};
use std::{
    fmt::{Display, Formatter},
    io::Read,
    str::FromStr,
};
use structopt::StructOpt;

#[derive(Copy, Clone)]
struct SegmentAndOffset {
    segment: u16,
    offset: u16,
}

#[inline(always)]
fn segment_and_offset(segment: u16, offset: u16) -> SegmentAndOffset {
    SegmentAndOffset { segment, offset }
}

impl From<(u16, u16)> for SegmentAndOffset {
    fn from(x: (u16, u16)) -> Self {
        Self {
            segment: x.0,
            offset: x.1,
        }
    }
}

trait ToSegmentAndOffset {
    fn relative_to(&self, segment_and_offset: &SegmentAndOffset) -> SegmentAndOffset;
}

impl ToSegmentAndOffset for u32 {
    fn relative_to(&self, addr: &SegmentAndOffset) -> SegmentAndOffset {
        let offset = (self & !((addr.segment as u32) << 4)) + addr.offset as u32;
        segment_and_offset(addr.segment, offset as u16)
    }
}

impl Display for SegmentAndOffset {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04X}:{:04X}", self.segment, self.offset)
    }
}

struct Section<'a> {
    addr: SegmentAndOffset,
    data: &'a [u8],
}

impl<'a> Section<'a> {
    fn new(addr: SegmentAndOffset, data: &'a [u8]) -> Self {
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

const BYTES_TO_PRINT: usize = 7;

fn print_data_byte(addr: SegmentAndOffset, byte: u8) {
    print!("{}  {:02X}", addr, byte);
    for _ in 0..BYTES_TO_PRINT {
        print!("   ");
    }
    println!("db {:02X}", byte);
}

struct DecodedInstruction<'a> {
    address: SegmentAndOffset,
    bytes: &'a [u8],
    instruction: Instruction,
    size: u8,
}

impl<'a> DecodedInstruction<'a> {
    fn new(address: SegmentAndOffset, bytes: &'a [u8], instruction: Instruction, size: u8) -> Self {
        Self {
            address,
            bytes,
            instruction,
            size,
        }
    }
}

impl<'a> Display for DecodedInstruction<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // address
        write!(f, "{}  ", self.address)?;

        // bytes
        let mut b: String = self
            .bytes
            .iter()
            .take(BYTES_TO_PRINT)
            .map(|b| format!("{:02X} ", b))
            .collect();

        for _ in self.bytes.len()..BYTES_TO_PRINT {
            b.push_str("   ");
        }

        write!(f, "{}  ", b)?;

        match self.instruction.operands {
            OperandSet::None => write!(f, "{}", self.instruction.operation)?,
            _ => write!(f, "{} ", self.instruction.operation)?,
        }

        match self.instruction.operands {
            OperandSet::Displacement(displacement) => match displacement {
                Displacement::None => {
                    write!(f, "{}", (self.size as u32).relative_to(&self.address))?
                }
                Displacement::Byte(displacement) => write!(
                    f,
                    "{:#06X}",
                    ((self.address.offset + (self.size as u16)) as i16) + displacement as i16
                )?,
                Displacement::Word(_) => todo!(),
            },
            OperandSet::None => {}
            _ => write!(f, "{}", self.instruction.operands)?,
        }

        Ok(())
    }
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
                segment_and_offset(0, origin),
                &data.as_slice()[(entry as usize)..],
            );

            print_section(&section);
        }
        _ => todo!(),
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
