use mrc_decoder::decode_instruction;
use mrc_instruction::Instruction;
use std::{
    fmt::{Display, Formatter},
    io::Read,
    str::FromStr,
};
use structopt::StructOpt;

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

struct Data {
    data: Vec<u8>,
    current_position: u32,
}

impl Data {
    fn from_buffer(buffer: Vec<u8>) -> Self {
        Self {
            data: buffer,
            current_position: 0,
        }
    }
}

impl Iterator for Data {
    type Item = u8;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.data.get(self.current_position as usize) {
            Some(byte) => {
                self.current_position += 1;
                Some(*byte)
            }
            None => None,
        }
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

fn print_instruction(addr: SegmentAndOffset, bytes: &[u8], instruction: &Instruction) {
    let bytes_to_print = 5;
    let mut b: String = bytes
        .iter()
        .take(bytes_to_print)
        .map(|b| format!("{:02X} ", b))
        .collect();

    for _ in bytes.len()..5 {
        b.push_str("   ");
    }

    println!("{}  {}  {}", addr, b, instruction);
}

fn print_data_byte(addr: SegmentAndOffset, byte: u8) {
    println!("{}  {:02X}               db {:02X}", addr, byte, byte);
}

fn print_section(section: &Section) {
    let mut it = SectionIterator {
        section,
        position: 0,
    };

    while (it.position as usize) < section.data.len() {
        let start = it.position;
        match decode_instruction(&mut it) {
            Ok(instruction) => {
                let bytes_used = it.position - start;
                let bytes = &section.data[(start as usize)..(start + bytes_used) as usize];
                print_instruction(start.relative_to(&section.addr), bytes, &instruction);
            }
            Err(_) => {
                print_data_byte(
                    start.relative_to(&section.addr),
                    section.data[start as usize],
                );
            }
        }
    }
}

fn load_data(binary: &str) -> Result<Data, std::io::Error> {
    let mut file = std::fs::File::open(binary)?;
    let mut buffer: Vec<u8> = Vec::new();
    let _ = file.read_to_end(&mut buffer)?;

    Ok(Data::from_buffer(buffer))
}

fn disassemble(binary: &str, format: BinaryFormat) -> Result<(), std::io::Error> {
    let data = load_data(binary)?;

    match format {
        BinaryFormat::Raw => {
            let section = Section::new(segment_and_offset(0, 0), data.data.as_slice());

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

fn main() {
    let opts = Opt::from_args();

    let format = if let Some(format) = opts.format {
        format
    } else {
        detect_format(opts.binary.as_str())
    };

    if let Err(err) = disassemble(opts.binary.as_str(), format) {
        eprintln!("{}", err);
    }
}
