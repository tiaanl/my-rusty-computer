use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "My Rusty Computer")]
struct Opts {
    #[structopt(parse(from_os_str))]
    rom_file: PathBuf,
}

fn load_file(path: impl AsRef<Path>) -> std::io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buf = vec![];
    file.read_to_end(&mut buf)?;
    Ok(buf)
}

fn main() {
    let opts = Opts::from_args();

    let data = match load_file(opts.rom_file) {
        Ok(data) => data,
        Err(err) => {
            eprintln!("Could not open ROM file. {err}");
            std::process::exit(1);
        }
    };

    let checksum = data.iter().fold(0_u8, |o, n| o.wrapping_add(*n));

    println!("Size: {:#X}", data.len());
    println!("Checksum: {checksum:#04X}");
}
