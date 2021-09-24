use clap::{App, Arg};
use mrc_dos::mz::MzHeader;
use std::io::Read;

fn print_mz_header(header: &MzHeader) {
    macro_rules! print_value {
        ($field:ident) => {{
            println!(
                "{:<24}{:#06X} ({})",
                stringify!($field),
                header.$field,
                header.$field
            );
        }};
    }

    unsafe {
        print_value!(id);
        print_value!(extra_bytes);
        print_value!(pages);
        print_value!(relocation_items);
        print_value!(header_size);
        print_value!(minimum_allocation);
        print_value!(maximum_allocation);
        print_value!(initial_ss);
        print_value!(initial_sp);
        print_value!(checksum);
        print_value!(initial_ip);
        print_value!(initial_cs);
        print_value!(relocation_table);
        print_value!(overlay);
    }
}

fn main() {
    let matches = App::new("mz-inspect")
        .version("0.1.0")
        .arg(
            Arg::with_name("binary")
                .value_name("BINARY")
                .help("The .EXE file to inspect.")
                .required(true)
                .takes_value(true),
        )
        .get_matches();

    // We can unwrap, because clap will stop if binary is not provided.
    let path = matches.value_of("binary").unwrap();

    let mut file = match std::fs::File::open(path) {
        Err(err) => {
            eprintln!("Could not open file ({})", err);
            return;
        }
        Ok(file) => file,
    };

    let mut header: MzHeader = unsafe { std::mem::zeroed() };
    let header_slice = unsafe {
        std::slice::from_raw_parts_mut(
            &mut header as *mut _ as *mut u8,
            std::mem::size_of::<MzHeader>(),
        )
    };

    if let Err(err) = file.read_exact(header_slice) {
        eprintln!("Could not read binary file. ({})", err);
    }

    println!("{:<24}{}", "File", path);
    println!("{:<24}{}", "Header size", header.header_size_in_bytes());
    println!(
        "{:<24}{}",
        "Calculated file size",
        header.file_size_in_bytes()
    );
    println!();
    print_mz_header(&header);
}
