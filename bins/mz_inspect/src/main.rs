use clap::{App, Arg};
use mrc_dos::mz::{ExeHeader, MzHeader, Relocation};
use std::io::{Read, Seek, SeekFrom};

macro_rules! print_header_value {
    ($name:expr,$value:expr) => {
        let name = $name;
        let value = $value;
        println!("{:<24}{:04X} ({})", name, value, value);
    };
}

fn print_mz_header(header: &MzHeader) {
    macro_rules! print_values {
        ($field:ident) => {{
                print_header_value!(stringify!($field), header.$field);
        }};
        ($first:ident,$($rest:ident),*) => {{
            $(
                print_values!($rest);
            )+
        }};
    }

    print_values!(
        id,
        extra_bytes,
        pages,
        relocation_items,
        header_size,
        minimum_allocation,
        maximum_allocation,
        initial_ss,
        initial_sp,
        checksum,
        initial_ip,
        initial_cs,
        relocation_table,
        overlay
    );
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
            eprintln!("Could not open file. ({})", err);
            return;
        }
        Ok(file) => file,
    };

    println!("{:<24}{}", "File", path);

    let exe_header = match ExeHeader::new(&mut file) {
        Err(err) => {
            eprintln!("Could not read EXE header. ({})", err);
            return;
        }
        Ok(exe_header) => exe_header,
    };

    print_header_value!("Header size", exe_header.mz_header.header_size_in_bytes());
    print_header_value!("MZ header size", std::mem::size_of::<MzHeader>());
    print_header_value!("Code offset", exe_header.mz_header.code_offset());
    print_header_value!(
        "Relocation table size",
        exe_header.mz_header.relocation_items as usize * std::mem::size_of::<Relocation>()
    );
    print_header_value!(
        "Calculated file size",
        exe_header.mz_header.file_size_in_bytes()
    );
    println!();

    print_mz_header(&exe_header.mz_header);

    // Read the relocation table.
    let mut relocation_table: Vec<Relocation> =
        Vec::with_capacity(exe_header.mz_header.relocation_items as usize);
    relocation_table.resize(
        exe_header.mz_header.relocation_items as usize,
        Relocation::default(),
    );
    if let Err(err) = file.seek(SeekFrom::Start(
        exe_header.mz_header.relocation_table.into(),
    )) {
        unsafe {
            eprintln!(
                "Invalid relocation table position. ({:04X}) ({})",
                exe_header.mz_header.relocation_table, err
            );
        }
        return;
    }

    let slice: &mut [u8] = unsafe {
        std::slice::from_raw_parts_mut(
            relocation_table.as_mut_slice() as *mut _ as *mut u8,
            std::mem::size_of::<Relocation>() * exe_header.mz_header.relocation_items as usize,
        )
    };

    if let Err(err) = file.read_exact(slice) {
        eprintln!("Could not read relocation table! ({})", err);
        return;
    }

    if !relocation_table.is_empty() {
        println!();
        let offset = exe_header.mz_header.relocation_table;
        println!("Relocation table: (offset {:04X})", offset);
        let mut row_length = 0;
        for relocation in relocation_table {
            let segment = relocation.segment;
            let offset = relocation.offset;
            print!("{:04X}:{:04X} ", segment, offset);
            row_length += 1;
            if row_length >= 8 {
                row_length = 0;
                println!();
            }
        }
    }
}
