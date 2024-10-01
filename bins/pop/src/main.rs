use mrc_dos::mz;
use mrc_emulator::components::ram::RandomAccessMemory;
use mrc_emulator::cpu::ExecuteResult;
use mrc_emulator::cpu::WordRegister::SP;
use mrc_emulator::{cpu::CPU, segment_and_offset, Address, Bus};
use mrc_instruction::Segment;
use std::io::{Read, Seek};
use structopt::StructOpt;

#[derive(StructOpt)]
struct Opt {
    /// Path to where POP is installed.
    pop_path: std::path::PathBuf,
}

struct DataBus;

impl Bus for DataBus {
    fn read(&self, _address: Address) -> u8 {
        0
    }

    fn write(&mut self, _address: Address, _value: u8) {}
}

struct IO;

impl Bus for IO {
    fn read(&self, _port: Address) -> u8 {
        0
    }

    fn write(&mut self, _port: Address, _value: u8) {}
}

fn load_executable(
    memory: &mut RandomAccessMemory,
    location: Address,
    path: impl AsRef<std::path::Path>,
) -> std::io::Result<mz::MzHeader> {
    let exe_path = path.as_ref().join("POPCORN.EXE");
    println!("exe_path: {:?}", exe_path);

    let mut file = std::fs::File::open(exe_path)?;

    let exe_header = mrc_dos::mz::ExeHeader::new(&mut file)?;

    let code_offset = exe_header.mz_header.code_offset();
    let code_size = exe_header.mz_header.code_size();

    file.seek(std::io::SeekFrom::Start(code_offset.into()))?;
    let mut code = vec![0u8; code_size as usize];
    file.read_exact(&mut code)?;

    for (i, b) in code.into_iter().enumerate() {
        memory.write(location + i as Address, b);
    }

    Ok(exe_header.mz_header)
}

fn main() {
    pretty_env_logger::init();

    let opts = Opt::from_args();

    println!("{:?}", opts.pop_path);

    let mut memory = RandomAccessMemory::with_capacity(0x100000);

    let header = match load_executable(
        &mut memory,
        segment_and_offset(0x1000, 0x0000),
        opts.pop_path,
    ) {
        Err(err) => {
            log::error!("Could not load executable ({})", err);
            return;
        }

        Ok(header) => header,
    };

    let io = IO;

    let mut cpu = CPU::new(memory, io);
    cpu.jump_to(0x1000 + header.initial_cs, header.initial_ip);
    cpu.state
        .set_segment(Segment::SS, 0x1000 + header.initial_ss);
    cpu.state.set_register(SP, header.initial_sp);

    let t = std::thread::spawn(
        move || {
            while matches!(cpu.cycle_once(), Ok(ExecuteResult::Continue)) {}
        },
    );
    t.join().unwrap();
}
