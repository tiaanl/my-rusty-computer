use mrc_emulator::{cpu::CPU, Address, Bus, Port};
use structopt::StructOpt;

#[derive(StructOpt)]
struct Opt {
    pop_path: std::path::PathBuf,
}

struct DataBus;

impl Bus<Address> for DataBus {
    fn read(&self, _address: Address) -> mrc_emulator::error::Result<u8> {
        Ok(0)
    }

    fn write(&mut self, _address: Address, _value: u8) -> mrc_emulator::error::Result<()> {
        Ok(())
    }
}

struct IO;

impl Bus<Port> for IO {
    fn read(&self, _port: u16) -> mrc_emulator::error::Result<u8> {
        Ok(0)
    }

    fn write(&mut self, _port: u16, _value: u8) -> mrc_emulator::error::Result<()> {
        Ok(())
    }
}

fn main() {
    let opts = Opt::from_args();

    println!("{:?}", opts.pop_path);

    let bus = DataBus;
    let io = IO;

    let mut cpu = CPU::new(bus, io);
    cpu.tick().unwrap();
}
