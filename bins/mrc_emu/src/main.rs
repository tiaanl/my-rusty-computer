mod components;
mod config;
mod debugger;
mod interrupts;

use crate::components::pic::ProgrammableInterruptController;
use crate::{
    components::{
        cga::Cga, dma::DirectMemoryAccessController, pit::ProgrammableIntervalTimer8253,
        ppi::ProgrammablePeripheralInterface,
    },
    interrupts::InterruptManager,
};
use config::Config;
use mrc_emulator::{
    components::{ram::RandomAccessMemory, rom::ReadOnlyMemory},
    cpu::{ExecuteResult, CPU},
    error::Error,
    Address, Bus, Port,
};
use std::{cell::RefCell, io::Read, rc::Rc};
use structopt::StructOpt;

const MEMORY_MAX: usize = 0x100000;
const BIOS_SIZE: usize = 0x2000;

const BIOS_START: Address = (MEMORY_MAX - BIOS_SIZE) as Address;

fn load_rom(path: impl AsRef<std::path::Path>) -> std::io::Result<Vec<u8>> {
    let metadata = std::fs::metadata(&path)?;
    let file_size = metadata.len();

    let mut data = vec![0; file_size as usize];

    std::fs::File::open(&path)?.read_exact(data.as_mut_slice())?;

    Ok(data)
}

/// Create a [ReadOnlyMemory] instance that has a fixed `size`.  If the path is valid and could
/// be loaded from disk, then it will be mapped to the beginning of the block.
fn create_bios(path: Option<String>) -> ReadOnlyMemory {
    let bios = if let Some(path) = path {
        if let Ok(mut data) = load_rom(&path) {
            if data.len() > BIOS_SIZE {
                log::warn!(
                    "BIOS file truncated from {} to {}. ({})",
                    data.len(),
                    BIOS_SIZE,
                    &path
                );
            }
            data.resize(BIOS_SIZE, 0);
            Some(ReadOnlyMemory::from_vec(data))
        } else {
            None
        }
    } else {
        None
    };

    bios.unwrap_or_else(|| ReadOnlyMemory::from_vec(vec![0u8; BIOS_SIZE as usize]))
}

// fn create_emulator(
//     builder: &mut EmulatorBuilder,
//     config: &Config,
//     screen_text_mode: Arc<RwLock<TextMode>>,
// ) {
//     // 640KiB RAM
//     builder.map_address(0x00000..0xA0000, RandomAccessMemory::with_capacity(0xA0000));
//
//     // Single Intel 8259A programmable interrupt controller.
//     builder.map_io_range(0x20, 2, Rc::new(RefCell::new(Pic::default())));
//
//     // On IBM PC/XT port 0xA0 is mapped to a non-maskable interrupt (NMI) register.
//     let nmi = NMI { value: 0xA0 };
//     builder.map_io_range(0xA0, 1, Rc::new(RefCell::new(nmi)));
//
//     // Programmable Interval Timer
//     builder.map_io_range(
//         0x40,
//         0x04,
//         Rc::new(RefCell::new(ProgrammableIntervalTimer8253::default())),
//     );
//
//     /*
//     // Basic ROM just below BIOS.
//     [
//         (
//             r#"C:\Code\my-rusty-computer\bins\mrc_emu\ext\bios\BASICF6.ROM"#,
//             0xF6000,
//         ),
//         (
//             r#"C:\Code\my-rusty-computer\bins\mrc_emu\ext\bios\BASICF8.ROM"#,
//             0xF8000,
//         ),
//         (
//             r#"C:\Code\my-rusty-computer\bins\mrc_emu\ext\bios\BASICFA.ROM"#,
//             0xFA000,
//         ),
//         (
//             r#"C:\Code\my-rusty-computer\bins\mrc_emu\ext\bios\BASICFC.ROM"#,
//             0xFC000,
//         ),
//     ]
//         .map(|(path, address)| {
//             let data = load_rom(path).unwrap();
//             let data_len = data.len() as u32;
//             let rom = ReadOnlyMemory::from_vec(data);
//             emulator
//                 .bus()
//                 .borrow_mut()
//                 .map(address, data_len, Rc::new(RefCell::new(rom)));
//             log::info!(
//             "Loading ROM \"{}\" to [{:05X}..{:05X}]",
//             path,
//             address,
//             address + data_len
//         );
//         });
//     */
//
//     if let Some(bios_path) = &config.bios {
//         install_bios(builder, bios_path);
//     } else {
//         let data = include_bytes!("../ext/mrc_bios/bios.bin");
//         let data = Vec::from(*data);
//         let data_size = data.len() as u32;
//         let bios_start_addr = 0x100000 - data_size;
//
//         builder.map_address(
//             bios_start_addr..bios_start_addr + data_size,
//             ReadOnlyMemory::from_vec(data),
//         );
//     }
//
//     builder.map_address(0xB8000..0xBC000, TextModeInterface::new(screen_text_mode));
//     // builder.map_interrupt(0x10, TextModeInterface::new(screen_text_mode));
//
//     builder.reset_vector(0xF000, 0xFFF0);
// fn main() {
//     pretty_env_logger::init();
//
//     let config = Config::from_args();
//
//     let event_loop = glutin::event_loop::EventLoop::new();
//
//     let mut screen = Screen::new(&event_loop);
//
//     let mut last_monitor_update = Instant::now();
//
//     let mut is_debugging = false;
//
//     event_loop.run(move |event, _, control_flow| {
//         *control_flow = ControlFlow::Poll;
//
//         if let Some(cf) = screen.handle_events(&event) {
//             *control_flow = cf;
//             return;
//         }
//
//         if is_debugging {
//             // if let Some(debugger_action) = debugger.handle_events(&event) {
//             //     match debugger_action {
//             //         DebuggerAction::Step => {
//             //             emulator.write().tick();
//             //         }
//             //     }
//             //     return;
//             // }
//         } else {
//             for _ in 0..10 {
//                 let current_cpu_addr = segment_and_offset(
//                     emulator.read().cpu.state.get_segment_value(Segment::Cs),
//                     emulator.read().cpu.state.ip,
//                 );
//
//                 // fe00:00b0 = fe0b0
//                 if current_cpu_addr == 0xFE0B0 {
//                     is_debugging = true;
//                 } else {
//                     emulator.write().tick();
//                 }
//             }
//         }
//
//         // 60 fps
//         let now = Instant::now();
//         if now >= last_monitor_update + std::time::Duration::from_nanos(16_666_667) {
//             last_monitor_update = now;
//             screen.tick();
//             debugger.update(emulator.read());
//             debugger.tick();
//         }
//     });
// }
// }

struct Memory {
    ram: RandomAccessMemory,
    bios: ReadOnlyMemory,
}

impl Memory {
    fn new(bios: ReadOnlyMemory) -> Self {
        Self {
            ram: RandomAccessMemory::with_capacity(0xA0000),
            bios,
        }
    }
}

impl Bus<Address> for Memory {
    fn read(&self, address: Address) -> mrc_emulator::error::Result<u8> {
        if address >= BIOS_START {
            self.bios.read(address - BIOS_START)
        } else if address < 0xA0000 {
            self.ram.read(address)
        } else {
            Err(Error::AddressOutOfRange(address))
        }
    }

    fn write(&mut self, address: Address, value: u8) -> mrc_emulator::error::Result<()> {
        if address >= BIOS_START {
            self.bios.write(address - BIOS_START, value)
        } else if address < 0xA0000 {
            self.ram.write(address, value)
        } else {
            Err(Error::AddressOutOfRange(address))
        }
    }
}

struct IOBus {
    interrupt_manager: Rc<RefCell<InterruptManager>>,

    dma: DirectMemoryAccessController,
    cga: Cga,
    pic: ProgrammableInterruptController,
    pit: Rc<RefCell<ProgrammableIntervalTimer8253>>,
    ppi: ProgrammablePeripheralInterface,
}

impl Bus<Port> for IOBus {
    fn read(&self, address: Port) -> mrc_emulator::error::Result<u8> {
        match address {
            0x0000..=0x000D => self.dma.read(address),

            0x0020..=0x0021 => self.pic.read(address),

            0x0040..=0x005F => self.pit.borrow().read(address),

            0x0060..=0x0063 => self.ppi.read(address),

            0x0081 | 0x0082 | 0x0083 | 0x0087 => self.dma.read(address),

            _ => panic!("Reading from unsupported port: {:#06X}", address),
        }
    }

    fn write(&mut self, address: Port, value: u8) -> mrc_emulator::error::Result<()> {
        match address {
            0x0000..=0x000D => self.dma.write(address, value),

            0x0020..=0x0021 => self.pic.write(address, value),

            0x0040..=0x005F => self.pit.borrow_mut().write(address, value),

            0x0060..=0x0063 => self.ppi.write(address, value),

            0x0081 | 0x0082 | 0x0083 | 0x0087 => self.dma.write(address, value),

            0x00A0 => {
                self.interrupt_manager.borrow_mut().allow_nmi = false;
                Ok(())
            }

            0x03D0..=0x03DF => self.cga.write(address, value),

            0x03B0..=0x03BF => Ok(()),

            _ => panic!(
                "Writing {:#04x} to unsupported port: {:#06X}",
                value, address
            ),
        }
    }
}

fn main() {
    pretty_env_logger::init();

    let config = Config::from_args();

    let bios = create_bios(config.bios);
    let memory = Memory::new(bios);

    let interrupt_manager = Rc::new(RefCell::new(InterruptManager { allow_nmi: true }));
    let dma = DirectMemoryAccessController::default();
    let cga = components::cga::Cga::default();
    let pit = Rc::new(RefCell::new(
        components::pit::ProgrammableIntervalTimer8253::default(),
    ));
    let pic = ProgrammableInterruptController::default();

    let io_bus = IOBus {
        interrupt_manager,
        dma,
        cga,
        pic,
        pit: Rc::clone(&pit),
        ppi: ProgrammablePeripheralInterface::default(),
    };

    let mut cpu = CPU::new(memory, io_bus);

    // Set the CPU reset vector: 0xFFFF0
    cpu.jump_to(0xF000, 0xFFF0);

    // for _ in 0..10 {
    loop {
        pit.borrow_mut().tick();
        if !matches!(cpu.tick(), Ok(ExecuteResult::Continue)) {
            break;
        }
    }
}
