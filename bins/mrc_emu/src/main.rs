mod components;
mod config;
mod interrupts;

use crate::interrupts::InterruptManager;
use config::Config;
use mrc_decoder::decode_instruction;
use mrc_emulator::{
    components::{
        cga::Cga, dma::DirectMemoryAccessController, keyboard::Keyboard,
        pic::ProgrammableInterruptController, pit::ProgrammableIntervalTimer8253, ppi::Latch,
        ppi::ProgrammablePeripheralInterface, ram::RandomAccessMemory, rom::ReadOnlyMemory,
    },
    cpu::{ExecuteResult, CPU},
    debugger::{Debugger, DebuggerState, EmulatorCommand},
    error::Error,
    segment_and_offset, Address, Bus, Port,
};
use mrc_instruction::Segment;
use std::{
    cell::RefCell,
    io::Read,
    rc::Rc,
    sync::{mpsc::Receiver, Arc, Mutex},
};
use structopt::StructOpt;
use Segment::CS;

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
    ppi: ProgrammablePeripheralInterface<Latch, Keyboard, Latch>,
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

struct CPUIt<'cpu, D: Bus<Address>, I: Bus<Port>> {
    cpu: &'cpu CPU<D, I>,
    pub address: Address,
}

impl<'cpu, D: Bus<Address>, I: Bus<Port>> Iterator for CPUIt<'cpu, D, I> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cpu.read_from_bus(self.address) {
            Ok(byte) => {
                self.address = self.address.wrapping_add(1);
                Some(byte)
            }
            Err(_) => None,
        }
    }
}

fn update_debugger_state<D: Bus<Address>, I: Bus<Port>>(
    debugger_state: &mut DebuggerState,
    cpu: &CPU<D, I>,
) {
    debugger_state.state = cpu.state;

    let cs = cpu.state.segment(CS);
    let mut it = CPUIt {
        cpu,
        address: segment_and_offset(cs, cpu.state.ip),
    };

    debugger_state.source.resize(10, "".to_string());

    for line in debugger_state.source.iter_mut() {
        *line = if let Ok(instruction) = decode_instruction(&mut it) {
            format!(
                "{:04X}:{:04X} {}",
                cs,
                { it.address & !((cs as u32) << 4) },
                instruction,
            )
        } else {
            "ERROR".to_string()
        }
    }
}

fn run_emulator(
    bios: ReadOnlyMemory,
    debugger_state: Arc<Mutex<DebuggerState>>,
    receiver: Receiver<EmulatorCommand>,
) {
    let memory = Memory::new(bios);

    let interrupt_manager = Rc::new(RefCell::new(InterruptManager { allow_nmi: true }));
    let dma = DirectMemoryAccessController::default();
    let cga = Cga::default();
    let pit = Rc::new(RefCell::new(ProgrammableIntervalTimer8253::default()));
    let pic = ProgrammableInterruptController::default();

    let io_bus = IOBus {
        interrupt_manager,
        dma,
        cga,
        pic,
        pit: Rc::clone(&pit),
        ppi: ProgrammablePeripheralInterface::new(Latch(0x00), Keyboard::default(), Latch(0x00)),
    };

    let mut cpu = CPU::new(memory, io_bus);

    // Set the CPU reset vector: 0xFFFF0
    cpu.jump_to(0xF000, 0xFFF0);

    let mut running = false;

    {
        let mut debugger_state = debugger_state.lock().unwrap();
        update_debugger_state(&mut debugger_state, &cpu);
    }

    loop {
        pit.borrow_mut().tick();
        if !matches!(cpu.tick(), Ok(ExecuteResult::Continue)) {
            break;
        }

        if running {
            if let Ok(command) = receiver.try_recv() {
                match command {
                    EmulatorCommand::Run => running = true,
                    EmulatorCommand::Stop => running = false,
                    EmulatorCommand::Step => running = false,
                }
            }
        } else {
            // Send a new state to the debugger and wait for a command:
            if let Ok(command) = receiver.recv() {
                match command {
                    EmulatorCommand::Run => running = true,
                    EmulatorCommand::Stop => running = false,
                    EmulatorCommand::Step => running = false,
                }
            }
        }

        {
            let mut debugger_state = debugger_state.lock().unwrap();
            update_debugger_state(&mut debugger_state, &cpu);
        }
    }
}

fn main() {
    pretty_env_logger::init();

    let config = Config::from_args();

    let bios = create_bios(config.bios);

    let event_loop = glutin::event_loop::EventLoop::new();

    let window_builder = glutin::window::WindowBuilder::new()
        .with_resizable(true)
        .with_inner_size(glutin::dpi::LogicalSize {
            width: 800.0,
            height: 600.0,
        })
        .with_title("My Rusty Computer (Debugger)");

    let context_builder = glutin::ContextBuilder::new()
        .with_depth_buffer(0)
        .with_srgb(true)
        .with_stencil_buffer(0)
        .with_vsync(true);

    let display = glium::Display::new(window_builder, context_builder, &event_loop)
        .expect("Could not create display.");

    let (command_sender, command_receiver) = std::sync::mpsc::channel();

    let debugger_state = Arc::new(Mutex::new(DebuggerState::default()));

    let mut debugger = Debugger::new(&display, debugger_state.clone(), command_sender);

    std::thread::spawn(move || {
        run_emulator(bios, debugger_state, command_receiver);
    });

    event_loop.run(move |event, _, control_flow| {
        let mut redraw = || {
            let needs_repaint = debugger.needs_redraw(&display);

            *control_flow = if needs_repaint {
                display.gl_window().window().request_redraw();
                glutin::event_loop::ControlFlow::Poll
            } else {
                glutin::event_loop::ControlFlow::Wait
            };

            {
                let mut frame = display.draw();
                debugger.draw(&display, &mut frame);
                frame.finish().unwrap();
            }
        };

        match event {
            // Platform-dependent event handlers to workaround a winit bug
            // See: https://github.com/rust-windowing/winit/issues/987
            // See: https://github.com/rust-windowing/winit/issues/1619
            glutin::event::Event::RedrawEventsCleared if cfg!(windows) => redraw(),
            glutin::event::Event::RedrawRequested(_) if !cfg!(windows) => redraw(),

            glutin::event::Event::WindowEvent { event, .. } => {
                use glutin::event::WindowEvent;
                if matches!(event, WindowEvent::CloseRequested | WindowEvent::Destroyed) {
                    *control_flow = glutin::event_loop::ControlFlow::Exit;
                }

                debugger.on_event(&event);

                display.gl_window().window().request_redraw(); // TODO: ask egui if the events warrants a repaint instead
            }

            _ => {}
        }
    });
}
