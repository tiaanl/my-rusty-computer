use crate::InterruptManager;
use mrc_decoder::decode_instruction;
use mrc_emulator::{
    components::{
        cga::Cga,
        dma::DirectMemoryAccessController,
        keyboard::Keyboard,
        pic::ProgrammableInterruptController,
        pit::ProgrammableIntervalTimer8253,
        ppi::{Latch, ProgrammablePeripheralInterface},
        ram::RandomAccessMemory,
        rom::ReadOnlyMemory,
    },
    cpu::{ExecuteResult, CPU},
    debugger::{DebuggerState, EmulatorCommand, SourceLine},
    error::Error,
    segment_and_offset, Address, Bus, Port,
};
use mrc_instruction::Segment::CS;
use std::sync::mpsc::TryRecvError;
use std::{
    cell::RefCell,
    rc::Rc,
    sync::{mpsc::Receiver, Arc, Mutex},
};

pub const MEMORY_MAX: usize = 0x100000;
pub const BIOS_SIZE: usize = 0x2000;

pub const BIOS_START: Address = (MEMORY_MAX - BIOS_SIZE) as Address;

pub struct DataBus {
    ram: RandomAccessMemory,
    bios: ReadOnlyMemory,
}

impl DataBus {
    fn new(bios: ReadOnlyMemory) -> Self {
        Self {
            ram: RandomAccessMemory::with_capacity(0xA0000),
            bios,
        }
    }
}

impl Bus<Port> for InputOutputBus {
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

impl Bus<Address> for DataBus {
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

pub struct InputOutputBus {
    interrupt_manager: Rc<RefCell<InterruptManager>>,

    dma: DirectMemoryAccessController,
    cga: Cga,
    pic: ProgrammableInterruptController,
    pit: Rc<RefCell<ProgrammableIntervalTimer8253>>,
    ppi: ProgrammablePeripheralInterface<Latch, Keyboard, Latch>,
}

pub struct Emulator {
    cpu: CPU<DataBus, InputOutputBus>,
    pit: Rc<RefCell<ProgrammableIntervalTimer8253>>,
    debugger_state: Arc<Mutex<DebuggerState>>,
}

impl Emulator {
    pub fn new(
        bios: ReadOnlyMemory,
        debugger_state: Arc<Mutex<DebuggerState>>,
        init: impl FnOnce(&mut CPU<DataBus, InputOutputBus>),
    ) -> Self {
        let data_bus = DataBus::new(bios);

        let interrupt_manager = Rc::new(RefCell::new(InterruptManager { allow_nmi: true }));
        let dma = DirectMemoryAccessController::default();
        let cga = Cga::default();
        let pit = Rc::new(RefCell::new(ProgrammableIntervalTimer8253::default()));
        let pic = ProgrammableInterruptController::default();

        let input_output_bus = InputOutputBus {
            interrupt_manager,
            dma,
            cga,
            pic,
            pit: Rc::clone(&pit),
            ppi: ProgrammablePeripheralInterface::new(
                Latch(0x00),
                Keyboard::default(),
                Latch(0x00),
            ),
        };

        let mut cpu = CPU::new(data_bus, input_output_bus);

        init(&mut cpu);

        Self {
            cpu,
            pit,
            debugger_state,
        }
    }

    pub fn run(&mut self, receiver: Receiver<EmulatorCommand>) {
        let mut running = false;

        let update_debugger = |cpu: &mut CPU<DataBus, InputOutputBus>| {
            let mut debugger_state = self.debugger_state.lock().unwrap();
            update_debugger_state(&mut debugger_state, cpu);
        };

        update_debugger(&mut self.cpu);

        loop {
            self.pit.borrow_mut().tick();
            if !matches!(self.cpu.tick(), Ok(ExecuteResult::Continue)) {
                break;
            }

            if running {
                match receiver.try_recv() {
                    Ok(command) => match command {
                        EmulatorCommand::Run => running = true,
                        EmulatorCommand::Stop => running = false,
                        EmulatorCommand::Step => running = false,
                    },

                    // Ignore errors when we try to fetch from an empty queue.
                    Err(TryRecvError::Empty) => {}
                    Err(err) => {
                        log::warn!("Could not receive emulator command! ({})", err);
                        continue;
                    }
                }
            } else {
                match receiver.recv() {
                    Ok(command) => match command {
                        EmulatorCommand::Run => running = true,
                        EmulatorCommand::Stop => running = false,
                        EmulatorCommand::Step => running = false,
                    },
                    Err(err) => {
                        log::warn!("Could not receive emulator command! ({})", err);
                        continue;
                    }
                }
            };

            update_debugger(&mut self.cpu);
        }
    }
}

struct CPUIt<'cpu, D: Bus<Address>, I: Bus<Port>> {
    cpu: &'cpu CPU<D, I>,
    pub address: Address,
}

impl<'cpu, D: Bus<Address>, I: Bus<Port>> CPUIt<'cpu, D, I> {
    fn new(cpu: &'cpu CPU<D, I>, address: Address) -> Self {
        Self { cpu, address }
    }
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
    debugger_state.source.resize(5, SourceLine::default());

    let cs = cpu.state.segment(CS);
    let ip = cpu.state.ip;

    let mut it = CPUIt::new(cpu, segment_and_offset(cs, ip));

    for line in debugger_state.source.iter_mut() {
        let address = mrc_instruction::Address::new(cs, (it.address & !((cs as u32) << 4)) as u16);
        *line = if let Ok(instruction) = decode_instruction(&mut it) {
            SourceLine {
                address,
                instruction: format!("{}", instruction),
            }
        } else {
            SourceLine {
                address,
                instruction: "ERR!".to_owned(),
            }
        }
    }
}
