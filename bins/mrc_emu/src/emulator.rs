use crate::InterruptManager;
use mrc_emulator::cpu2::Intel8088;
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
    cpu::CPU,
    error::Error,
    Address, Bus, Cpu, Port,
};
use std::{cell::RefCell, rc::Rc};

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

            _ => {
                // panic!(
                //     "Writing {:#04x} to unsupported port: {:#06X}",
                //     value, address
                // );
                Ok(())
            }
        }
    }
}

// #[derive(Debug)]
// pub enum EmulatorStatus {
//     Running,
//     Paused,
// }

pub struct Emulator {
    // cpu: CPU<DataBus, InputOutputBus>,
    cpu: mrc_emulator::cpu2::Intel8088<DataBus, InputOutputBus>,
    pit: Rc<RefCell<ProgrammableIntervalTimer8253>>,
    // debugger_state: Arc<Mutex<DebuggerState>>,
    // breakpoints: Vec<Address>,

    // status: EmulatorStatus,
}

impl Emulator {
    pub fn new(
        bios: ReadOnlyMemory,
        // debugger_state: Arc<Mutex<DebuggerState>>,
        init: impl FnOnce(&mut Intel8088<DataBus, InputOutputBus>),
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

        // let mut cpu = CPU::new(data_bus, input_output_bus);
        let mut cpu = Intel8088::new(data_bus, input_output_bus);
        cpu.reset();

        init(&mut cpu);

        Self {
            cpu,
            pit,
            // debugger_state,
            // breakpoints: vec![],
            // status: EmulatorStatus::Paused,
        }
    }

    pub fn run(&mut self /*, receiver: Receiver<EmulatorCommand>*/) {
        self.update_debugger_state();

        loop {
            // let so = self.cpu.flat_address();

            // if self.breakpoints.iter().any(|&a| a == so) {
            //     self.status = EmulatorStatus::Paused;
            // }

            // const TEST_07: u32 = 0xfe285;
            //
            // const CHECKPOINTS: [(u32, &str); 15] = [
            //     (0xfe05b, "test_01"),
            //     (0xfe0b0, "test_02"),
            //     (0xfe0da, "test_03"),
            //     (0xfe158, "test_04"),
            //     (0xfe33b, "test_05"),
            //     (0xfe235, "test_06"),
            //     (TEST_07, "test_07"),
            //     (0xfe352, "test_08"),
            //     (0xfe3af, "test_09"),
            //     (0xfe3c0, "test_10"),
            //     (0xfe3f8, "test_11"),
            //     (0xfe4c7, "test_12"),
            //     (0xfe51e, "test_13"),
            //     (0xfe55c, "test_14"),
            //     (0xfe630, "INITIAL RELIABILITY TEST -- SUBROUTINES"),
            // ];
            // if let Some((addr, label)) = CHECKPOINTS.iter().find(|(addr, _)| *addr == so) {
            //     println!("CHECKPOINT REACHED: {:#06X} = {}", addr, label);
            // }

            // if so == TEST_07 {
            //     println!("BREAK!");
            //     self.status = EmulatorStatus::Paused;
            // }

            // if matches!(self.status, EmulatorStatus::Running) {
            self.cycle();
            // }

            // match self.status {
            //     EmulatorStatus::Running => match receiver.try_recv() {
            //         Ok(command) => self.handle_command(command),
            //
            //         // Ignore errors when we try to fetch from an empty queue.
            //         Err(TryRecvError::Empty) => {}
            //         Err(err) => {
            //             log::warn!("Could not receive emulator command! ({})", err);
            //             continue;
            //         }
            //     },
            //
            //     EmulatorStatus::Paused => match receiver.recv() {
            //         Ok(command) => self.handle_command(command),
            //         Err(err) => {
            //             log::warn!("Could not receive emulator command! ({})", err);
            //             continue;
            //         }
            //     },
            // };

            // if let Some(address) = self
            //     .breakpoints
            //     .iter()
            //     .find(|bp| **bp == self.cpu.flat_address())
            // {
            //     log::trace!("Breakpoint: {:#07X}", address);
            //     self.status = EmulatorStatus::Paused;
            // }
        }
    }

    /// Perform a sincle clock cycle on the system.
    fn cycle(&mut self) {
        self.pit.borrow_mut().tick();

        // For not just run for an arbitraty amount of cycles.
        self.cpu.cycle(50);
        self.update_debugger_state();
    }

    fn update_debugger_state(&mut self) {
        // if let Ok(mut debugger_state) = self.debugger_state.lock() {
        //     debugger_state.status = format!("{:?}", self.status);
        //     // debugger_state.state = self.cpu.state;
        //
        //     while debugger_state.history.len() > DEBUGGER_HISTORY_SIZE - 1 {
        //         debugger_state.history.pop_front();
        //     }
        //
        //     let mut first = None;
        //     if let Some(line) = debugger_state.source.get(0) {
        //         first = Some(line.clone())
        //     };
        //     if let Some(line) = first {
        //         debugger_state.history.push_back(line);
        //     }
        //
        //     // let cs = self.cpu.state.segment(CS);
        //     // let ip = self.cpu.state.ip;
        //
        //     // let mut it = CPUIt::new(&self.cpu, self.cpu.flat_address());
        //
        //     // for line in debugger_state.source.iter_mut() {
        //     //     let address =
        //     //         mrc_instruction::Address::new(cs, (it.address & !((cs as u32) << 4)) as u16);
        //     //     *line = if let Ok(instruction) = decode_instruction(&mut it) {
        //     //         SourceLine::new(address, format!("{}", instruction))
        //     //     } else {
        //     //         SourceLine::new(address, "ERR!".to_owned())
        //     //     }
        //     // }
        //
        //     // Copy over the breakpoint data.
        //     debugger_state.breakpoints = self
        //         .breakpoints
        //         .iter()
        //         .map(|a| format!("{a:05X}"))
        //         .collect();
        // }
    }

    // fn handle_command(&mut self, command: EmulatorCommand) {
    //     match command {
    //         EmulatorCommand::Run => {
    //             self.status = EmulatorStatus::Running;
    //         }
    //
    //         EmulatorCommand::Step => {
    //             self.pit.borrow_mut().tick();
    //             let _ = self.cpu.step();
    //             self.update_debugger_state();
    //
    //             self.status = EmulatorStatus::Paused;
    //         }
    //
    //         EmulatorCommand::Reset => {
    //             self.status = EmulatorStatus::Paused;
    //             self.cpu.reset();
    //             *self.debugger_state.lock().unwrap() = DebuggerState::default();
    //             self.update_debugger_state();
    //         }
    //
    //         EmulatorCommand::SetBreakpoint(address) => {
    //             self.breakpoints.push(address);
    //         }
    //     }
    // }
}

struct CPUIt<'cpu, D: Bus<Address>, I: Bus<Port>> {
    cpu: &'cpu CPU<D, I>,
    pub address: Address,
}

// impl<'cpu, D: Bus<Address>, I: Bus<Port>> CPUIt<'cpu, D, I> {
//     fn new(cpu: &'cpu CPU<D, I>, address: Address) -> Self {
//         Self { cpu, address }
//     }
// }

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
