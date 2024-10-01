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
    Address, Bus, Cpu,
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

impl Bus for DataBus {
    fn read(&self, address: Address) -> u8 {
        if address >= BIOS_START {
            self.bios.read(address - BIOS_START)
        } else if address < 0xA0000 {
            self.ram.read(address)
        } else {
            log::warn!("Invalid address read from data bus! ({:#07X})", address);
            0
        }
    }

    fn write(&mut self, address: Address, value: u8) {
        if address >= BIOS_START {
            self.bios.write(address - BIOS_START, value)
        } else if address < 0xA0000 {
            self.ram.write(address, value)
        } else {
            log::warn!("Invalid address written to data bus! ({:#07X})", address);
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

impl Bus for InputOutputBus {
    fn read(&self, address: Address) -> u8 {
        match address {
            0x0000..=0x000D => self.dma.read(address),
            0x0020..=0x0021 => self.pic.read(address),
            0x0040..=0x005F => self.pit.borrow().read(address),
            0x0060..=0x0063 => self.ppi.read(address),
            0x0081 | 0x0082 | 0x0083 | 0x0087 => self.dma.read(address),
            _ => panic!("Reading from unsupported port: {:#06X}", address),
        }
    }

    fn write(&mut self, address: Address, value: u8) {
        match address {
            0x0000..=0x000D => self.dma.write(address, value),
            0x0020..=0x0021 => self.pic.write(address, value),
            0x0040..=0x005F => self.pit.borrow_mut().write(address, value),
            0x0060..=0x0063 => self.ppi.write(address, value),
            0x0081 | 0x0082 | 0x0083 | 0x0087 => self.dma.write(address, value),
            0x00A0 => {
                self.interrupt_manager.borrow_mut().allow_nmi = false;
            }
            0x03D0..=0x03DF => self.cga.write(address, value),
            0x03B0..=0x03BF => {}
            _ => {
                panic!(
                    "Writing {:#04x} to unsupported port: {:#06X}",
                    value, address
                );
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
    pub fn new(bios: ReadOnlyMemory) -> Self {
        let data_bus = DataBus::new(bios);

        let interrupt_manager = Rc::new(RefCell::new(InterruptManager { allow_nmi: true }));
        let dma = DirectMemoryAccessController::default();
        let cga = Cga::default();
        let pit = Rc::new(RefCell::new(ProgrammableIntervalTimer8253::default()));
        let pic = ProgrammableInterruptController::default();

        let io_bus = InputOutputBus {
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
        let mut cpu = Intel8088::new(data_bus, io_bus);
        cpu.reset();

        Self { cpu, pit }
    }

    pub fn run(&mut self) {
        loop {
            self.cycle();
        }
    }

    /// Perform a sincle clock cycle on the system.
    fn cycle(&mut self) {
        self.pit.borrow_mut().tick();

        // For not just run for an arbitraty amount of cycles.
        self.cpu.cycle(50);
    }
}
