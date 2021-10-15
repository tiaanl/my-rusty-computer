use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use bitflags::bitflags;

use mrc_decoder::decode_instruction;
use mrc_x86::{Register, Segment};

use crate::bus::{segment_and_offset, BusInterface};
pub use crate::cpu::executor::{execute, ExecuteResult};
use crate::error::{Error, Result};
use crate::io::IOController;
use crate::irq::InterruptController;

mod executor;

bitflags! {
    pub struct Flags : u16 {
        const CARRY = 1 << 0;
        const _UNDEFINED_1 = 1 << 1;
        const PARITY = 1 << 2;
        const _UNDEFINED_3 = 1 << 3;
        const AUX_CARRY = 1 << 4;
        const _UNDEFINED_5 = 1 << 5;
        const ZERO = 1 << 6;
        const SIGN = 1 << 7;
        const TRAP = 1 << 8;
        const INTERRUPT = 1 << 9;
        const DIRECTION = 1 << 10;
        const OVERFLOW = 1 << 11;
    }
}

#[derive(Copy, Clone, PartialEq)]
pub struct State {
    registers: [u16; 8],
    segments: [u16; 4],
    // TODO: Do not have this public.  Right now it is required to set the reset vector. Maybe set
    //       with a builder config?
    pub ip: u16,
    flags: Flags,
}

impl Default for State {
    fn default() -> Self {
        Self {
            registers: [0; 8],
            segments: [0; 4],
            ip: 0,
            // By default when the CPU starts, we enable interrupts.
            flags: Flags::INTERRUPT,
        }
    }
}

impl Display for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use mrc_x86::Register::*;
        use mrc_x86::Segment::*;

        write!(f, "AX: {:04X} ", self.registers[AlAx as usize])?;
        write!(f, "BX: {:04X} ", self.registers[BlBx as usize])?;
        write!(f, "CX: {:04X} ", self.registers[ClCx as usize])?;
        write!(f, "DX: {:04X} ", self.registers[DlDx as usize])?;

        write!(f, "SP: {:04X} ", self.registers[AhSp as usize])?;
        write!(f, "BP: {:04X} ", self.registers[ChBp as usize])?;
        write!(f, "SI: {:04X} ", self.registers[DhSi as usize])?;
        write!(f, "DI: {:04X} ", self.registers[BhDi as usize])?;

        write!(f, "ES: {:04X} ", self.segments[Es as usize])?;
        write!(f, "CS: {:04X} ", self.segments[Cs as usize])?;
        write!(f, "SS: {:04X} ", self.segments[Ss as usize])?;
        write!(f, "DS: {:04X} ", self.segments[Ds as usize])?;

        write!(f, "IP: {:04X} ", self.ip)?;

        macro_rules! print_flag {
            ($name:ident,$flag:expr) => {{
                if self.flags.contains($flag) {
                    write!(f, "{}", stringify!($name))?;
                } else {
                    write!(f, ".")?;
                }
            }};
        }

        print_flag!(O, Flags::OVERFLOW);
        print_flag!(D, Flags::DIRECTION);
        print_flag!(I, Flags::INTERRUPT);
        print_flag!(T, Flags::TRAP);
        print_flag!(S, Flags::SIGN);
        print_flag!(Z, Flags::ZERO);
        print_flag!(U, Flags::_UNDEFINED_5);
        print_flag!(A, Flags::AUX_CARRY);
        print_flag!(U, Flags::_UNDEFINED_3);
        print_flag!(P, Flags::PARITY);
        print_flag!(U, Flags::_UNDEFINED_1);
        print_flag!(C, Flags::CARRY);

        Ok(())
    }
}

/// An emulated 8086 CPU.  Contains all data and functions to access it.
pub struct CPU {
    // TODO: This is public because ip is public.
    pub state: State,
    io_controller: Option<Rc<RefCell<IOController>>>,
    interrupt_controller: Option<Rc<RefCell<InterruptController>>>,
    bus: Rc<RefCell<dyn BusInterface>>,
}

impl CPU {
    pub fn new(
        bus: Rc<RefCell<dyn BusInterface>>,
        io_controller: Option<Rc<RefCell<IOController>>>,
        interrupt_controller: Option<Rc<RefCell<InterruptController>>>,
    ) -> Self {
        Self {
            state: Default::default(),
            io_controller,
            interrupt_controller,
            bus,
        }
    }

    pub fn tick(&mut self) -> Result<ExecuteResult> {
        // println!("state: {}", self.state);
        // print_bus_bytes(self);

        let _start_cs = self.state.segments[Segment::Cs as usize];
        let _start_ip = self.state.ip;

        let instruction = match decode_instruction(self) {
            Ok(instruction) => instruction,
            Err(err) => {
                log::error!("CPU Error: {}", err);
                return Err(Error::DecodeError(err));
            }
        };

        // Print instruction.
        // println!("{:04X}:{:04X} {}", _start_cs, _start_ip, &instruction);

        execute(self, &instruction)
    }

    pub fn start(&mut self) -> Result<()> {
        loop {
            if let Ok(result) = self.tick() {
                if result == ExecuteResult::Stop {
                    break;
                }
            }
        }

        Ok(())
    }

    pub fn get_byte_register_value(&self, register: Register) -> u8 {
        use Register::*;

        let byte: u8 = match register {
            AlAx => self.state.registers[AlAx as usize].to_le_bytes()[0],
            ClCx => self.state.registers[ClCx as usize].to_le_bytes()[0],
            DlDx => self.state.registers[DlDx as usize].to_le_bytes()[0],
            BlBx => self.state.registers[BlBx as usize].to_le_bytes()[0],
            AhSp => self.state.registers[AlAx as usize].to_le_bytes()[1],
            ChBp => self.state.registers[ClCx as usize].to_le_bytes()[1],
            DhSi => self.state.registers[DlDx as usize].to_le_bytes()[1],
            BhDi => self.state.registers[BlBx as usize].to_le_bytes()[1],
        } as u8;
        byte
    }

    pub fn get_word_register_value(&self, register: Register) -> u16 {
        use Register::*;

        match register {
            AlAx => self.state.registers[Register::AlAx as usize],
            ClCx => self.state.registers[Register::ClCx as usize],
            DlDx => self.state.registers[Register::DlDx as usize],
            BlBx => self.state.registers[Register::BlBx as usize],
            AhSp => self.state.registers[Register::AhSp as usize],
            ChBp => self.state.registers[Register::ChBp as usize],
            DhSi => self.state.registers[Register::DhSi as usize],
            BhDi => self.state.registers[Register::BhDi as usize],
        }
    }

    pub fn set_byte_register_value(&mut self, register: Register, value: u8) {
        use Register::*;

        match register {
            AlAx => {
                let mut bytes = self.state.registers[Register::AlAx as usize].to_le_bytes();
                bytes[0] = value;
                self.state.registers[Register::AlAx as usize] = u16::from_le_bytes(bytes);
            }
            ClCx => {
                let mut bytes = self.state.registers[Register::ClCx as usize].to_le_bytes();
                bytes[0] = value;
                self.state.registers[Register::ClCx as usize] = u16::from_le_bytes(bytes);
            }
            DlDx => {
                let mut bytes = self.state.registers[Register::DlDx as usize].to_le_bytes();
                bytes[0] = value;
                self.state.registers[Register::DlDx as usize] = u16::from_le_bytes(bytes);
            }
            BlBx => {
                let mut bytes = self.state.registers[Register::BlBx as usize].to_le_bytes();
                bytes[0] = value;
                self.state.registers[Register::BlBx as usize] = u16::from_le_bytes(bytes);
            }
            AhSp => {
                let mut bytes = self.state.registers[Register::AlAx as usize].to_le_bytes();
                bytes[1] = value;
                self.state.registers[Register::AlAx as usize] = u16::from_le_bytes(bytes);
            }
            ChBp => {
                let mut bytes = self.state.registers[Register::ClCx as usize].to_le_bytes();
                bytes[1] = value;
                self.state.registers[Register::ClCx as usize] = u16::from_le_bytes(bytes);
            }
            DhSi => {
                let mut bytes = self.state.registers[Register::DlDx as usize].to_le_bytes();
                bytes[1] = value;
                self.state.registers[Register::DlDx as usize] = u16::from_le_bytes(bytes);
            }
            BhDi => {
                let mut bytes = self.state.registers[Register::BlBx as usize].to_le_bytes();
                bytes[1] = value;
                self.state.registers[Register::BlBx as usize] = u16::from_le_bytes(bytes);
            }
        };
    }

    pub fn set_word_register_value(&mut self, register: Register, value: u16) {
        use Register::*;

        match register {
            AlAx => self.state.registers[Register::AlAx as usize] = value,
            ClCx => self.state.registers[Register::ClCx as usize] = value,
            DlDx => self.state.registers[Register::DlDx as usize] = value,
            BlBx => self.state.registers[Register::BlBx as usize] = value,
            AhSp => self.state.registers[Register::AhSp as usize] = value,
            ChBp => self.state.registers[Register::ChBp as usize] = value,
            DhSi => self.state.registers[Register::DhSi as usize] = value,
            BhDi => self.state.registers[Register::BhDi as usize] = value,
        }
    }

    pub fn get_segment_value(&self, segment: Segment) -> u16 {
        use Segment::*;

        match segment {
            Es => self.state.segments[segment as usize],
            Cs => self.state.segments[segment as usize],
            Ss => self.state.segments[segment as usize],
            Ds => self.state.segments[segment as usize],
        }
    }

    pub fn set_segment_value(&mut self, segment: Segment, value: u16) {
        use Segment::*;

        match segment {
            Es => self.state.segments[segment as usize] = value,
            Cs => self.state.segments[segment as usize] = value,
            Ss => self.state.segments[segment as usize] = value,
            Ds => self.state.segments[segment as usize] = value,
        }
    }
}

/// The decoder requires an iterator to fetch bytes.
impl Iterator for CPU {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let address = segment_and_offset(self.state.segments[Segment::Cs as usize], self.state.ip);
        if let Ok(byte) = self.bus.borrow().read(address) {
            let (new_ip, overflow) = self.state.ip.overflowing_add(1);
            if overflow {
                log::error!("IP overflow!");
                None
            } else {
                self.state.ip = new_ip;
                Some(byte)
            }
        } else {
            None
        }
    }
}

fn _print_bus_bytes(cpu: &CPU) {
    print!("Bytes to decode: ");
    let start = segment_and_offset(cpu.get_segment_value(Segment::Cs), cpu.state.ip);
    for i in 0..5 {
        let addr = start + i;

        let byte = match cpu.bus.borrow().read(addr) {
            Ok(byte) => byte,
            Err(_) => break,
        };

        print!("{:02X} ", byte);
    }
    println!();
}
