mod calc;
mod mrrm;
mod ops;

use crate::{segment_and_offset, Address, Bus, Port};
use bitflags::bitflags;

#[allow(dead_code)]
pub const ES: usize = 0b00;
pub const CS: usize = 0b01;
#[allow(dead_code)]
pub const SS: usize = 0b10;
#[allow(dead_code)]
pub const DS: usize = 0b11;

#[allow(dead_code)]
pub const AX: usize = 0b000;
#[allow(dead_code)]
pub const CX: usize = 0b001;
#[allow(dead_code)]
pub const DX: usize = 0b010;
#[allow(dead_code)]
pub const BX: usize = 0b011;
#[allow(dead_code)]
pub const SP: usize = 0b100;
#[allow(dead_code)]
pub const BP: usize = 0b101;
#[allow(dead_code)]
pub const SI: usize = 0b110;
#[allow(dead_code)]
pub const DI: usize = 0b111;

bitflags! {
    pub struct Flags : u16 {
        const CARRY = 1 << 0;
        const RESERVED_1 = 1 << 1;
        const PARITY = 1 << 2;
        const RESERVED_3 = 1 << 3;
        const AUX_CARRY = 1 << 4;
        const RESERVED_5 = 1 << 5;
        const ZERO = 1 << 6;
        const SIGN = 1 << 7;
        const TRAP = 1 << 8;
        const INTERRUPT = 1 << 9;
        const DIRECTION = 1 << 10;
        const OVERFLOW = 1 << 11;
    }
}

pub struct Intel8088<D: Bus<Address>, I: Bus<Port>> {
    pub registers: [u16; 8],
    pub segments: [u16; 4],
    pub ip: u16,
    pub flags: Flags,

    /// Optional segment override that will be used during memory operations. Uses default if None.
    segment_override: Option<usize>,

    data_bus: D,
    #[allow(dead_code)]
    io_bus: I,

    // States
    halted: bool,

    /// The amount of cycles that was consumed so far and that needs to be processed.
    to_consume: usize,

    /// Amount of cycles since power on.
    cycles: usize,

    #[cfg(debug_assertions)]
    last_op_code: u8,
}

trait Inc: Copy {
    fn inc(self) -> Self;
}

impl Inc for Address {
    fn inc(self) -> Self {
        self.wrapping_add(1)
    }
}

struct BusIter<'a, S: Inc, B: Bus<S>> {
    bus: &'a B,
    pos: S,
}

impl<'a, S: Inc, B: Bus<S>> Iterator for BusIter<'a, S, B> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let pos = self.pos;
        self.pos = pos.inc();
        Some(self.bus.read(pos).unwrap())
    }
}

impl<D: Bus<Address>, I: Bus<Port>> Intel8088<D, I> {
    pub fn new(data_bus: D, io_bus: I) -> Self {
        Self {
            registers: [0; 8],
            segments: [0; 4],
            ip: 0,
            flags: Flags::RESERVED_1,

            segment_override: None,

            data_bus,
            io_bus,

            halted: false,

            to_consume: 0,
            cycles: 0,

            #[cfg(debug_assertions)]
            last_op_code: 0x00,
        }
    }

    pub fn _print_state(&self) {
        macro_rules! do_flag {
            ($name:ident, $char:literal) => {{
                if self.flags.contains(Flags::$name) {
                    $char
                } else {
                    "-"
                }
            }};
        }

        let flags = format!(
            "{}{}{}{}{}{}{}{}{}{}{}{}",
            do_flag!(CARRY, "C"),
            do_flag!(RESERVED_1, "R"),
            do_flag!(PARITY, "P"),
            do_flag!(RESERVED_3, "R"),
            do_flag!(AUX_CARRY, "A"),
            do_flag!(RESERVED_5, "R"),
            do_flag!(ZERO, "Z"),
            do_flag!(SIGN, "S"),
            do_flag!(TRAP, "T"),
            do_flag!(INTERRUPT, "I"),
            do_flag!(DIRECTION, "D"),
            do_flag!(OVERFLOW, "O"),
        );

        println!(
            "AX:{:04X} BX:{:04X} CX:{:04X} DX:{:04X} SP:{:04X} BP:{:04X} SI:{:04X} DI:{:04X} | ES:{:04X} CS:{:04X} SS:{:04X} DS:{:04X} | IP:{:04X} FL:{} | {}",
            self.registers[AX],
            self.registers[BX],
            self.registers[CX],
            self.registers[DX],
            self.registers[SP],
            self.registers[BP],
            self.registers[SI],
            self.registers[DI],
            self.segments[ES],
            self.segments[CS],
            self.segments[SS],
            self.segments[DS],
            self.ip,
            flags,
            self.cycles,
        );
    }

    #[inline(always)]
    pub fn flat_address(&self) -> Address {
        segment_and_offset(self.segments[CS], self.ip)
    }

    fn fetch(&mut self) -> Result<u8, crate::error::Error> {
        let byte = self.data_bus.read(self.flat_address())?;
        self.ip = self.ip.wrapping_add(1);
        Ok(byte)
    }

    fn execute_instruction(&mut self) -> usize {
        if self.halted {
            return 0;
        }

        self._print_state();
        print!("{:04X}:{:04X}  ", self.segments[CS], self.ip);

        let decode_addr = self.flat_address();

        #[cfg(debug_assertions)]
        let pre_cycles = self.to_consume;

        let op_code = self.fetch().unwrap();

        if cfg!(debug_assertions) {
            self.last_op_code = op_code;
        }

        self.execute_op_code(op_code);

        let mut it = BusIter {
            bus: &self.data_bus,
            pos: decode_addr,
        };
        let insn = mrc_decoder::decode_instruction(&mut it).unwrap();
        println!(
            "{}",
            mrc_instruction::At {
                item: &insn,
                addr: Some(mrc_instruction::Address {
                    segment: self.segments[CS],
                    offset: self.ip,
                })
            }
        );

        #[cfg(debug_assertions)]
        debug_assert_ne!(
            pre_cycles, self.to_consume,
            "Operation should have consumed cycles (op_code: {op_code:02X})"
        );

        0
    }

    #[inline(always)]
    fn consume_cycles(&mut self, cycles: usize) {
        self.to_consume += cycles;
    }

    fn write_register_byte(&mut self, encoding: u8, value: u8) {
        let encoding = encoding as usize;
        if encoding <= 0b111 {
            if encoding & 0b100 == 0b100 {
                self.registers[encoding & 0b011] =
                    (self.registers[encoding] & 0x00FF) | ((value as u16) << 8)
            } else {
                self.registers[encoding] = (self.registers[encoding] & 0xFF00) | (value as u16)
            }
        } else {
            unreachable!("Invalid register encoding.");
        }
    }

    fn write_register_word(&mut self, encoding: u8, value: u16) {
        let encoding = encoding as usize;
        if encoding <= 0b111 {
            self.registers[encoding] = value;
        } else {
            unreachable!("Invalid register encoding.");
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) enum Operand {
    Register(u8),
    Memory(Address),
}

impl<D: Bus<Address>, I: Bus<Port>> Intel8088<D, I> {
    fn read_operand_byte(&mut self, operand: Operand) -> u8 {
        match operand {
            Operand::Register(encoding) => {
                if (encoding & 0b100) == 0 {
                    self.registers[(encoding & 0b011) as usize] as u8
                } else {
                    (self.registers[(encoding & 0b011) as usize] >> 8) as u8
                }
            }

            Operand::Memory(addr) => self.data_bus.read(addr).unwrap(),
        }
    }

    fn read_operand_word(&mut self, operand: Operand) -> u16 {
        match operand {
            Operand::Register(encoding) => self.registers[(encoding & 0b011) as usize],

            Operand::Memory(addr) => {
                let lo = self.data_bus.read(addr).unwrap();
                let hi = self.data_bus.read(addr.wrapping_add(1)).unwrap();
                u16::from_le_bytes([lo, hi])
            }
        }
    }

    fn write_operand_byte(&mut self, operand: Operand, value: u8) {
        match operand {
            Operand::Register(encoding) => {
                if (encoding & 0b100) == 0 {
                    let encoding = (encoding & 0b011) as usize;
                    let mut temp = self.registers[encoding] & 0xFF00;
                    temp |= value as u16;
                    self.registers[encoding] = temp;
                } else {
                    let encoding = (encoding & 0b011) as usize;
                    let mut temp = self.registers[encoding] & 0x00FF;
                    temp |= (value as u16) << 8;
                    self.registers[encoding] = temp;
                }
            }
            Operand::Memory(addr) => {
                self.data_bus.write(addr, value).unwrap();
            }
        }
    }

    fn write_operand_word(&mut self, operand: Operand, value: u16) {
        match operand {
            Operand::Register(encoding) => {
                let encoding = (encoding & 0b011) as usize;
                self.registers[encoding] = value;
            }
            Operand::Memory(addr) => {
                let [lo, hi] = value.to_le_bytes();
                self.data_bus.write(addr, lo).unwrap();
                self.data_bus.write(addr.wrapping_add(1), hi).unwrap();
            }
        }
    }
}

impl<D: Bus<Address>, I: Bus<Port>> Intel8088<D, I> {
    fn add_byte(dst: u8, src: u8, flags: &mut Flags) -> u8 {
        let (result, o) = dst.overflowing_add(src);

        flags.set(Flags::CARRY, o);
        flags.set(Flags::ZERO, result == 0);
        flags.set(Flags::OVERFLOW, result & 0x80 != 0);

        result
    }

    fn add_word(dst: u16, src: u16, flags: &mut Flags) -> u16 {
        let (result, o) = dst.overflowing_add(src);

        flags.set(Flags::CARRY, o);
        flags.set(Flags::ZERO, result == 0);
        flags.set(Flags::OVERFLOW, result & 0x8000 != 0);

        result
    }
}

impl<D: Bus<Address>, I: Bus<Port>> crate::Cpu for Intel8088<D, I> {
    fn reset(&mut self) {
        self.registers = [0; 8];
        self.segments = [0x0000, 0xF000, 0x0000, 0x0000];
        self.ip = 0xFFF0;
        self.flags = Flags::RESERVED_1;
        self.segment_override = None;
        self.cycles = 0;
    }

    fn cycle(&mut self, cycles: usize) {
        let mut cycles_to_run = cycles;

        while cycles_to_run >= self.to_consume {
            cycles_to_run -= self.to_consume;
            self.cycles += self.to_consume;
            self.to_consume = 0;

            // Execute the next instructon.
            self.execute_instruction();
        }

        // If the amount of cycles to consume did not fit into this run, then we will do it on the
        // next run.
        self.to_consume -= cycles_to_run;
        self.cycles += cycles_to_run;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct BusPrinter {
        name: &'static str,
    }

    impl Bus<Address> for BusPrinter {
        fn read(&self, address: Address) -> crate::error::Result<u8> {
            println!("{}: r [{:04X}]", self.name, address);
            Ok(0)
        }

        fn write(&mut self, address: Address, value: u8) -> crate::error::Result<()> {
            println!("{}: w [{:04X}] {:02X}", self.name, address, value);
            Ok(())
        }
    }

    impl Bus<Port> for BusPrinter {
        fn read(&self, port: Port) -> crate::error::Result<u8> {
            println!("{}: r [{:04X}]", self.name, port);
            Ok(0)
        }

        fn write(&mut self, port: Port, value: u8) -> crate::error::Result<()> {
            println!("{}: w [{:04X}] {:02X}", self.name, port, value);
            Ok(())
        }
    }

    impl Bus<Address> for &mut [u8] {
        fn read(&self, address: Address) -> crate::error::Result<u8> {
            if address as usize >= self.len() {
                Err(crate::error::Error::AddressOutOfRange(address))
            } else {
                Ok(self[address as usize])
            }
        }

        fn write(&mut self, address: Address, value: u8) -> crate::error::Result<()> {
            if address as usize >= self.len() {
                Err(crate::error::Error::AddressOutOfRange(address))
            } else {
                self[address as usize] = value;
                Ok(())
            }
        }
    }

    #[test]
    fn add_rm8_reg8() {
        use crate::Cpu;

        let data = &mut [0x00_u8, 0b11_001_000];

        let mut cpu = Intel8088::new(&mut data[..], BusPrinter { name: "io" });
        cpu.registers[AX] = 0x0101;
        cpu.registers[CX] = 0x0102;
        cpu.cycle(1);
        assert_eq!(0x0103, cpu.registers[AX]);

        let data = &mut [0x00_u8, 0b00_001_110, 0x04, 0x00, 0x01];

        let mut cpu = Intel8088::new(&mut data[..], BusPrinter { name: "io" });
        cpu.registers[CX] = 0x0102;
        cpu.cycle(1);
        assert_eq!(0x03, data[4]);
    }

    #[test]
    fn add_rm16_reg16() {
        use crate::Cpu;

        let data = &mut [0x01_u8, 0b11_001_000];

        let mut cpu = Intel8088::new(&mut data[..], BusPrinter { name: "io" });
        cpu.registers[AX] = 0x0101;
        cpu.registers[CX] = 0x0102;
        cpu.cycle(1);
        assert_eq!(0x0203, cpu.registers[AX]);

        let data = &mut [0x01_u8, 0b00_001_110, 0x04, 0x00, 0x02, 0x01];

        let mut cpu = Intel8088::new(&mut data[..], BusPrinter { name: "io" });
        cpu.registers[CX] = 0x0102;
        cpu.cycle(1);
        assert_eq!(0x04, data[4]);
        assert_eq!(0x02, data[5]);
    }
}
