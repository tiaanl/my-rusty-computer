mod calc;
mod calc2;
mod mrrm;
mod ops;

use crate::{segment_and_offset, Address, Bus};
use bitflags::bitflags;
use mrc_decoder::TryFromEncoding;

pub const ES: usize = 0b00;
pub const CS: usize = 0b01;
pub const SS: usize = 0b10;
pub const DS: usize = 0b11;

pub const AX: usize = 0b000;
pub const CX: usize = 0b001;
pub const DX: usize = 0b010;
pub const BX: usize = 0b011;
pub const SP: usize = 0b100;
pub const BP: usize = 0b101;
pub const SI: usize = 0b110;
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

pub struct Intel8088<D: Bus, I: Bus> {
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
    repeat: bool,

    /// The amount of cycles that was consumed so far and that needs to be processed.
    to_consume: usize,

    /// Amount of cycles since power on.
    cycles: usize,

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

struct BusIter<'a, B: Bus> {
    bus: &'a B,
    pos: Address,

    /// Setting to true will print out each byte it consumes.
    print: bool,
}

impl<'a, B: Bus> Iterator for BusIter<'a, B> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let pos = self.pos;
        self.pos = pos.inc();
        let value = self.bus.read(pos);

        if self.print {
            print!("{:02X} ", value);
        }

        Some(value)
    }
}

impl<D: Bus, I: Bus> Intel8088<D, I> {
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
            repeat: false,

            to_consume: 0,
            cycles: 0,

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

    fn fetch(&mut self) -> u8 {
        let byte = self.data_bus.read(self.flat_address());
        self.ip = self.ip.wrapping_add(1);
        byte
    }

    fn execute_instruction(&mut self) -> usize {
        if self.halted {
            return 0;
        }

        // self._print_state();
        // print!("{:04X}:{:04X}  ", self.segments[CS], self.ip);

        // self.disasm_instruction(self.flat_address());

        #[cfg(debug_assertions)]
        let pre_cycles = self.to_consume;

        let mut op_code = self.fetch();

        // Handle segment prefixes.
        if op_code == 0x2E {
            self.segment_override = Some(CS);
            op_code = self.fetch();
        }

        self.last_op_code = op_code;

        self.execute_op_code(op_code);

        // Reset any prefixes after the instruction ran.
        self.segment_override = None;

        // Ignore prefix op_codes.
        #[cfg(debug_assertions)]
        if op_code != 0x2E {
            debug_assert_ne!(
                pre_cycles, self.to_consume,
                "Operation should have consumed cycles (op_code: {op_code:02X})"
            );
        }

        0
    }

    fn disasm_instruction(&mut self, decode_addr: Address) {
        let mut it = BusIter {
            bus: &self.data_bus,
            pos: decode_addr,
            print: true,
        };
        let insn = mrc_decoder::decode_instruction(&mut it).unwrap();
        println!(
            "{}",
            mrc_instruction::DisAsmOptions {
                item: &insn,
                // addr: Some(mrc_instruction::Address {
                //     segment: self.segments[CS],
                //     offset: self.ip,
                // }),
                addr: None,
                segment_override: self
                    .segment_override
                    .map(|enc| mrc_instruction::Segment::try_from_encoding(enc as u8).unwrap()),
            }
        );
    }

    #[inline(always)]
    fn consume_cycles(&mut self, cycles: usize) {
        self.to_consume += cycles;
    }

    #[inline(always)]
    fn consume_cycles_for_operand(
        &mut self,
        operand: Operand,
        reg_cycles: usize,
        mem_cycles: usize,
    ) {
        self.consume_cycles(match operand {
            Operand::Register(..) => reg_cycles,
            Operand::Memory(..) => mem_cycles,
        });
    }

    #[inline(always)]
    fn read_register_byte(&self, encoding: usize) -> u8 {
        if encoding & 0b100 != 0 {
            // high byte
            (self.registers[encoding & 0b11] >> 8) as u8
        } else {
            // low byte
            self.registers[encoding & 0b11] as u8
        }
    }

    #[inline(always)]
    fn read_register_word(&self, encoding: usize) -> u16 {
        self.registers[encoding]
    }

    #[inline(always)]
    fn write_register_byte(&mut self, encoding: usize, value: u8) {
        let original = self.registers[encoding & 0b11];
        if encoding & 0b100 != 0 {
            self.registers[encoding & 0b11] = (original & 0x00FF) | ((value as u16) << 8);
        } else {
            self.registers[encoding & 0b11] = (original & 0xFF00) | (value as u16);
        }
    }

    #[inline(always)]
    fn write_register_word(&mut self, encoding: usize, value: u16) {
        self.registers[encoding] = value;
    }

    fn read_data_bus_byte(&self, addr: Address) -> u8 {
        self.data_bus.read(addr)
    }

    fn read_data_bus_word(&self, addr: Address) -> u16 {
        let lo = self.data_bus.read(addr);
        let hi = self.data_bus.read(addr.wrapping_add(1));
        u16::from_le_bytes([lo, hi])
    }

    fn write_data_bus_byte(&mut self, addr: Address, value: u8) {
        self.data_bus.write(addr, value);
    }

    fn write_data_bus_word(&mut self, addr: Address, value: u16) {
        let [lo, hi] = value.to_le_bytes();
        self.data_bus.write(addr, lo);
        self.data_bus.write(addr.wrapping_add(1), hi);
    }

    fn _push(&mut self, value: u16) {
        let ss = self.segments[SS];
        let sp = self.registers[SP].wrapping_sub(2);
        self.registers[SP] = sp;

        let addr = segment_and_offset(ss, sp);
        self.write_data_bus_word(addr, value);
    }

    fn pop(&mut self) -> u16 {
        let ss = self.segments[SS];
        let sp = self.read_register_word(SP);

        let value = self.read_data_bus_word(segment_and_offset(ss, sp));
        self.write_register_word(SP, sp.wrapping_add(2));

        value
    }
}

#[derive(Clone, Copy)]
pub(crate) enum Operand {
    Register(u8),
    Memory(Address),
}

impl From<Operand> for u8 {
    fn from(operand: Operand) -> Self {
        match operand {
            Operand::Register(enc) => enc,
            Operand::Memory(_) => unreachable!(),
        }
    }
}

impl From<Operand> for usize {
    fn from(operand: Operand) -> Self {
        match operand {
            Operand::Register(enc) => enc as usize,
            Operand::Memory(_) => unreachable!(),
        }
    }
}

impl<D: Bus, I: Bus> Intel8088<D, I> {
    fn _add_byte(dst: u8, src: u8, flags: &mut Flags) -> u8 {
        let (result, o) = dst.overflowing_add(src);

        flags.set(Flags::CARRY, o);
        flags.set(Flags::ZERO, result == 0);
        flags.set(Flags::OVERFLOW, result & 0x80 != 0);

        result
    }

    fn _add_word(dst: u16, src: u16, flags: &mut Flags) -> u16 {
        let (result, o) = dst.overflowing_add(src);

        flags.set(Flags::CARRY, o);
        flags.set(Flags::ZERO, result == 0);
        flags.set(Flags::OVERFLOW, result & 0x8000 != 0);

        result
    }
}

impl<D: Bus, I: Bus> crate::Cpu for Intel8088<D, I> {
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
    use tracing::warn;

    use super::*;

    struct BusPrinter {
        name: &'static str,
    }

    impl Bus for BusPrinter {
        fn read(&self, address: Address) -> u8 {
            println!("{}: r [{:04X}]", self.name, address);
            0
        }

        fn write(&mut self, address: Address, value: u8) {
            println!("{}: w [{:04X}] {:02X}", self.name, address, value);
        }
    }

    // impl Bus for BusPrinter {
    //     fn read(&self, port: Address) -> u8 {
    //         println!("{}: r [{:04X}]", self.name, port);
    //         0
    //     }

    //     fn write(&mut self, port: Address, value: u8) {
    //         println!("{}: w [{:04X}] {:02X}", self.name, port, value);
    //     }
    // }

    impl Bus for &mut [u8] {
        fn read(&self, address: Address) -> u8 {
            let address = address as usize;
            if address >= self.len() {
                warn!("Reading outside of bounds! ({:05X})", address);
                0
            } else {
                self[address]
            }
        }

        fn write(&mut self, address: Address, value: u8) {
            let address = address as usize;
            if address >= self.len() {
                warn!("Writing outside of bounds! ({:05X})", address);
            } else {
                self[address as usize] = value;
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

    #[test]
    fn test_asm() {
        let source = include_str!("tests.asm");
        let mut bytes = mrc_compiler::compile(source).unwrap();
        let bytes_len = bytes.len();
        let mut cpu = Intel8088::new(&mut bytes[..], BusPrinter { name: "io" });
        let mut failure_ip;
        while (cpu.ip as usize) < bytes_len {
            // Store the value of IP before running the next instruction so that we can decode this
            // instruction on failure.
            failure_ip = cpu.ip;

            cpu.execute_instruction();

            // Halting the CPU signals a failed test.
            if cpu.halted {
                let mut it = BusIter {
                    bus: &cpu.data_bus,
                    pos: segment_and_offset(0, failure_ip),
                    print: false,
                };
                let insn = mrc_decoder::decode_instruction(&mut it).unwrap();
                eprintln!("ERROR: {}", insn);
                break;
            }
        }
        assert_eq!(cpu.ip, 0xFFFF);
    }
}
