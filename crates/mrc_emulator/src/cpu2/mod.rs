use crate::{segment_and_offset, Address, Bus, ExecuteError, Port};
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
}

struct OpCodeEntry<D: Bus<Address>, I: Bus<Port>> {
    exec: fn(&mut Intel8088<D, I>, u8) -> usize,
}

macro_rules! op {
    ($op_code:literal, $exec:ident) => {{
        OpCodeEntry { exec: Self::$exec }
    }};
}

impl<D: Bus<Address>, I: Bus<Port>> Intel8088<D, I> {
    pub fn new(data_bus: D, io_bus: I) -> Self {
        Self {
            registers: [0; 8],
            segments: [0; 4],
            ip: 0,
            flags: Flags::empty(),

            segment_override: None,

            data_bus,
            io_bus,
        }
    }

    pub fn _print_state(&self) {
        println!(
            "AX:{:04X} BX:{:04X} CX:{:04X} DX:{:04X} SP:{:04X} BP:{:04X} SI:{:04X} DI:{:04X} | ES:{:04X} CS:{:04X} SS:{:04X} DS:{:04X} | IP:{:04X} FL:{:04X}",
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
            self.flags,
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

    fn execute(&mut self) -> usize {
        let op_code = self.fetch().unwrap();
        let oce = &Self::OP_CODE_TABLE[op_code as usize];
        (oce.exec)(self, op_code)
    }
}

enum Operand {
    Register(usize),
    #[allow(dead_code)]
    Direct(u16),
    #[allow(dead_code)]
    Indirect(usize, i16),
}

impl<D: Bus<Address>, I: Bus<Port>> Intel8088<D, I> {
    fn address_from_operand(
        &mut self,
        operand: &Operand,
        segment_override: Option<usize>,
    ) -> Address {
        match operand {
            Operand::Register(_) => unreachable!("Address of register is not possible!"),
            Operand::Direct(offset) => {
                // Direct memory addressing uses DS as the default, unless overridden.
                let segment = segment_override.unwrap_or(DS);
                segment_and_offset(self.segments[segment], *offset)
            }
            Operand::Indirect(encoding, displacement) => {
                let mut default_segment = DS;
                let offset = match *encoding {
                    0b000 => self.registers[BX].wrapping_add(self.registers[SI]),
                    0b001 => self.registers[BX].wrapping_add(self.registers[DI]),
                    0b010 => {
                        default_segment = SS;
                        self.registers[BP].wrapping_add(self.registers[SI])
                    }
                    0b011 => {
                        default_segment = SS;
                        self.registers[BP].wrapping_add(self.registers[DI])
                    }
                    0b100 => self.registers[SI],
                    0b101 => self.registers[DI],
                    0b110 => {
                        default_segment = SS;
                        self.registers[BP]
                    }
                    0b111 => self.registers[BX],
                    _ => unreachable!("Invalid encoding"),
                }
                .wrapping_add(*displacement as u16);
                let segment = segment_override.unwrap_or(default_segment);
                segment_and_offset(self.segments[segment], offset)
            }
        }
    }

    fn mod_reg_rm_to_operands(&mut self, mrrm: u8) -> (Operand, Operand) {
        let first = Operand::Register(((mrrm >> 3) & 0b111) as usize);

        let second = match mrrm >> 6 {
            0b00 => {
                let rm = mrrm & 0b111;
                if rm == 0b110 {
                    let lo = self.fetch().unwrap();
                    let hi = self.fetch().unwrap();
                    Operand::Direct(u16::from_le_bytes([lo, hi]))
                } else {
                    todo!()
                }
            }
            0b01 => todo!(),
            0b10 => todo!(),
            0b11 => Operand::Register((mrrm & 0b111) as usize),
            _ => unreachable!(),
        };

        (first, second)
    }

    fn read_operand_byte(&mut self, operand: &Operand) -> u8 {
        match operand {
            Operand::Register(encoding) => {
                if (encoding & 0b100) == 0 {
                    self.registers[encoding & 0b011] as u8
                } else {
                    (self.registers[encoding & 0b011] >> 8) as u8
                }
            }

            o @ Operand::Direct(..) | o @ Operand::Indirect(..) => {
                let addr = self.address_from_operand(o, self.segment_override);
                self.data_bus.read(addr).unwrap()
            }
        }
    }

    fn read_operand_word(&mut self, operand: &Operand) -> u16 {
        match operand {
            Operand::Register(encoding) => self.registers[encoding & 0b011],

            o @ Operand::Direct(..) | o @ Operand::Indirect(..) => {
                let addr = self.address_from_operand(o, self.segment_override);

                let lo = self.data_bus.read(addr).unwrap();
                let hi = self.data_bus.read(addr + 1).unwrap();
                u16::from_le_bytes([lo, hi])
            }
        }
    }

    fn write_operand_byte(&mut self, operand: &Operand, value: u8) {
        match operand {
            Operand::Register(encoding) => {
                if (encoding & 0b100) == 0 {
                    let mut temp = self.registers[*encoding & 0b011] & 0xFF00;
                    temp |= value as u16;
                    self.registers[encoding & 0b011] = temp;
                } else {
                    let mut temp = self.registers[*encoding & 0b011] & 0x00FF;
                    temp |= (value as u16) << 8;
                    self.registers[encoding & 0b011] = temp;
                }
            }
            o @ Operand::Direct(..) | o @ Operand::Indirect(..) => {
                let addr = self.address_from_operand(o, self.segment_override);
                self.data_bus.write(addr, value).unwrap();
            }
        }
    }

    fn write_operand_word(&mut self, operand: &Operand, value: u16) {
        match operand {
            Operand::Register(encoding) => {
                self.registers[encoding & 0b011] = value;
            }
            o @ Operand::Direct(..) | o @ Operand::Indirect(..) => {
                let addr = self.address_from_operand(o, self.segment_override);
                let [lo, hi] = value.to_le_bytes();
                self.data_bus.write(addr, lo).unwrap();
                self.data_bus.write(addr + 1, hi).unwrap();
            }
        }
    }
}

impl<D: Bus<Address>, I: Bus<Port>> Intel8088<D, I> {
    const OP_CODE_TABLE: [OpCodeEntry<D, I>; 0x100] = [
        op!(0x00, op_add_rm8_reg8),
        op!(0x01, op_add_rm16_reg16),
        op!(0x02, op_invalid),
        op!(0x03, op_invalid),
        op!(0x04, op_invalid),
        op!(0x05, op_invalid),
        op!(0x06, op_invalid),
        op!(0x07, op_invalid),
        op!(0x08, op_invalid),
        op!(0x09, op_invalid),
        op!(0x0A, op_invalid),
        op!(0x0B, op_invalid),
        op!(0x0C, op_invalid),
        op!(0x0D, op_invalid),
        op!(0x0E, op_invalid),
        op!(0x0F, op_invalid),
        op!(0x10, op_invalid),
        op!(0x11, op_invalid),
        op!(0x12, op_invalid),
        op!(0x13, op_invalid),
        op!(0x14, op_invalid),
        op!(0x15, op_invalid),
        op!(0x16, op_invalid),
        op!(0x17, op_invalid),
        op!(0x18, op_invalid),
        op!(0x19, op_invalid),
        op!(0x1A, op_invalid),
        op!(0x1B, op_invalid),
        op!(0x1C, op_invalid),
        op!(0x1D, op_invalid),
        op!(0x1E, op_invalid),
        op!(0x1F, op_invalid),
        op!(0x20, op_invalid),
        op!(0x21, op_invalid),
        op!(0x22, op_invalid),
        op!(0x23, op_invalid),
        op!(0x24, op_invalid),
        op!(0x25, op_invalid),
        op!(0x26, op_invalid),
        op!(0x27, op_invalid),
        op!(0x28, op_invalid),
        op!(0x29, op_invalid),
        op!(0x2A, op_invalid),
        op!(0x2B, op_invalid),
        op!(0x2C, op_invalid),
        op!(0x2D, op_invalid),
        op!(0x2E, op_invalid),
        op!(0x2F, op_invalid),
        op!(0x30, op_invalid),
        op!(0x31, op_invalid),
        op!(0x32, op_invalid),
        op!(0x33, op_invalid),
        op!(0x34, op_invalid),
        op!(0x35, op_invalid),
        op!(0x36, op_invalid),
        op!(0x37, op_invalid),
        op!(0x38, op_invalid),
        op!(0x39, op_invalid),
        op!(0x3A, op_invalid),
        op!(0x3B, op_invalid),
        op!(0x3C, op_invalid),
        op!(0x3D, op_invalid),
        op!(0x3E, op_invalid),
        op!(0x3F, op_invalid),
        op!(0x40, op_invalid),
        op!(0x41, op_invalid),
        op!(0x42, op_invalid),
        op!(0x43, op_invalid),
        op!(0x44, op_invalid),
        op!(0x45, op_invalid),
        op!(0x46, op_invalid),
        op!(0x47, op_invalid),
        op!(0x48, op_invalid),
        op!(0x49, op_invalid),
        op!(0x4A, op_invalid),
        op!(0x4B, op_invalid),
        op!(0x4C, op_invalid),
        op!(0x4D, op_invalid),
        op!(0x4E, op_invalid),
        op!(0x4F, op_invalid),
        op!(0x50, op_invalid),
        op!(0x51, op_invalid),
        op!(0x52, op_invalid),
        op!(0x53, op_invalid),
        op!(0x54, op_invalid),
        op!(0x55, op_invalid),
        op!(0x56, op_invalid),
        op!(0x57, op_invalid),
        op!(0x58, op_invalid),
        op!(0x59, op_invalid),
        op!(0x5A, op_invalid),
        op!(0x5B, op_invalid),
        op!(0x5C, op_invalid),
        op!(0x5D, op_invalid),
        op!(0x5E, op_invalid),
        op!(0x5F, op_invalid),
        op!(0x60, op_invalid),
        op!(0x61, op_invalid),
        op!(0x62, op_invalid),
        op!(0x63, op_invalid),
        op!(0x64, op_invalid),
        op!(0x65, op_invalid),
        op!(0x66, op_invalid),
        op!(0x67, op_invalid),
        op!(0x68, op_invalid),
        op!(0x69, op_invalid),
        op!(0x6A, op_invalid),
        op!(0x6B, op_invalid),
        op!(0x6C, op_invalid),
        op!(0x6D, op_invalid),
        op!(0x6E, op_invalid),
        op!(0x6F, op_invalid),
        op!(0x70, op_invalid),
        op!(0x71, op_invalid),
        op!(0x72, op_invalid),
        op!(0x73, op_invalid),
        op!(0x74, op_invalid),
        op!(0x75, op_invalid),
        op!(0x76, op_invalid),
        op!(0x77, op_invalid),
        op!(0x78, op_invalid),
        op!(0x79, op_invalid),
        op!(0x7A, op_invalid),
        op!(0x7B, op_invalid),
        op!(0x7C, op_invalid),
        op!(0x7D, op_invalid),
        op!(0x7E, op_invalid),
        op!(0x7F, op_invalid),
        op!(0x80, op_invalid),
        op!(0x81, op_invalid),
        op!(0x82, op_invalid),
        op!(0x83, op_invalid),
        op!(0x84, op_invalid),
        op!(0x85, op_invalid),
        op!(0x86, op_invalid),
        op!(0x87, op_invalid),
        op!(0x88, op_invalid),
        op!(0x89, op_invalid),
        op!(0x8A, op_invalid),
        op!(0x8B, op_invalid),
        op!(0x8C, op_invalid),
        op!(0x8D, op_invalid),
        op!(0x8E, op_invalid),
        op!(0x8F, op_invalid),
        op!(0x90, op_invalid),
        op!(0x91, op_invalid),
        op!(0x92, op_invalid),
        op!(0x93, op_invalid),
        op!(0x94, op_invalid),
        op!(0x95, op_invalid),
        op!(0x96, op_invalid),
        op!(0x97, op_invalid),
        op!(0x98, op_invalid),
        op!(0x99, op_invalid),
        op!(0x9A, op_invalid),
        op!(0x9B, op_invalid),
        op!(0x9C, op_invalid),
        op!(0x9D, op_invalid),
        op!(0x9E, op_invalid),
        op!(0x9F, op_invalid),
        op!(0xA0, op_invalid),
        op!(0xA1, op_invalid),
        op!(0xA2, op_invalid),
        op!(0xA3, op_invalid),
        op!(0xA4, op_invalid),
        op!(0xA5, op_invalid),
        op!(0xA6, op_invalid),
        op!(0xA7, op_invalid),
        op!(0xA8, op_invalid),
        op!(0xA9, op_invalid),
        op!(0xAA, op_invalid),
        op!(0xAB, op_invalid),
        op!(0xAC, op_invalid),
        op!(0xAD, op_invalid),
        op!(0xAE, op_invalid),
        op!(0xAF, op_invalid),
        op!(0xB0, op_invalid),
        op!(0xB1, op_invalid),
        op!(0xB2, op_invalid),
        op!(0xB3, op_invalid),
        op!(0xB4, op_invalid),
        op!(0xB5, op_invalid),
        op!(0xB6, op_invalid),
        op!(0xB7, op_invalid),
        op!(0xB8, op_invalid),
        op!(0xB9, op_invalid),
        op!(0xBA, op_invalid),
        op!(0xBB, op_invalid),
        op!(0xBC, op_invalid),
        op!(0xBD, op_invalid),
        op!(0xBE, op_invalid),
        op!(0xBF, op_invalid),
        op!(0xC0, op_invalid),
        op!(0xC1, op_invalid),
        op!(0xC2, op_invalid),
        op!(0xC3, op_invalid),
        op!(0xC4, op_invalid),
        op!(0xC5, op_invalid),
        op!(0xC6, op_invalid),
        op!(0xC7, op_invalid),
        op!(0xC8, op_invalid),
        op!(0xC9, op_invalid),
        op!(0xCA, op_invalid),
        op!(0xCB, op_invalid),
        op!(0xCC, op_invalid),
        op!(0xCD, op_invalid),
        op!(0xCE, op_invalid),
        op!(0xCF, op_invalid),
        op!(0xD0, op_invalid),
        op!(0xD1, op_invalid),
        op!(0xD2, op_invalid),
        op!(0xD3, op_invalid),
        op!(0xD4, op_invalid),
        op!(0xD5, op_invalid),
        op!(0xD6, op_invalid),
        op!(0xD7, op_invalid),
        op!(0xD8, op_invalid),
        op!(0xD9, op_invalid),
        op!(0xDA, op_invalid),
        op!(0xDB, op_invalid),
        op!(0xDC, op_invalid),
        op!(0xDD, op_invalid),
        op!(0xDE, op_invalid),
        op!(0xDF, op_invalid),
        op!(0xE0, op_invalid),
        op!(0xE1, op_invalid),
        op!(0xE2, op_invalid),
        op!(0xE3, op_invalid),
        op!(0xE4, op_invalid),
        op!(0xE5, op_invalid),
        op!(0xE6, op_invalid),
        op!(0xE7, op_invalid),
        op!(0xE8, op_invalid),
        op!(0xE9, op_invalid),
        op!(0xEA, op_invalid),
        op!(0xEB, op_invalid),
        op!(0xEC, op_invalid),
        op!(0xED, op_invalid),
        op!(0xEE, op_invalid),
        op!(0xEF, op_invalid),
        op!(0xF0, op_invalid),
        op!(0xF1, op_invalid),
        op!(0xF2, op_invalid),
        op!(0xF3, op_invalid),
        op!(0xF4, op_invalid),
        op!(0xF5, op_invalid),
        op!(0xF6, op_invalid),
        op!(0xF7, op_invalid),
        op!(0xF8, op_invalid),
        op!(0xF9, op_invalid),
        op!(0xFA, op_invalid),
        op!(0xFB, op_invalid),
        op!(0xFC, op_invalid),
        op!(0xFD, op_invalid),
        op!(0xFE, op_invalid),
        op!(0xFF, op_invalid),
    ];

    fn op_add_rm8_reg8(&mut self, _op_code: u8) -> usize {
        let mrrm = self.fetch().unwrap();
        let (src_op, dst_op) = self.mod_reg_rm_to_operands(mrrm);

        let dst = self.read_operand_byte(&dst_op);
        let src = self.read_operand_byte(&src_op);

        let dst = Self::add_byte(dst, src, &mut self.flags);
        self.write_operand_byte(&dst_op, dst);

        0
    }

    fn op_add_rm16_reg16(&mut self, _op_code: u8) -> usize {
        let mrrm = self.fetch().unwrap();
        let (src_op, dst_op) = self.mod_reg_rm_to_operands(mrrm);

        let dst = self.read_operand_word(&dst_op);
        let src = self.read_operand_word(&src_op);

        let dst = Self::add_word(dst, src, &mut self.flags);
        self.write_operand_word(&dst_op, dst);

        0
    }

    fn op_invalid(&mut self, _op_code: u8) -> usize {
        unreachable!()
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
        self.flags = Flags::empty();
        self.segment_override = None;
    }

    fn step(&mut self) -> Result<usize, ExecuteError> {
        self._print_state();
        self.execute();
        Ok(0)
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
        cpu.step().unwrap();
        assert_eq!(0x0103, cpu.registers[AX]);

        let data = &mut [0x00_u8, 0b00_001_110, 0x04, 0x00, 0x01];

        let mut cpu = Intel8088::new(&mut data[..], BusPrinter { name: "io" });
        cpu.registers[CX] = 0x0102;
        cpu.step().unwrap();
        assert_eq!(0x03, data[4]);
    }

    #[test]
    fn add_rm16_reg16() {
        use crate::Cpu;

        let data = &mut [0x01_u8, 0b11_001_000];

        let mut cpu = Intel8088::new(&mut data[..], BusPrinter { name: "io" });
        cpu.registers[AX] = 0x0101;
        cpu.registers[CX] = 0x0102;
        cpu.step().unwrap();
        assert_eq!(0x0203, cpu.registers[AX]);

        let data = &mut [0x01_u8, 0b00_001_110, 0x04, 0x00, 0x02, 0x01];

        let mut cpu = Intel8088::new(&mut data[..], BusPrinter { name: "io" });
        cpu.registers[CX] = 0x0102;
        cpu.step().unwrap();
        assert_eq!(0x04, data[4]);
        assert_eq!(0x02, data[5]);
    }
}
