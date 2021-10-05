use crate::errors::Result;
use crate::{it_read_byte, it_read_word, Error, LowBitsDecoder};
use mrc_x86::{AddressingMode, Displacement, OperandType, Register, Segment};

impl LowBitsDecoder<Self> for AddressingMode {
    fn try_from_low_bits(byte: u8) -> Result<Self> {
        use AddressingMode::*;

        match byte {
            0b000 => Ok(BxSi),
            0b001 => Ok(BxDi),
            0b010 => Ok(BpSi),
            0b011 => Ok(BpDi),
            0b100 => Ok(Si),
            0b101 => Ok(Di),
            0b110 => Ok(Bp),
            0b111 => Ok(Bx),
            _ => Err(Error::InvalidIndirectMemoryEncoding(byte)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum RegisterOrMemory {
    Direct(u16),
    Indirect(AddressingMode),
    DisplacementByte(AddressingMode, u8),
    DisplacementWord(AddressingMode, u16),
    Register(Register),
}

impl RegisterOrMemory {
    pub fn try_from_modrm<It: Iterator<Item = u8>>(mod_rm_byte: u8, it: &mut It) -> Result<Self> {
        let mode = mod_rm_byte >> 6;
        let rm = mod_rm_byte & 0b111;

        match mode {
            0b00 => match rm {
                0b110 => Ok(RegisterOrMemory::Direct(it_read_word(it).unwrap())),
                _ => Ok(RegisterOrMemory::Indirect(
                    AddressingMode::try_from_low_bits(rm)?,
                )),
            },
            0b01 => Ok(RegisterOrMemory::DisplacementByte(
                AddressingMode::try_from_low_bits(rm)?,
                it_read_byte(it).unwrap(),
            )),
            0b10 => Ok(RegisterOrMemory::DisplacementWord(
                AddressingMode::try_from_low_bits(rm)?,
                it_read_word(it).unwrap(),
            )),
            0b11 => Ok(RegisterOrMemory::Register(Register::try_from_low_bits(rm)?)),
            _ => Err(Error::InvalidModRmEncoding(mod_rm_byte)),
        }
    }
}

impl From<RegisterOrMemory> for OperandType {
    fn from(register_or_memory: RegisterOrMemory) -> Self {
        match register_or_memory {
            RegisterOrMemory::Direct(offset) => OperandType::Direct(Segment::Ds, offset),
            RegisterOrMemory::Indirect(encoding) => {
                OperandType::Indirect(Segment::Ds, encoding, Displacement::None)
            }
            RegisterOrMemory::DisplacementByte(encoding, displacement) => OperandType::Indirect(
                Segment::Ds,
                encoding,
                Displacement::Byte(displacement as i8),
            ),
            RegisterOrMemory::DisplacementWord(encoding, displacement) => OperandType::Indirect(
                Segment::Ds,
                encoding,
                Displacement::Word(displacement as i16),
            ),
            RegisterOrMemory::Register(encoding) => OperandType::Register(encoding),
        }
    }
}

fn encoding_for_register(register: Register) -> u8 {
    use Register::*;

    match register {
        AlAx => 0b000,
        ClCx => 0b001,
        DlDx => 0b010,
        BlBx => 0b011,
        AhSp => 0b100,
        ChBp => 0b101,
        DhSi => 0b110,
        BhDi => 0b111,
    }
}

fn encoding_for_addressing_mode(addressing_mode: AddressingMode) -> u8 {
    use AddressingMode::*;

    match addressing_mode {
        BxSi => 0b000,
        BxDi => 0b001,
        BpSi => 0b010,
        BpDi => 0b011,
        Si => 0b100,
        Di => 0b101,
        Bp => 0b110,
        Bx => 0b111,
    }
}

#[derive(Debug)]
pub struct Modrm {
    pub register: Register,
    pub register_or_memory: RegisterOrMemory,
}

impl Modrm {
    pub fn new(register: Register, register_or_memory: RegisterOrMemory) -> Self {
        Self {
            register,
            register_or_memory,
        }
    }

    pub fn try_from_byte<It: Iterator<Item = u8>>(mod_rm_byte: u8, it: &mut It) -> Result<Self> {
        let register = Register::try_from_low_bits(mod_rm_byte >> 3 & 0b111)?;

        let register_or_memory = RegisterOrMemory::try_from_modrm(mod_rm_byte, it)?;

        Ok(Modrm {
            register,
            register_or_memory,
        })
    }

    pub fn try_from_iter<It: Iterator<Item = u8>>(it: &mut It) -> Result<Self> {
        let modrm_byte = match it.next() {
            Some(byte) => byte,
            None => return Err(Error::CouldNotReadExtraBytes),
        };

        Self::try_from_byte(modrm_byte, it)
    }

    pub fn as_byte(&self) -> u8 {
        let mut byte: u8 = match self.register_or_memory {
            RegisterOrMemory::Direct(_) => 0b00,
            RegisterOrMemory::Indirect(_) => 0b00,
            RegisterOrMemory::DisplacementByte(_, _) => 0b01,
            RegisterOrMemory::DisplacementWord(_, _) => 0b10,
            RegisterOrMemory::Register(_) => 0b11,
        } << 6;

        byte |= encoding_for_register(self.register) << 3;

        byte |= match self.register_or_memory {
            RegisterOrMemory::Direct(_) => 0b110,
            RegisterOrMemory::Indirect(addressing_mode) => {
                encoding_for_addressing_mode(addressing_mode)
            }
            RegisterOrMemory::DisplacementByte(addressing_mode, _) => {
                encoding_for_addressing_mode(addressing_mode)
            }
            RegisterOrMemory::DisplacementWord(addressing_mode, _) => {
                encoding_for_addressing_mode(addressing_mode)
            }
            RegisterOrMemory::Register(register) => encoding_for_register(register),
        };

        byte
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn indirect_memory() {
        assert_eq!(
            AddressingMode::try_from_low_bits(0b000).unwrap(),
            AddressingMode::BxSi
        );
        assert_eq!(
            AddressingMode::try_from_low_bits(0b001).unwrap(),
            AddressingMode::BxDi
        );
        assert_eq!(
            AddressingMode::try_from_low_bits(0b010).unwrap(),
            AddressingMode::BpSi
        );
        assert_eq!(
            AddressingMode::try_from_low_bits(0b011).unwrap(),
            AddressingMode::BpDi
        );
        assert_eq!(
            AddressingMode::try_from_low_bits(0b100).unwrap(),
            AddressingMode::Si
        );
        assert_eq!(
            AddressingMode::try_from_low_bits(0b101).unwrap(),
            AddressingMode::Di
        );
        assert_eq!(
            AddressingMode::try_from_low_bits(0b110).unwrap(),
            AddressingMode::Bp
        );
        assert_eq!(
            AddressingMode::try_from_low_bits(0b111).unwrap(),
            AddressingMode::Bx
        );

        if let Err(err) = AddressingMode::try_from_low_bits(77) {
            assert_eq!(err, Error::InvalidIndirectMemoryEncoding(77))
        } else {
            assert!(false, "does not return error");
        }
    }

    /*
    #[test]
    fn register_or_memory() {
        // Indirect
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b00_000_000, &[]).unwrap(),
            RegisterOrMemory::Indirect(AddressingMode::BxSi),
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b00_000_001, &[]).unwrap(),
            (RegisterOrMemory::Indirect(AddressingMode::BxDi), 0)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b00_000_010, &[]).unwrap(),
            (RegisterOrMemory::Indirect(AddressingMode::BpSi), 0)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b00_000_011, &[]).unwrap(),
            (RegisterOrMemory::Indirect(AddressingMode::BpDi), 0)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b00_000_100, &[]).unwrap(),
            (RegisterOrMemory::Indirect(AddressingMode::Si), 0)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b00_000_101, &[]).unwrap(),
            (RegisterOrMemory::Indirect(AddressingMode::Di), 0)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b00_000_110, &[0x01, 0x01]).unwrap(),
            (RegisterOrMemory::Direct(0x0101), 2)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b00_000_111, &[]).unwrap(),
            (RegisterOrMemory::Indirect(AddressingMode::Bx), 0)
        );

        // DisplacementByte
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b01_000_000, &[0x01]).unwrap(),
            (
                RegisterOrMemory::DisplacementByte(AddressingMode::BxSi, 1),
                1
            )
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b01_000_001, &[0x01]).unwrap(),
            (
                RegisterOrMemory::DisplacementByte(AddressingMode::BxDi, 1),
                1
            )
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b01_000_010, &[0x01]).unwrap(),
            (
                RegisterOrMemory::DisplacementByte(AddressingMode::BpSi, 1),
                1
            )
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b01_000_011, &[0x01]).unwrap(),
            (
                RegisterOrMemory::DisplacementByte(AddressingMode::BpDi, 1),
                1
            )
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b01_000_100, &[0x01]).unwrap(),
            (RegisterOrMemory::DisplacementByte(AddressingMode::Si, 1), 1)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b01_000_101, &[0x01]).unwrap(),
            (RegisterOrMemory::DisplacementByte(AddressingMode::Di, 1), 1)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b01_000_110, &[0x01]).unwrap(),
            (RegisterOrMemory::DisplacementByte(AddressingMode::Bp, 1), 1)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b01_000_111, &[0x01]).unwrap(),
            (RegisterOrMemory::DisplacementByte(AddressingMode::Bx, 1), 1)
        );

        // DisplacementWord
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b10_000_000, &[0x01, 0x01]).unwrap(),
            (
                RegisterOrMemory::DisplacementWord(AddressingMode::BxSi, 257),
                2
            )
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b10_000_001, &[0x01, 0x01]).unwrap(),
            (
                RegisterOrMemory::DisplacementWord(AddressingMode::BxDi, 257),
                2
            )
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b10_000_010, &[0x01, 0x01]).unwrap(),
            (
                RegisterOrMemory::DisplacementWord(AddressingMode::BpSi, 257),
                2
            )
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b10_000_011, &[0x01, 0x01]).unwrap(),
            (
                RegisterOrMemory::DisplacementWord(AddressingMode::BpDi, 257),
                2
            )
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b10_000_100, &[0x01, 0x01]).unwrap(),
            (
                RegisterOrMemory::DisplacementWord(AddressingMode::Si, 257),
                2
            )
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b10_000_101, &[0x01, 0x01]).unwrap(),
            (
                RegisterOrMemory::DisplacementWord(AddressingMode::Di, 257),
                2
            )
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b10_000_110, &[0x01, 0x01]).unwrap(),
            (
                RegisterOrMemory::DisplacementWord(AddressingMode::Bp, 257),
                2
            )
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b10_000_111, &[0x01, 0x01]).unwrap(),
            (
                RegisterOrMemory::DisplacementWord(AddressingMode::Bx, 257),
                2
            )
        );

        // Register
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b11_000_000, &[]).unwrap(),
            (RegisterOrMemory::Register(Register::AlAx), 0)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b11_000_001, &[]).unwrap(),
            (RegisterOrMemory::Register(Register::ClCx), 0)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b11_000_010, &[]).unwrap(),
            (RegisterOrMemory::Register(Register::DlDx), 0)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b11_000_011, &[]).unwrap(),
            (RegisterOrMemory::Register(Register::BlBx), 0)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b11_000_100, &[]).unwrap(),
            (RegisterOrMemory::Register(Register::AhSp), 0)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b11_000_101, &[]).unwrap(),
            (RegisterOrMemory::Register(Register::ChBp), 0)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b11_000_110, &[]).unwrap(),
            (RegisterOrMemory::Register(Register::DhSi), 0)
        );
        assert_eq!(
            RegisterOrMemory::try_from_modrm(0b11_000_111, &[]).unwrap(),
            (RegisterOrMemory::Register(Register::BhDi), 0)
        );
    }
    */

    macro_rules! test_modrm_to_byte {
        ($expected:expr,$register:expr,$register_or_memory:expr) => {{
            let byte: u8 = Modrm::new($register, $register_or_memory).as_byte();
            assert_eq!($expected, byte);
        }};
    }

    #[test]
    fn modrm_to_byte_register_indirect() {
        test_modrm_to_byte!(
            0b00011001,
            Register::BlBx,
            RegisterOrMemory::Indirect(AddressingMode::BxDi)
        );
    }

    #[test]
    fn modrm_to_byte_register_displacement_byte() {
        test_modrm_to_byte!(
            0b01011001,
            Register::BlBx,
            RegisterOrMemory::DisplacementByte(AddressingMode::BxDi, 0)
        );
    }

    #[test]
    fn modrm_to_byte_register_displacement_word() {
        test_modrm_to_byte!(
            0b10011001,
            Register::BlBx,
            RegisterOrMemory::DisplacementWord(AddressingMode::BxDi, 0)
        );
    }

    #[test]
    fn modrm_to_byte_register_register() {
        test_modrm_to_byte!(
            0b11011110,
            Register::BlBx,
            RegisterOrMemory::Register(Register::DhSi)
        );
    }

    #[test]
    fn modrm_to_byte_register_direct() {
        test_modrm_to_byte!(0b00010110, Register::DlDx, RegisterOrMemory::Direct(0));
    }
}
