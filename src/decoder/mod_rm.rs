use crate::decoder::{ByteReader, Error, Result};
use crate::instructions::{AddressingMode, Operand, Register};

impl AddressingMode {
    pub fn try_from_byte(byte: u8) -> Result<Self> {
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
    pub fn try_from(mod_rm_byte: u8, extra_bytes: &[u8]) -> Result<Self> {
        let mode = mod_rm_byte >> 6;
        let rm = mod_rm_byte & 0b111;

        match mode {
            0b00 => Ok(RegisterOrMemory::Indirect(AddressingMode::try_from_byte(
                rm,
            )?)),
            0b01 => Ok(RegisterOrMemory::DisplacementByte(
                AddressingMode::try_from_byte(rm)?,
                extra_bytes.read_u8()?,
            )),
            0b10 => Ok(RegisterOrMemory::DisplacementWord(
                AddressingMode::try_from_byte(rm)?,
                extra_bytes.read_u16()?,
            )),
            0b11 => Ok(RegisterOrMemory::Register(Register::try_from_low_bits(rm)?)),
            _ => Err(Error::InvalidModRMEncoding(mod_rm_byte)),
        }
    }
}

impl Into<Operand> for RegisterOrMemory {
    fn into(self) -> Operand {
        match self {
            RegisterOrMemory::Direct(offset) => Operand::Direct(offset),
            RegisterOrMemory::Indirect(encoding) => Operand::Indirect(encoding, 0),
            RegisterOrMemory::DisplacementByte(encoding, displacement) => {
                Operand::Indirect(encoding, displacement as u16)
            }
            RegisterOrMemory::DisplacementWord(encoding, displacement) => {
                Operand::Indirect(encoding, displacement)
            }
            RegisterOrMemory::Register(encoding) => Operand::Register(encoding),
        }
    }
}

impl From<Register> for Operand {
    fn from(register_encoding: Register) -> Self {
        Self::Register(register_encoding)
    }
}

#[derive(Debug)]
pub struct ModRM {
    pub register: Register,
    pub register_or_memory: RegisterOrMemory,
}

impl ModRM {
    pub fn try_from_mod_rm_byte(mod_rm_byte: u8, extra_bytes: &[u8]) -> Result<Self> {
        let reg = mod_rm_byte & 6;

        Ok(ModRM {
            register: Register::try_from_low_bits(reg)?,
            register_or_memory: RegisterOrMemory::try_from(mod_rm_byte, extra_bytes)?,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn indirect_memory() {
        assert_eq!(
            AddressingMode::try_from_byte(0b000).unwrap(),
            AddressingMode::BxSi
        );
        assert_eq!(
            AddressingMode::try_from_byte(0b001).unwrap(),
            AddressingMode::BxDi
        );
        assert_eq!(
            AddressingMode::try_from_byte(0b010).unwrap(),
            AddressingMode::BpSi
        );
        assert_eq!(
            AddressingMode::try_from_byte(0b011).unwrap(),
            AddressingMode::BpDi
        );
        assert_eq!(
            AddressingMode::try_from_byte(0b100).unwrap(),
            AddressingMode::Si
        );
        assert_eq!(
            AddressingMode::try_from_byte(0b101).unwrap(),
            AddressingMode::Di
        );
        assert_eq!(
            AddressingMode::try_from_byte(0b110).unwrap(),
            AddressingMode::Bp
        );
        assert_eq!(
            AddressingMode::try_from_byte(0b111).unwrap(),
            AddressingMode::Bx
        );

        if let Err(err) = AddressingMode::try_from_byte(77) {
            assert_eq!(err, Error::InvalidIndirectMemoryEncoding(77))
        } else {
            assert!(false, "does not return error");
        }
    }

    #[test]
    fn register_or_memory() {
        // Indirect
        assert_eq!(
            RegisterOrMemory::try_from(0b00_000_000, &[]).unwrap(),
            RegisterOrMemory::Indirect(AddressingMode::BxSi)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b00_000_001, &[]).unwrap(),
            RegisterOrMemory::Indirect(AddressingMode::BxDi)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b00_000_010, &[]).unwrap(),
            RegisterOrMemory::Indirect(AddressingMode::BpSi)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b00_000_011, &[]).unwrap(),
            RegisterOrMemory::Indirect(AddressingMode::BpDi)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b00_000_100, &[]).unwrap(),
            RegisterOrMemory::Indirect(AddressingMode::Si)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b00_000_101, &[]).unwrap(),
            RegisterOrMemory::Indirect(AddressingMode::Di)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b00_000_110, &[]).unwrap(),
            RegisterOrMemory::Indirect(AddressingMode::Bp)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b00_000_111, &[]).unwrap(),
            RegisterOrMemory::Indirect(AddressingMode::Bx)
        );

        // DisplacementByte
        assert_eq!(
            RegisterOrMemory::try_from(0b01_000_000, &[0x01]).unwrap(),
            RegisterOrMemory::DisplacementByte(AddressingMode::BxSi, 1)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b01_000_001, &[0x01]).unwrap(),
            RegisterOrMemory::DisplacementByte(AddressingMode::BxDi, 1)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b01_000_010, &[0x01]).unwrap(),
            RegisterOrMemory::DisplacementByte(AddressingMode::BpSi, 1)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b01_000_011, &[0x01]).unwrap(),
            RegisterOrMemory::DisplacementByte(AddressingMode::BpDi, 1)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b01_000_100, &[0x01]).unwrap(),
            RegisterOrMemory::DisplacementByte(AddressingMode::Si, 1)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b01_000_101, &[0x01]).unwrap(),
            RegisterOrMemory::DisplacementByte(AddressingMode::Di, 1)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b01_000_110, &[0x01]).unwrap(),
            RegisterOrMemory::DisplacementByte(AddressingMode::Bp, 1)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b01_000_111, &[0x01]).unwrap(),
            RegisterOrMemory::DisplacementByte(AddressingMode::Bx, 1)
        );

        // DisplacementWord
        assert_eq!(
            RegisterOrMemory::try_from(0b10_000_000, &[0x01, 0x01]).unwrap(),
            RegisterOrMemory::DisplacementWord(AddressingMode::BxSi, 257)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b10_000_001, &[0x01, 0x01]).unwrap(),
            RegisterOrMemory::DisplacementWord(AddressingMode::BxDi, 257)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b10_000_010, &[0x01, 0x01]).unwrap(),
            RegisterOrMemory::DisplacementWord(AddressingMode::BpSi, 257)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b10_000_011, &[0x01, 0x01]).unwrap(),
            RegisterOrMemory::DisplacementWord(AddressingMode::BpDi, 257)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b10_000_100, &[0x01, 0x01]).unwrap(),
            RegisterOrMemory::DisplacementWord(AddressingMode::Si, 257)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b10_000_101, &[0x01, 0x01]).unwrap(),
            RegisterOrMemory::DisplacementWord(AddressingMode::Di, 257)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b10_000_110, &[0x01, 0x01]).unwrap(),
            RegisterOrMemory::DisplacementWord(AddressingMode::Bp, 257)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b10_000_111, &[0x01, 0x01]).unwrap(),
            RegisterOrMemory::DisplacementWord(AddressingMode::Bx, 257)
        );

        // Register
        assert_eq!(
            RegisterOrMemory::try_from(0b11_000_000, &[]).unwrap(),
            RegisterOrMemory::Register(RegisterEncoding::AlAx)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b11_000_001, &[]).unwrap(),
            RegisterOrMemory::Register(RegisterEncoding::ClCx)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b11_000_010, &[]).unwrap(),
            RegisterOrMemory::Register(RegisterEncoding::DlDx)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b11_000_011, &[]).unwrap(),
            RegisterOrMemory::Register(RegisterEncoding::BlBx)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b11_000_100, &[]).unwrap(),
            RegisterOrMemory::Register(RegisterEncoding::AhSp)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b11_000_101, &[]).unwrap(),
            RegisterOrMemory::Register(RegisterEncoding::ChBp)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b11_000_110, &[]).unwrap(),
            RegisterOrMemory::Register(RegisterEncoding::DhSi)
        );
        assert_eq!(
            RegisterOrMemory::try_from(0b11_000_111, &[]).unwrap(),
            RegisterOrMemory::Register(RegisterEncoding::BhDi)
        );
    }
}
