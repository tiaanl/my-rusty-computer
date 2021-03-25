use super::DecodeError;
use crate::instructions::{AddressingMode, Operand, RegisterEncoding};

impl AddressingMode {
    pub fn try_from_byte(byte: u8) -> Result<Self, DecodeError> {
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
            _ => Err(DecodeError::InvalidIndirectMemoryEncoding(byte)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum RegisterOrMemory {
    Indirect(AddressingMode),
    DisplacementByte(AddressingMode, u8),
    DisplacementWord(AddressingMode, u16),
    Register(RegisterEncoding),
}

impl RegisterOrMemory {
    pub fn try_from(mod_rm_byte: u8, extra_bytes: &[u8]) -> Result<Self, DecodeError> {
        let mode = mod_rm_byte >> 6;
        let rm = mod_rm_byte & 7;

        match mode {
            0b00 => Ok(RegisterOrMemory::Indirect(AddressingMode::try_from_byte(
                rm,
            )?)),
            0b01 => Ok(RegisterOrMemory::DisplacementByte(
                AddressingMode::try_from_byte(rm)?,
                extra_bytes[0],
            )),
            0b10 => Ok(RegisterOrMemory::DisplacementWord(
                AddressingMode::try_from_byte(rm)?,
                ((extra_bytes[0] as u16) << 8) + (extra_bytes[1] as u16),
            )),
            0b11 => Ok(RegisterOrMemory::Register(RegisterEncoding::try_from_byte(
                rm,
            )?)),
            _ => Err(DecodeError::InvalidModRMEncoding(mod_rm_byte)),
        }
    }
}

impl Into<Operand> for RegisterOrMemory {
    fn into(self) -> Operand {
        match self {
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

#[derive(Debug)]
pub struct ModRM(pub RegisterOrMemory, pub RegisterEncoding);

impl ModRM {
    pub fn try_from_bytes(mod_rm_byte: u8, extra_bytes: &[u8]) -> Result<Self, DecodeError> {
        let rm = (mod_rm_byte >> 3) & 6;
        let reg = mod_rm_byte & 6;

        if let Ok(memory_or_register) = RegisterOrMemory::try_from(mod_rm_byte, extra_bytes) {
            if let Ok(register) = RegisterEncoding::try_from_byte(reg) {
                Ok(ModRM(memory_or_register, register))
            } else {
                Err(DecodeError::InvalidRegisterEncoding(reg))
            }
        } else {
            Err(DecodeError::InvalidModRMEncoding(rm))
        }
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
            assert_eq!(err, DecodeError::InvalidIndirectMemoryEncoding(77))
        } else {
            assert!(false, "does not return error");
        }
    }

    #[test]
    fn mod_rm_encoding() {
        assert_eq!(
            RegisterOrMemory::try_from(&[0b00000000]).unwrap(),
            RegisterOrMemory::Indirect(AddressingMode::BxSi)
        );
        assert_eq!(
            RegisterOrMemory::try_from(&[0b01000001, 1]).unwrap(),
            RegisterOrMemory::DisplacementByte(AddressingMode::BxDi, 1)
        );
        assert_eq!(
            RegisterOrMemory::try_from(&[0b10000010, 2, 1]).unwrap(),
            RegisterOrMemory::DisplacementWord(AddressingMode::BpSi, 513)
        );
        assert_eq!(
            RegisterOrMemory::try_from(&[0b11000011, 2, 1]).unwrap(),
            RegisterOrMemory::Register(RegisterEncoding::BlBx)
        );
    }
}
