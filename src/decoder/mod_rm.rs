use super::DecodeError;
use crate::instructions::{IndirectMemoryEncoding, RegisterEncoding};

impl IndirectMemoryEncoding {
    pub fn try_from_byte(byte: u8) -> Result<Self, DecodeError> {
        if byte >= 8 {
            Err(DecodeError::InvalidIndirectMemoryOffset(byte))
        } else {
            Ok(unsafe { std::mem::transmute(byte as i8) })
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ModRMEncoding {
    Indirect(IndirectMemoryEncoding),
    DisplacementByte(IndirectMemoryEncoding, u8),
    DisplacementWord(IndirectMemoryEncoding, u16),
    Register(RegisterEncoding),
}

impl ModRMEncoding {
    pub fn try_from_mod_rm_byte(mod_rm_byte: u8, extra_bytes: &[u8]) -> Result<Self, DecodeError> {
        let mode = mod_rm_byte >> 6;
        let rm = mod_rm_byte & 7;

        match mode {
            0b00 => Ok(ModRMEncoding::Indirect(
                IndirectMemoryEncoding::try_from_byte(rm)?,
            )),
            0b01 => Ok(ModRMEncoding::DisplacementByte(
                IndirectMemoryEncoding::try_from_byte(rm)?,
                extra_bytes[0],
            )),
            0b10 => Ok(ModRMEncoding::DisplacementWord(
                IndirectMemoryEncoding::try_from_byte(rm)?,
                ((extra_bytes[0] as u16) << 8) + (extra_bytes[1] as u16),
            )),
            0b11 => Ok(ModRMEncoding::Register(RegisterEncoding::try_from_byte(
                rm,
            )?)),
            _ => Err(DecodeError::InvalidModRMEncoding(mod_rm_byte)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn indirect_memory_encoding() {
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(0b00000000).unwrap(),
            IndirectMemoryEncoding::BxSi
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(0b00000001).unwrap(),
            IndirectMemoryEncoding::BxDi
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(0b00000010).unwrap(),
            IndirectMemoryEncoding::BpSi
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(0b00000011).unwrap(),
            IndirectMemoryEncoding::BpDi
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(0b00000100).unwrap(),
            IndirectMemoryEncoding::Si
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(0b00000101).unwrap(),
            IndirectMemoryEncoding::Di
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(0b00000110).unwrap(),
            IndirectMemoryEncoding::Bp
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(0b00000111).unwrap(),
            IndirectMemoryEncoding::Bx
        );
        assert!(IndirectMemoryEncoding::try_from_byte(0b01001000).is_err());
    }

    #[test]
    fn mod_rm_encoding() {
        assert_eq!(
            ModRMEncoding::try_from_mod_rm_byte(0b00000000, &[]).unwrap(),
            ModRMEncoding::Indirect(IndirectMemoryEncoding::BxSi)
        );
        assert_eq!(
            ModRMEncoding::try_from_mod_rm_byte(0b01000001, &[1]).unwrap(),
            ModRMEncoding::DisplacementByte(IndirectMemoryEncoding::BxDi, 1)
        );
        assert_eq!(
            ModRMEncoding::try_from_mod_rm_byte(0b10000010, &[2, 1]).unwrap(),
            ModRMEncoding::DisplacementWord(IndirectMemoryEncoding::BpSi, 513)
        );
        assert_eq!(
            ModRMEncoding::try_from_mod_rm_byte(0b11000011, &[2, 1]).unwrap(),
            ModRMEncoding::Register(RegisterEncoding::BlBx)
        );
    }
}
