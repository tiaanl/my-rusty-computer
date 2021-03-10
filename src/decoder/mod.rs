use std::fmt;

use crate::instruction::*;

#[derive(PartialEq, Debug)]
pub enum DecodeError {
    InvalidOpCode(u8),
    InvalidRegisterEncoding(u8),
    InvalidIndirectMemoryOffset(u8),
    InvalidModRMEncoding(u8),
    InvalidModRMMode(u8),
    CouldNotCreateOperandFromModRMEncoding(ModRMEncoding),
    CouldNotReadExtraBytes,
}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            DecodeError::InvalidOpCode(op_code) => write!(f, "invalid op code: {:#04x}", op_code),
            _ => write!(f, "unknown error"),
        }
    }
}

impl RegisterEncoding {
    fn try_from_byte(byte: u8) -> Result<Self, DecodeError> {
        match byte {
            0b000 => Ok(RegisterEncoding::AlAx),
            0b001 => Ok(RegisterEncoding::ClCx),
            0b010 => Ok(RegisterEncoding::DlDx),
            0b011 => Ok(RegisterEncoding::BlBx),
            0b100 => Ok(RegisterEncoding::AhSp),
            0b101 => Ok(RegisterEncoding::ChBp),
            0b110 => Ok(RegisterEncoding::DhSi),
            0b111 => Ok(RegisterEncoding::BhDi),
            _ => Err(DecodeError::InvalidRegisterEncoding(byte)),
        }
    }
}

impl IndirectMemoryEncoding {
    fn try_from_byte(byte: u8) -> Result<Self, DecodeError> {
        match byte {
            0b000 => Ok(IndirectMemoryEncoding::BxSi),
            0b001 => Ok(IndirectMemoryEncoding::BxDi),
            0b010 => Ok(IndirectMemoryEncoding::BpSi),
            0b011 => Ok(IndirectMemoryEncoding::BpDi),
            0b100 => Ok(IndirectMemoryEncoding::Si),
            0b101 => Ok(IndirectMemoryEncoding::Di),
            0b110 => Ok(IndirectMemoryEncoding::Bp),
            0b111 => Ok(IndirectMemoryEncoding::Bx),
            _ => Err(DecodeError::InvalidIndirectMemoryOffset(byte)),
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

trait ByteReader {
    fn read_u8(&self) -> Result<u8, DecodeError>;
    fn read_u16(&self) -> Result<u16, DecodeError>;
}

impl ByteReader for &[u8] {
    fn read_u8(&self) -> Result<u8, DecodeError> {
        if self.len() >= 1 {
            Ok(self[0])
        } else {
            Err(DecodeError::CouldNotReadExtraBytes)
        }
    }

    fn read_u16(&self) -> Result<u16, DecodeError> {
        if self.len() >= 2 {
            Ok(((self[0] as u16) << 8) + self[1] as u16)
        } else {
            Err(DecodeError::CouldNotReadExtraBytes)
        }
    }
}

impl ModRMEncoding {
    fn try_from_byte(byte: u8, extra_bytes: &[u8]) -> Result<Self, DecodeError> {
        let mode = byte >> 6;
        match mode {
            0b00 => {
                let encoding = IndirectMemoryEncoding::try_from_byte(byte)?;
                Ok(ModRMEncoding::Indirect(encoding))
            }
            0b01 => {
                let encoding = IndirectMemoryEncoding::try_from_byte(byte)?;
                if let Ok(displacement) = extra_bytes.read_u8() {
                    Ok(ModRMEncoding::DisplacementByte(encoding, displacement))
                } else {
                    Err(DecodeError::CouldNotReadExtraBytes)
                }
            }
            0b10 => {
                let encoding = IndirectMemoryEncoding::try_from_byte(byte)?;
                if let Ok(displacement) = extra_bytes.read_u16() {
                    Ok(ModRMEncoding::DisplacementWord(encoding, displacement))
                } else {
                    Err(DecodeError::CouldNotReadExtraBytes)
                }
            }
            0b11 => {
                let encoding = RegisterEncoding::try_from_byte(byte)?;
                Ok(ModRMEncoding::Register(encoding))
            }
            _ => Err(DecodeError::InvalidModRMMode(mode)),
        }
    }
}

#[derive(Debug)]
struct ModRM(ModRMEncoding, RegisterEncoding);

impl ModRM {
    fn try_from_mod_rm_byte(mod_rm_byte: u8, extra_bytes: &[u8]) -> Result<Self, DecodeError> {
        // let mode = mod_rm_byte >> 6;
        let rm = mod_rm_byte >> 3 & 6;
        let reg = mod_rm_byte & 6;
        if let Ok(encoding) = ModRMEncoding::try_from_byte(rm, extra_bytes) {
            if let Ok(register) = RegisterEncoding::try_from_byte(reg) {
                Ok(ModRM(encoding, register))
            } else {
                Err(DecodeError::InvalidRegisterEncoding(reg))
            }
        } else {
            Err(DecodeError::InvalidModRMEncoding(rm))
        }
    }
}

impl Operand {
    fn from_mod_rm_encoding(encoding: ModRMEncoding) -> Result<Self, DecodeError> {
        match encoding {
            ModRMEncoding::Indirect(encoding) => Ok(Operand::Indirect(encoding, 0)),
            ModRMEncoding::Register(register_encoding) => Ok(Operand::Register(register_encoding)),
            _ => Err(DecodeError::CouldNotCreateOperandFromModRMEncoding(
                encoding,
            )),
        }
    }
}

fn operands_from_mod_rm(mod_rm: u8, extra_bytes: &[u8]) -> Result<(Operand, Operand), DecodeError> {
    let ModRM(encoding, register_encoding) = ModRM::try_from_mod_rm_byte(mod_rm, extra_bytes)?;

    Ok((
        Operand::from_mod_rm_encoding(encoding)?,
        Operand::Register(register_encoding),
    ))
}

trait CodeAndExtra<'a> {
    fn code_and_extra(&'a self) -> (u8, &'a [u8]);
}

impl<'a> CodeAndExtra<'a> for &[u8] {
    fn code_and_extra(&'a self) -> (u8, &'a [u8]) {
        let (code, extra) = self.split_at(1);
        (code[0], extra)
    }
}

pub fn decode_instruction(data: &[u8]) -> Result<Instruction, DecodeError> {
    let (op_code, extra_bytes) = data.code_and_extra();

    println!("op_code = {}", op_code);

    match op_code {
        // ADD / Add

        // 0 0 0 0 0 0 d w - Reg/Memory with Register to Either
        0b00000000 => {
            let (mod_rm, extra_bytes) = data.code_and_extra();
            let (destination, source) = operands_from_mod_rm(mod_rm, extra_bytes)?;
            Ok(Instruction::new(
                Operation::Add,
                DataSize::Byte,
                destination,
                source,
            ))
        }
        0b00000001 => {
            let (mod_rm, extra_bytes) = data.code_and_extra();
            let (destination, source) = operands_from_mod_rm(mod_rm, extra_bytes)?;
            Ok(Instruction::new(
                Operation::Add,
                DataSize::Word,
                destination,
                source,
            ))
        }
        0b00000010 => {
            let (mod_rm, extra_bytes) = data.code_and_extra();
            let (destination, source) = operands_from_mod_rm(mod_rm, extra_bytes)?;
            Ok(Instruction::new(
                Operation::Add,
                DataSize::Byte,
                destination,
                source,
            ))
        }
        0b00000011 => {
            let (mod_rm, extra_bytes) = data.code_and_extra();
            let (destination, source) = operands_from_mod_rm(mod_rm, extra_bytes)?;
            Ok(Instruction::new(
                Operation::Add,
                DataSize::Word,
                destination,
                source,
            ))
        }

        // 1 0 0 0 0 0 s w - Immediate to Register/Memory
        0b10000000 => {
            let (mod_rm_byte, extra_bytes) = extra_bytes.code_and_extra();
            let ModRM(encoding, _) = ModRM::try_from_mod_rm_byte(mod_rm_byte, extra_bytes)?;
            let immediate = extra_bytes.read_u8()?;
            Ok(Instruction::new(
                Operation::Add,
                DataSize::Byte,
                Operand::from_mod_rm_encoding(encoding)?,
                Operand::Immediate(immediate as u16),
            ))
        }
        /*
        0b10000001 => {
            let (mod_rm, mut extra_bytes) = extra_bytes.code_and_extra();
            let mod_rm = ModRM::try_from_mod_rm_byte(mod_rm, &mut extra_bytes)?;
            println!("{:?}", mod_rm);
            Err(DecodeError::Unknown)
        }
        0b10000010 => {
            let mod_rm = ModRM::try_from_mod_rm_byte(data[1], &mut data.as_ref())?;
            println!("{:?}", mod_rm);
            Err(DecodeError::Unknown)
        }
        0b10000011 => {
            let mod_rm = ModRM::try_from_mod_rm_byte(data[1], &mut data.as_ref())?;
            println!("{:?}", mod_rm);
            Err(DecodeError::Unknown)
        }
        */
        _ => Err(DecodeError::InvalidOpCode(op_code)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_encoding_from_byte() {
        assert_eq!(
            RegisterEncoding::try_from_byte(0).unwrap(),
            RegisterEncoding::AlAx
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(1).unwrap(),
            RegisterEncoding::ClCx
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(2).unwrap(),
            RegisterEncoding::DlDx
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(3).unwrap(),
            RegisterEncoding::BlBx
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(4).unwrap(),
            RegisterEncoding::AhSp
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(5).unwrap(),
            RegisterEncoding::ChBp
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(6).unwrap(),
            RegisterEncoding::DhSi
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(7).unwrap(),
            RegisterEncoding::BhDi
        );
        assert_eq!(
            RegisterEncoding::try_from_byte(8),
            Err(DecodeError::InvalidRegisterEncoding(8))
        );
    }

    #[test]
    fn indirect_memory_encoding() {
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(0).unwrap(),
            IndirectMemoryEncoding::BxSi
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(1).unwrap(),
            IndirectMemoryEncoding::BxDi
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(2).unwrap(),
            IndirectMemoryEncoding::BpSi
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(3).unwrap(),
            IndirectMemoryEncoding::BpDi
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(4).unwrap(),
            IndirectMemoryEncoding::Si
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(5).unwrap(),
            IndirectMemoryEncoding::Di
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(6).unwrap(),
            IndirectMemoryEncoding::Bp
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(7).unwrap(),
            IndirectMemoryEncoding::Bx
        );
        assert_eq!(
            IndirectMemoryEncoding::try_from_byte(8),
            Err(DecodeError::InvalidIndirectMemoryOffset(8))
        );
    }

    #[test]
    fn mod_rm_encoding() {
        assert_eq!(
            ModRMEncoding::try_from_byte(0, &mut [0u8; 1].as_ref()).unwrap(),
            ModRMEncoding::Indirect(IndirectMemoryEncoding::BxSi)
        );
        assert_eq!(
            ModRMEncoding::try_from_byte(1, &mut [0u8; 1].as_ref()).unwrap(),
            ModRMEncoding::Indirect(IndirectMemoryEncoding::BxDi)
        );
        assert_eq!(
            ModRMEncoding::try_from_byte(1, &mut [0u8; 1].as_ref()).unwrap(),
            ModRMEncoding::Indirect(IndirectMemoryEncoding::BxDi)
        );
    }
}
