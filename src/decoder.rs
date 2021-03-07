use crate::instruction::*;

type DecodeError = &'static str;

impl RegisterEncoding {
    pub fn try_from_byte(byte: u8) -> Result<Self, DecodeError> {
        match byte {
            0b000 => Ok(RegisterEncoding::AlAx),
            0b001 => Ok(RegisterEncoding::ClCx),
            0b010 => Ok(RegisterEncoding::DlDx),
            0b011 => Ok(RegisterEncoding::BlBx),
            0b100 => Ok(RegisterEncoding::AhSp),
            0b101 => Ok(RegisterEncoding::ChBp),
            0b110 => Ok(RegisterEncoding::DhSi),
            0b111 => Ok(RegisterEncoding::BhDi),
            _ => Err("invalid register encoding"),
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
            _ => Err("invalid indirect offset encoding"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ModRMEncoding {
    Indirect(IndirectMemoryEncoding),
    DisplacementByte(IndirectMemoryEncoding, u8),
    DisplacementWord(IndirectMemoryEncoding, u16),
    Register(RegisterEncoding),
}

impl ModRMEncoding {
    fn try_from_byte<Reader: std::io::Read>(
        byte: u8,
        extra_bytes: &mut Reader,
    ) -> Result<Self, DecodeError> {
        let mode = byte >> 6;
        match mode {
            0b00 => {
                let encoding = IndirectMemoryEncoding::try_from_byte(byte)?;
                Ok(ModRMEncoding::Indirect(encoding))
            }
            0b01 => {
                let encoding = IndirectMemoryEncoding::try_from_byte(byte)?;
                let mut buf = [0u8; 1];
                if extra_bytes.read_exact(&mut buf).is_ok() {
                    Ok(ModRMEncoding::DisplacementByte(encoding, buf[0]))
                } else {
                    Err("could not read extra byte")
                }
            }
            0b10 => {
                let encoding = IndirectMemoryEncoding::try_from_byte(byte)?;
                let mut buf = [0u8; 2];
                if extra_bytes.read_exact(&mut buf).is_ok() {
                    let displacement = (buf[0] as u16 + (buf[1] as u16)) << 8;
                    Ok(ModRMEncoding::DisplacementWord(encoding, displacement))
                } else {
                    Err("could not read extra word")
                }
            }
            0b11 => {
                let encoding = RegisterEncoding::try_from_byte(byte)?;
                Ok(ModRMEncoding::Register(encoding))
            }
            _ => Err("invalid modr/m encoding"),
        }
    }
}

struct ModRM(ModRMEncoding, RegisterEncoding);

impl ModRM {
    fn try_from_mod_rm_byte<Reader: std::io::Read>(
        mod_rm_byte: u8,
        extra_bytes: &mut Reader,
    ) -> Result<Self, DecodeError> {
        // let mode = mod_rm_byte >> 6;
        let rm = mod_rm_byte >> 3 & 6;
        let reg = mod_rm_byte & 6;
        if let Ok(encoding) = ModRMEncoding::try_from_byte(rm, extra_bytes) {
            if let Ok(register) = RegisterEncoding::try_from_byte(reg) {
                Ok(ModRM(encoding, register))
            } else {
                Err("invalid register encoding")
            }
        } else {
            Err("invalid modr/m encoding")
        }
    }
}

impl Operand {
    fn from_mod_rm_encoding(encoding: ModRMEncoding) -> Result<Self, DecodeError> {
        match encoding {
            ModRMEncoding::Indirect(encoding) => Ok(Operand::Indirect(encoding, 0)),
            ModRMEncoding::Register(register_encoding) => Ok(Operand::Register(register_encoding)),
            _ => Err("could not create operand from mod_rm encoding"),
        }
    }
}

fn decode_with_mod_rm(data: &[u8]) -> Result<Instruction, DecodeError> {
    let ModRM(encoding, register_encoding) =
        ModRM::try_from_mod_rm_byte(data[0], &mut data.as_ref())?;

    let source = Operand::from_mod_rm_encoding(encoding)?;
    let destination = Operand::Register(register_encoding);

    Ok(Instruction::new(
        Operation::Add,
        DataSize::Byte,
        destination,
        source,
    ))
}

pub fn decode_instruction(data: &[u8]) -> Result<Instruction, DecodeError> {
    let op_code = data[0];

    println!("op_code = {}", op_code);

    match op_code {
        0 => decode_with_mod_rm(data.split_at(1).1),
        _ => Err("invalid op code"),
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
            Err("invalid register encoding")
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
            Err("invalid indirect offset encoding")
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
