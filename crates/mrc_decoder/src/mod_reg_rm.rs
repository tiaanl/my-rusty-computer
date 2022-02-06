use crate::errors::Result;
use crate::{it_read_byte, it_read_word, DecodeError, TryFromByte};
use mrc_instruction::{AddressingMode, Displacement, Operand, OperandSize, Register, Segment};

impl TryFromByte<Self> for AddressingMode {
    fn try_from_byte(byte: u8) -> Result<Self> {
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
    Direct(u16),
    Indirect(AddressingMode),
    DisplacementByte(AddressingMode, i8),
    DisplacementWord(AddressingMode, i16),
    Register(Register),
}

impl RegisterOrMemory {
    pub fn try_from_modrm(mod_rm_byte: u8, it: &mut impl Iterator<Item = u8>) -> Result<Self> {
        let mode = mod_rm_byte >> 6;
        let rm = mod_rm_byte & 0b111;

        match mode {
            0b00 => match rm {
                0b110 => Ok(RegisterOrMemory::Direct(it_read_word(it)?)),
                _ => Ok(RegisterOrMemory::Indirect(AddressingMode::try_from_byte(
                    rm,
                )?)),
            },

            0b01 => Ok(RegisterOrMemory::DisplacementByte(
                AddressingMode::try_from_byte(rm)?,
                it_read_byte(it)? as i8,
            )),

            0b10 => Ok(RegisterOrMemory::DisplacementWord(
                AddressingMode::try_from_byte(rm)?,
                it_read_word(it)? as i16,
            )),

            0b11 => Ok(RegisterOrMemory::Register(Register::try_from_byte(rm)?)),

            _ => Err(DecodeError::InvalidModRmEncoding(mod_rm_byte)),
        }
    }

    pub fn into_operand_kind(self, operand_size: OperandSize) -> Operand {
        match self {
            RegisterOrMemory::Direct(offset) => Operand::Direct(Segment::DS, offset, operand_size),
            RegisterOrMemory::Indirect(addressing_mode) => Operand::Indirect(
                Segment::DS,
                addressing_mode,
                Displacement::None,
                operand_size,
            ),
            RegisterOrMemory::DisplacementByte(addressing_mode, displacement) => Operand::Indirect(
                Segment::DS,
                addressing_mode,
                Displacement::Byte(displacement),
                operand_size,
            ),
            RegisterOrMemory::DisplacementWord(addressing_mode, displacement) => Operand::Indirect(
                Segment::DS,
                addressing_mode,
                Displacement::Word(displacement),
                operand_size,
            ),
            RegisterOrMemory::Register(register) => Operand::Register(register, operand_size),
        }
    }
}

// impl From<RegisterOrMemory> for OperandKind {
//     fn from(register_or_memory: RegisterOrMemory) -> Self {
//         match register_or_memory {
//             RegisterOrMemory::Direct(offset) => OperandKind::Direct(Segment::DS, offset),
//             RegisterOrMemory::Indirect(encoding) => {
//                 OperandKind::Indirect(Segment::DS, encoding, Displacement::None)
//             }
//             RegisterOrMemory::DisplacementByte(encoding, displacement) => OperandKind::Indirect(
//                 Segment::DS,
//                 encoding,
//                 Displacement::Byte(displacement as i8),
//             ),
//             RegisterOrMemory::DisplacementWord(encoding, displacement) => OperandKind::Indirect(
//                 Segment::DS,
//                 encoding,
//                 Displacement::Word(displacement as i16),
//             ),
//             RegisterOrMemory::Register(encoding) => OperandKind::Register(encoding),
//         }
//     }
// }

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
pub struct ModRegRM {
    pub register: Register,
    pub register_or_memory: RegisterOrMemory,
}

impl ModRegRM {
    pub fn new(register: Register, register_or_memory: RegisterOrMemory) -> Self {
        Self {
            register,
            register_or_memory,
        }
    }

    pub fn try_from_byte(mod_rm_byte: u8, it: &mut impl Iterator<Item = u8>) -> Result<Self> {
        let register = Register::try_from_byte(mod_rm_byte >> 3 & 0b111)?;

        let register_or_memory = RegisterOrMemory::try_from_modrm(mod_rm_byte, it)?;

        Ok(ModRegRM {
            register,
            register_or_memory,
        })
    }

    pub fn try_from_iter(it: &mut impl Iterator<Item = u8>) -> Result<Self> {
        let modrm_byte = match it.next() {
            Some(byte) => byte,
            None => return Err(DecodeError::CouldNotReadExtraBytes),
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
    fn test_register_or_memory() {
        struct ByteIterator<'a> {
            data: &'a [u8],
            position: usize,
        }

        impl<'a> Iterator for ByteIterator<'a> {
            type Item = u8;

            fn next(&mut self) -> Option<Self::Item> {
                let byte = self.data.get(self.position)?;
                self.position += 1;
                Some(*byte)
            }
        }

        macro_rules! bytes {
            () => {{
                &mut ByteIterator {
                    data: &[],
                    position: 0,
                }
            }};
            ($b1:expr) => {{
                &mut ByteIterator {
                    data: &[$b1],
                    position: 0,
                }
            }};
            ($b1:expr,$b2:expr) => {{
                &mut ByteIterator {
                    data: &[$b1, $b2],
                    position: 0,
                }
            }};
        }

        macro_rules! test_reg_or_mem {
            ($mod_reg_rm_byte:expr,$bytes:expr,$expected:expr) => {{
                assert_eq!(
                    $expected,
                    RegisterOrMemory::try_from_modrm($mod_reg_rm_byte, $bytes).unwrap()
                );
            }};
        }

        // Indirect
        test_reg_or_mem!(
            0b00_000_000,
            bytes![],
            RegisterOrMemory::Indirect(AddressingMode::BxSi)
        );
        test_reg_or_mem!(
            0b00_000_001,
            bytes![],
            RegisterOrMemory::Indirect(AddressingMode::BxDi)
        );
        test_reg_or_mem!(
            0b00_000_010,
            bytes![],
            RegisterOrMemory::Indirect(AddressingMode::BpSi)
        );
        test_reg_or_mem!(
            0b00_000_011,
            bytes![],
            RegisterOrMemory::Indirect(AddressingMode::BpDi)
        );
        test_reg_or_mem!(
            0b00_000_100,
            bytes![],
            RegisterOrMemory::Indirect(AddressingMode::Si)
        );
        test_reg_or_mem!(
            0b00_000_101,
            bytes![],
            RegisterOrMemory::Indirect(AddressingMode::Di)
        );
        test_reg_or_mem!(
            0b00_000_110,
            bytes![0x12, 0x34],
            RegisterOrMemory::Direct(0x3412)
        );
        test_reg_or_mem!(
            0b00_000_111,
            bytes![],
            RegisterOrMemory::Indirect(AddressingMode::Bx)
        );

        // DisplacementByte
        test_reg_or_mem!(
            0b01_000_000,
            bytes![0x01],
            RegisterOrMemory::DisplacementByte(AddressingMode::BxSi, 1)
        );
        test_reg_or_mem!(
            0b01_000_001,
            bytes![0x01],
            RegisterOrMemory::DisplacementByte(AddressingMode::BxDi, 1)
        );
        test_reg_or_mem!(
            0b01_000_010,
            bytes![0x01],
            RegisterOrMemory::DisplacementByte(AddressingMode::BpSi, 1)
        );
        test_reg_or_mem!(
            0b01_000_011,
            bytes![0x01],
            RegisterOrMemory::DisplacementByte(AddressingMode::BpDi, 1)
        );
        test_reg_or_mem!(
            0b01_000_100,
            bytes![0x01],
            RegisterOrMemory::DisplacementByte(AddressingMode::Si, 1)
        );
        test_reg_or_mem!(
            0b01_000_101,
            bytes![0x01],
            RegisterOrMemory::DisplacementByte(AddressingMode::Di, 1)
        );
        test_reg_or_mem!(
            0b01_000_110,
            bytes![0x01],
            RegisterOrMemory::DisplacementByte(AddressingMode::Bp, 1)
        );
        test_reg_or_mem!(
            0b01_000_111,
            bytes![0x01],
            RegisterOrMemory::DisplacementByte(AddressingMode::Bx, 1)
        );

        // DisplacementWord
        test_reg_or_mem!(
            0b10_000_000,
            bytes![0x12, 0x34],
            RegisterOrMemory::DisplacementWord(AddressingMode::BxSi, 0x3412)
        );
        test_reg_or_mem!(
            0b10_000_001,
            bytes![0x12, 0x34],
            RegisterOrMemory::DisplacementWord(AddressingMode::BxDi, 0x3412)
        );
        test_reg_or_mem!(
            0b10_000_010,
            bytes![0x12, 0x34],
            RegisterOrMemory::DisplacementWord(AddressingMode::BpSi, 0x3412)
        );
        test_reg_or_mem!(
            0b10_000_011,
            bytes![0x12, 0x34],
            RegisterOrMemory::DisplacementWord(AddressingMode::BpDi, 0x3412)
        );
        test_reg_or_mem!(
            0b10_000_100,
            bytes![0x12, 0x34],
            RegisterOrMemory::DisplacementWord(AddressingMode::Si, 0x3412)
        );
        test_reg_or_mem!(
            0b10_000_101,
            bytes![0x12, 0x34],
            RegisterOrMemory::DisplacementWord(AddressingMode::Di, 0x3412)
        );
        test_reg_or_mem!(
            0b10_000_110,
            bytes![0x12, 0x34],
            RegisterOrMemory::DisplacementWord(AddressingMode::Bp, 0x3412)
        );
        test_reg_or_mem!(
            0b10_000_111,
            bytes![0x12, 0x34],
            RegisterOrMemory::DisplacementWord(AddressingMode::Bx, 0x3412)
        );

        // Register
        test_reg_or_mem!(
            0b11_000_000,
            bytes![],
            RegisterOrMemory::Register(Register::AlAx)
        );
        test_reg_or_mem!(
            0b11_000_001,
            bytes![],
            RegisterOrMemory::Register(Register::ClCx)
        );
        test_reg_or_mem!(
            0b11_000_010,
            bytes![],
            RegisterOrMemory::Register(Register::DlDx)
        );
        test_reg_or_mem!(
            0b11_000_011,
            bytes![],
            RegisterOrMemory::Register(Register::BlBx)
        );
        test_reg_or_mem!(
            0b11_000_100,
            bytes![],
            RegisterOrMemory::Register(Register::AhSp)
        );
        test_reg_or_mem!(
            0b11_000_101,
            bytes![],
            RegisterOrMemory::Register(Register::ChBp)
        );
        test_reg_or_mem!(
            0b11_000_110,
            bytes![],
            RegisterOrMemory::Register(Register::DhSi)
        );
        test_reg_or_mem!(
            0b11_000_111,
            bytes![],
            RegisterOrMemory::Register(Register::BhDi)
        );
    }

    macro_rules! test_modrm_to_byte {
        ($expected:expr,$register:expr,$register_or_memory:expr) => {{
            let byte: u8 = ModRegRM::new($register, $register_or_memory).as_byte();
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
