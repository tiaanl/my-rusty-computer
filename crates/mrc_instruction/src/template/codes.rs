pub type Code = u8;

// Codes

// pub const C_END: Code = 0x00;
pub const C_BYTE: Code = 0x01;
pub const C_REG_BASE: Code = 0x02;
pub const C_MOD_RM: Code = 0x03;
pub const C_MOD_REG_RM: Code = 0x04;
pub const C_IMM_BYTE: Code = 0x05;
pub const C_IMM_WORD: Code = 0x06;
pub const C_IMM_BYTE_SIGN: Code = 0x07;
pub const C_IMM_WORD_SIGN: Code = 0x08;
pub const C_DISP_BYTE: Code = 0x09;
pub const C_DISP_WORD: Code = 0x0A;
pub const C_SEG_OFF: Code = 0x0B;

#[allow(unused)]
pub fn codes_to_string(codes: &[Code]) -> String {
    let mut parts = vec![];

    let mut it = codes.iter();
    while let Some(&code) = it.next() {
        match code {
            // C_END => {
            //     parts.push("C_END".to_owned());
            // }
            C_BYTE => {
                parts.push("C_BYTE".to_owned());
                let byte = it.next().unwrap();
                parts.push(format!("{:#04X}", byte));
            }

            C_REG_BASE => {
                parts.push("C_REG_BASE".to_owned());
                let base = it.next().unwrap();
                parts.push(format!("{:#04X}", base));
            }

            C_MOD_REG_RM => {
                parts.push("C_MOD_REG_RM".to_owned());
            }

            C_MOD_RM => {
                parts.push("C_MOD_RM".to_owned());
                let reg = it.next().unwrap();
                parts.push(format!("{:#04X}", reg));
            }

            C_IMM_BYTE => {
                parts.push("C_IMM_BYTE".to_owned());
            }

            C_IMM_WORD => {
                parts.push("C_IMM_WORD".to_owned());
            }

            C_IMM_BYTE_SIGN => {
                parts.push("C_IMM_BYTE_SIGN".to_owned());
            }

            C_IMM_WORD_SIGN => {
                parts.push("C_IMM_WORD_SIGN".to_owned());
            }

            C_DISP_BYTE => {
                parts.push("C_DISP_BYTE".to_owned());
            }

            C_DISP_WORD => {
                parts.push("C_DISP_WORD".to_owned());
            }

            C_SEG_OFF => {
                parts.push("C_SEG_OFF".to_owned());
            }

            _ => unreachable!("{}", code),
        }
    }

    parts.join(", ")
}
