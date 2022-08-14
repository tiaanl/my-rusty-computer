pub struct Emitter(pub Vec<u8>);

impl Emitter {
    pub fn segment_prefix(&mut self, seg: u8) {
        match seg {
            0 => self.byte(0x26),
            1 => self.byte(0x2E),
            2 => self.byte(0x36),
            3 => self.byte(0x3E),
            _ => unreachable!(),
        }
    }

    pub fn byte(&mut self, byte: u8) {
        self.0.push(byte);
    }

    pub fn immediate_byte(&mut self, value: u8) {
        self.byte(value);
    }

    pub fn immediate_word(&mut self, value: u16) {
        for byte in value.to_le_bytes() {
            self.byte(byte);
        }
    }

    fn mod_reg_rm(&mut self, mode: u8, reg: u8, reg_mem: u8) {
        self.byte((mode << 6) | (reg << 3) | reg_mem);
    }

    pub fn mod_rm_direct(&mut self, reg: u8, address: i32) {
        let mode = 0b00;
        let rm = 0b110;

        self.mod_reg_rm(mode, reg, rm);
        for byte in (address as u16).to_le_bytes() {
            self.byte(byte);
        }
    }
}
