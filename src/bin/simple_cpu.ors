use std::io::Read;
use std::path::Path;

fn load_bin_into_memory<P: AsRef<Path>>(path: P, memory: &mut Vec<u8>) -> std::io::Result<()> {
    let mut file = std::fs::File::open(path)?;
    let bytes_read = file.read_to_end(memory)?;

    // println!("bytes read: {:#06X}", bytes_read);

    Ok(())
}

fn flat_address(segment: u16, offset: u16) -> usize {
    ((segment as usize) << 4) + (offset as usize)
}

enum ByteRegister {
    AL,
    CL,
    DL,
    BL,
    AH,
    CH,
    DH,
    BH,
}

fn byte_register_to_string(register: ByteRegister) -> &'static str {
    match register {
        ByteRegister::AL => "al",
        ByteRegister::CL => "cl",
        ByteRegister::DL => "dl",
        ByteRegister::BL => "bl",
        ByteRegister::AH => "ah",
        ByteRegister::CH => "ch",
        ByteRegister::DH => "dh",
        ByteRegister::BH => "bh",
    }
}

fn encoding_to_byte_register(encoding: u8) -> ByteRegister {
    match encoding {
        0b000 => ByteRegister::AL,
        0b001 => ByteRegister::CL,
        0b010 => ByteRegister::DL,
        0b011 => ByteRegister::BL,
        0b100 => ByteRegister::AH,
        0b101 => ByteRegister::CH,
        0b110 => ByteRegister::DH,
        0b111 => ByteRegister::BH,
        _ => panic!("Invalid register encoding!"),
    }
}

enum WordRegister {
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI,
}

fn word_register_to_string(register: WordRegister) -> &'static str {
    match register {
        WordRegister::AX => "ax",
        WordRegister::CX => "cx",
        WordRegister::DX => "dx",
        WordRegister::BX => "bx",
        WordRegister::SP => "sp",
        WordRegister::BP => "bp",
        WordRegister::SI => "si",
        WordRegister::DI => "di",
    }
}

fn encoding_to_word_register(encoding: u8) -> WordRegister {
    match encoding {
        0b000 => WordRegister::AX,
        0b001 => WordRegister::CX,
        0b010 => WordRegister::DX,
        0b011 => WordRegister::BX,
        0b100 => WordRegister::SP,
        0b101 => WordRegister::BP,
        0b110 => WordRegister::SI,
        0b111 => WordRegister::DI,
        _ => panic!("Invalid register encoding!"),
    }
}

enum Segment {
    ES,
    CS,
    SS,
    DS,
}

enum Flag {
    Carry,
    Reserved1,
    Parity,
    Reserved3,
    Adjust,
    Reserved5,
    Zero,
    Sign,
    Trap,
    Interrupt,
    Direction,
    Overflow,
}

struct CPU<'a> {
    registers: [u16; 16],
    segments: [u16; 4],
    ip: u16,
    flags: u16,
    memory: &'a mut Vec<u8>,
}

impl<'a> CPU<'a> {
    fn new(memory: &mut Vec<u8>) -> CPU {
        CPU {
            registers: [0u16; 16],
            segments: [
                0x0000, // ES
                0xFFFF, // CS
                0x0000, // SS
                0x0000, // DS
            ],
            ip: 0x0000,
            flags: 0,
            memory,
        }
    }

    fn get_byte_register(&self, register: ByteRegister) -> u8 {
        match register {
            ByteRegister::AL => (self.registers[0] & 0x00FF) as u8,
            ByteRegister::CL => (self.registers[1] & 0x00FF) as u8,
            ByteRegister::DL => (self.registers[2] & 0x00FF) as u8,
            ByteRegister::BL => (self.registers[3] & 0x00FF) as u8,
            ByteRegister::AH => (self.registers[0] >> 8) as u8,
            ByteRegister::CH => (self.registers[1] >> 8) as u8,
            ByteRegister::DH => (self.registers[2] >> 8) as u8,
            ByteRegister::BH => (self.registers[3] >> 8) as u8,
        }
    }

    fn set_byte_register(&mut self, register: &ByteRegister, value: u8) {
        match register {
            ByteRegister::AL => self.registers[0] += self.registers[0] & 0xff00 + (value as u16),
            ByteRegister::CL => self.registers[1] += self.registers[1] & 0xff00 + (value as u16),
            ByteRegister::DL => self.registers[2] += self.registers[2] & 0xff00 + (value as u16),
            ByteRegister::BL => self.registers[3] += self.registers[3] & 0xff00 + (value as u16),
            ByteRegister::AH => {
                self.registers[0] += self.registers[0] & 0x00ff + ((value as u16) << 8)
            }
            ByteRegister::CH => {
                self.registers[1] += self.registers[1] & 0x00ff + ((value as u16) << 8)
            }
            ByteRegister::DH => {
                self.registers[2] += self.registers[2] & 0x00ff + ((value as u16) << 8)
            }
            ByteRegister::BH => {
                self.registers[3] += self.registers[3] & 0x00ff + ((value as u16) << 8)
            }
        }
    }

    fn get_word_register(&self, register: &WordRegister) -> u16 {
        match register {
            WordRegister::AX => self.registers[0],
            WordRegister::CX => self.registers[1],
            WordRegister::DX => self.registers[2],
            WordRegister::BX => self.registers[3],
            WordRegister::SP => self.registers[4],
            WordRegister::BP => self.registers[5],
            WordRegister::SI => self.registers[6],
            WordRegister::DI => self.registers[7],
        }
    }

    fn set_word_register(&mut self, register: &WordRegister, value: u16) {
        match register {
            WordRegister::AX => self.registers[0] = value,
            WordRegister::CX => self.registers[1] = value,
            WordRegister::DX => self.registers[2] = value,
            WordRegister::BX => self.registers[3] = value,
            WordRegister::SP => self.registers[4] = value,
            WordRegister::BP => self.registers[5] = value,
            WordRegister::SI => self.registers[6] = value,
            WordRegister::DI => self.registers[7] = value,
        }
    }

    fn get_segment(&self, segment: &Segment) -> u16 {
        match segment {
            Segment::ES => self.segments[0],
            Segment::CS => self.segments[1],
            Segment::SS => self.segments[2],
            Segment::DS => self.segments[3],
        }
    }

    fn set_segment(&mut self, segment: &Segment, value: u16) {
        match segment {
            Segment::ES => self.segments[0] = value,
            Segment::CS => self.segments[1] = value,
            Segment::SS => self.segments[2] = value,
            Segment::DS => self.segments[3] = value,
        };
    }

    fn get_execution_address(&self) -> usize {
        let cs = self.get_segment(&Segment::CS) as usize;
        let ip = self.ip as usize;

        // println!("execution_address: {:#06X}", (cs << 4usize) + ip);

        (cs << 4usize) + ip
    }

    fn is_flag_set(&self, flag: Flag) -> bool {
        match flag {
            Flag::Carry => self.flags & (1 << 0x0) > 0,
            Flag::Reserved1 => self.flags & (1 << 0x1) > 0,
            Flag::Parity => self.flags & (1 << 0x2) > 0,
            Flag::Reserved3 => self.flags & (1 << 0x3) > 0,
            Flag::Adjust => self.flags & (1 << 0x4) > 0,
            Flag::Reserved5 => self.flags & (1 << 0x5) > 0,
            Flag::Zero => self.flags & (1 << 0x6) > 0,
            Flag::Sign => self.flags & (1 << 0x7) > 0,
            Flag::Trap => self.flags & (1 << 0x8) > 0,
            Flag::Interrupt => self.flags & (1 << 0x9) > 0,
            Flag::Direction => self.flags & (1 << 0xA) > 0,
            Flag::Overflow => self.flags & (1 << 0xB) > 0,
        }
    }

    fn set_flag(&mut self, flag: Flag) {
        match flag {
            Flag::Carry => self.flags |= (1 << 0x0),
            Flag::Reserved1 => self.flags |= (1 << 0x1),
            Flag::Parity => self.flags |= (1 << 0x2),
            Flag::Reserved3 => self.flags |= (1 << 0x3),
            Flag::Adjust => self.flags |= (1 << 0x4),
            Flag::Reserved5 => self.flags |= (1 << 0x5),
            Flag::Zero => self.flags |= (1 << 0x6),
            Flag::Sign => self.flags |= (1 << 0x7),
            Flag::Trap => self.flags |= (1 << 0x8),
            Flag::Interrupt => self.flags |= (1 << 0x9),
            Flag::Direction => self.flags |= (1 << 0xA),
            Flag::Overflow => self.flags |= (1 << 0xB),
        }
    }

    fn clear_flag(&mut self, flag: Flag) {
        match flag {
            Flag::Carry => self.flags &= !(1 << 0x0),
            Flag::Reserved1 => self.flags &= !(1 << 0x1),
            Flag::Parity => self.flags &= !(1 << 0x2),
            Flag::Reserved3 => self.flags &= !(1 << 0x3),
            Flag::Adjust => self.flags &= !(1 << 0x4),
            Flag::Reserved5 => self.flags &= !(1 << 0x5),
            Flag::Zero => self.flags &= !(1 << 0x6),
            Flag::Sign => self.flags &= !(1 << 0x7),
            Flag::Trap => self.flags &= !(1 << 0x8),
            Flag::Interrupt => self.flags &= !(1 << 0x9),
            Flag::Direction => self.flags &= !(1 << 0xA),
            Flag::Overflow => self.flags &= !(1 << 0xB),
        }
    }

    fn advance(&mut self, offset: i16) {
        self.ip = self.ip.wrapping_add(offset as u16);
    }

    fn fetch_u8(&mut self) -> u8 {
        let value = self.memory[flat_address(self.get_segment(&Segment::CS), self.ip)];
        self.advance(1);
        value
    }

    fn fetch_u16(&mut self) -> u16 {
        let addr = flat_address(self.get_segment(&Segment::CS), self.ip);
        let value: u16 = (self.memory[addr] as u16) + ((self.memory[addr + 1] as u16) << 8);
        self.advance(2);
        value
    }

    fn read_u16(&self, starting_address: usize) -> u16 {
        let low = self.memory[starting_address];
        let high = self.memory[starting_address + 1];

        (low as u16) + ((high as u16) << 8u16)
    }

    fn step(&mut self) {
        let ip: usize = self.get_execution_address();

        let op_code = self.memory[ip];

        match op_code {
            0x00 => self.execute_00(),
            0x01 => self.execute_01(),
            0x02 => self.execute_02(),
            0x03 => self.execute_03(),
            0x04 => self.execute_04(),
            0x05 => self.execute_05(),
            0x06 => self.execute_06(),
            0x07 => self.execute_07(),
            0x08 => self.execute_08(),
            0x09 => self.execute_09(),
            0x0a => self.execute_0a(),
            0x0b => self.execute_0b(),
            0x0c => self.execute_0c(),
            0x0d => self.execute_0d(),
            0x0e => self.execute_0e(),
            0x0f => self.execute_0f(),
            0x10 => self.execute_10(),
            0x11 => self.execute_11(),
            0x12 => self.execute_12(),
            0x13 => self.execute_13(),
            0x14 => self.execute_14(),
            0x15 => self.execute_15(),
            0x16 => self.execute_16(),
            0x17 => self.execute_17(),
            0x18 => self.execute_18(),
            0x19 => self.execute_19(),
            0x1a => self.execute_1a(),
            0x1b => self.execute_1b(),
            0x1c => self.execute_1c(),
            0x1d => self.execute_1d(),
            0x1e => self.execute_1e(),
            0x1f => self.execute_1f(),
            0x20 => self.execute_20(),
            0x21 => self.execute_21(),
            0x22 => self.execute_22(),
            0x23 => self.execute_23(),
            0x24 => self.execute_24(),
            0x25 => self.execute_25(),
            0x26 => self.execute_26(),
            0x27 => self.execute_27(),
            0x28 => self.execute_28(),
            0x29 => self.execute_29(),
            0x2a => self.execute_2a(),
            0x2b => self.execute_2b(),
            0x2c => self.execute_2c(),
            0x2d => self.execute_2d(),
            0x2e => self.execute_2e(),
            0x2f => self.execute_2f(),
            0x30 => self.execute_30(),
            0x31 => self.execute_31(),
            0x32 => self.execute_32(),
            0x33 => self.execute_33(),
            0x34 => self.execute_34(),
            0x35 => self.execute_35(),
            0x36 => self.execute_36(),
            0x37 => self.execute_37(),
            0x38 => self.execute_38(),
            0x39 => self.execute_39(),
            0x3a => self.execute_3a(),
            0x3b => self.execute_3b(),
            0x3c => self.execute_3c(),
            0x3d => self.execute_3d(),
            0x3e => self.execute_3e(),
            0x3f => self.execute_3f(),
            0x40 => self.execute_40(),
            0x41 => self.execute_41(),
            0x42 => self.execute_42(),
            0x43 => self.execute_43(),
            0x44 => self.execute_44(),
            0x45 => self.execute_45(),
            0x46 => self.execute_46(),
            0x47 => self.execute_47(),
            0x48 => self.execute_48(),
            0x49 => self.execute_49(),
            0x4a => self.execute_4a(),
            0x4b => self.execute_4b(),
            0x4c => self.execute_4c(),
            0x4d => self.execute_4d(),
            0x4e => self.execute_4e(),
            0x4f => self.execute_4f(),
            0x50 => self.execute_50(),
            0x51 => self.execute_51(),
            0x52 => self.execute_52(),
            0x53 => self.execute_53(),
            0x54 => self.execute_54(),
            0x55 => self.execute_55(),
            0x56 => self.execute_56(),
            0x57 => self.execute_57(),
            0x58 => self.execute_58(),
            0x59 => self.execute_59(),
            0x5a => self.execute_5a(),
            0x5b => self.execute_5b(),
            0x5c => self.execute_5c(),
            0x5d => self.execute_5d(),
            0x5e => self.execute_5e(),
            0x5f => self.execute_5f(),
            0x60 => self.execute_60(),
            0x61 => self.execute_61(),
            0x62 => self.execute_62(),
            0x63 => self.execute_63(),
            0x64 => self.execute_64(),
            0x65 => self.execute_65(),
            0x66 => self.execute_66(),
            0x67 => self.execute_67(),
            0x68 => self.execute_68(),
            0x69 => self.execute_69(),
            0x6a => self.execute_6a(),
            0x6b => self.execute_6b(),
            0x6c => self.execute_6c(),
            0x6d => self.execute_6d(),
            0x6e => self.execute_6e(),
            0x6f => self.execute_6f(),
            0x70 => self.execute_70(),
            0x71 => self.execute_71(),
            0x72 => self.execute_72(),
            0x73 => self.execute_73(),
            0x74 => self.execute_74(),
            0x75 => self.execute_75(),
            0x76 => self.execute_76(),
            0x77 => self.execute_77(),
            0x78 => self.execute_78(),
            0x79 => self.execute_79(),
            0x7a => self.execute_7a(),
            0x7b => self.execute_7b(),
            0x7c => self.execute_7c(),
            0x7d => self.execute_7d(),
            0x7e => self.execute_7e(),
            0x7f => self.execute_7f(),
            0x80 => self.execute_80(),
            0x81 => self.execute_81(),
            0x82 => self.execute_82(),
            0x83 => self.execute_83(),
            0x84 => self.execute_84(),
            0x85 => self.execute_85(),
            0x86 => self.execute_86(),
            0x87 => self.execute_87(),
            0x88 => self.execute_88(),
            0x89 => self.execute_89(),
            0x8a => self.execute_8a(),
            0x8b => self.execute_8b(),
            0x8c => self.execute_8c(),
            0x8d => self.execute_8d(),
            0x8e => self.execute_8e(),
            0x8f => self.execute_8f(),
            0x90 => self.execute_90(),
            0x91 => self.execute_91(),
            0x92 => self.execute_92(),
            0x93 => self.execute_93(),
            0x94 => self.execute_94(),
            0x95 => self.execute_95(),
            0x96 => self.execute_96(),
            0x97 => self.execute_97(),
            0x98 => self.execute_98(),
            0x99 => self.execute_99(),
            0x9a => self.execute_9a(),
            0x9b => self.execute_9b(),
            0x9c => self.execute_9c(),
            0x9d => self.execute_9d(),
            0x9e => self.execute_9e(),
            0x9f => self.execute_9f(),
            0xa0 => self.execute_a0(),
            0xa1 => self.execute_a1(),
            0xa2 => self.execute_a2(),
            0xa3 => self.execute_a3(),
            0xa4 => self.execute_a4(),
            0xa5 => self.execute_a5(),
            0xa6 => self.execute_a6(),
            0xa7 => self.execute_a7(),
            0xa8 => self.execute_a8(),
            0xa9 => self.execute_a9(),
            0xaa => self.execute_aa(),
            0xab => self.execute_ab(),
            0xac => self.execute_ac(),
            0xad => self.execute_ad(),
            0xae => self.execute_ae(),
            0xaf => self.execute_af(),
            0xb0 => self.execute_b0_bf(),
            0xb1 => self.execute_b0_bf(),
            0xb2 => self.execute_b0_bf(),
            0xb3 => self.execute_b0_bf(),
            0xb4 => self.execute_b0_bf(),
            0xb5 => self.execute_b0_bf(),
            0xb6 => self.execute_b0_bf(),
            0xb7 => self.execute_b0_bf(),
            0xb8 => self.execute_b0_bf(),
            0xb9 => self.execute_b0_bf(),
            0xba => self.execute_b0_bf(),
            0xbb => self.execute_b0_bf(),
            0xbc => self.execute_b0_bf(),
            0xbd => self.execute_b0_bf(),
            0xbe => self.execute_b0_bf(),
            0xbf => self.execute_b0_bf(),
            0xc0 => self.execute_c0(),
            0xc1 => self.execute_c1(),
            0xc2 => self.execute_c2(),
            0xc3 => self.execute_c3(),
            0xc4 => self.execute_c4(),
            0xc5 => self.execute_c5(),
            0xc6 => self.execute_c6(),
            0xc7 => self.execute_c7(),
            0xc8 => self.execute_c8(),
            0xc9 => self.execute_c9(),
            0xca => self.execute_ca(),
            0xcb => self.execute_cb(),
            0xcc => self.execute_cc(),
            0xcd => self.execute_cd(),
            0xce => self.execute_ce(),
            0xcf => self.execute_cf(),
            0xd0 => self.execute_d0(),
            0xd1 => self.execute_d1(),
            0xd2 => self.execute_d2(),
            0xd3 => self.execute_d3(),
            0xd4 => self.execute_d4(),
            0xd5 => self.execute_d5(),
            0xd6 => self.execute_d6(),
            0xd7 => self.execute_d7(),
            0xd8 => self.execute_d8(),
            0xd9 => self.execute_d9(),
            0xda => self.execute_da(),
            0xdb => self.execute_db(),
            0xdc => self.execute_dc(),
            0xdd => self.execute_dd(),
            0xde => self.execute_de(),
            0xdf => self.execute_df(),
            0xe0 => self.execute_e0(),
            0xe1 => self.execute_e1(),
            0xe2 => self.execute_e2(),
            0xe3 => self.execute_e3(),
            0xe4 => self.execute_e4(),
            0xe5 => self.execute_e5(),
            0xe6 => self.execute_e6(),
            0xe7 => self.execute_e7(),
            0xe8 => self.execute_e8(),
            0xe9 => self.execute_e9(),
            0xea => self.execute_ea(),
            0xeb => self.execute_eb(),
            0xec => self.execute_ec(),
            0xed => self.execute_ed(),
            0xee => self.execute_ee(),
            0xef => self.execute_ef(),
            0xf0 => self.execute_f0(),
            0xf1 => self.execute_f1(),
            0xf2 => self.execute_f2(),
            0xf3 => self.execute_f3(),
            0xf4 => self.execute_f4(),
            0xf5 => self.execute_f5(),
            0xf6 => self.execute_f6(),
            0xf7 => self.execute_f7(),
            0xf8 => self.execute_f8(),
            0xf9 => self.execute_f9(),
            0xfa => self.execute_fa(),
            0xfb => self.execute_fb(),
            0xfc => self.execute_fc(),
            0xfd => self.execute_fd(),
            0xfe => self.execute_fe(),
            0xff => self.execute_ff(),
        }
    }

    fn execute_00(&mut self) {
        panic!("execute 00");
    }

    fn execute_01(&mut self) {
        panic!("execute 01");
    }

    fn execute_02(&mut self) {
        panic!("execute 02");
    }

    fn execute_03(&mut self) {
        panic!("execute 03");
    }

    fn execute_04(&mut self) {
        panic!("execute 04");
    }

    fn execute_05(&mut self) {
        panic!("execute 05");
    }

    fn execute_06(&mut self) {
        panic!("execute 06");
    }

    fn execute_07(&mut self) {
        panic!("execute 07");
    }

    fn execute_08(&mut self) {
        panic!("execute 08");
    }

    fn execute_09(&mut self) {
        panic!("execute 09");
    }

    fn execute_0a(&mut self) {
        panic!("execute 0a");
    }

    fn execute_0b(&mut self) {
        panic!("execute 0b");
    }

    fn execute_0c(&mut self) {
        panic!("execute 0c");
    }

    fn execute_0d(&mut self) {
        panic!("execute 0d");
    }

    fn execute_0e(&mut self) {
        panic!("execute 0e");
    }

    fn execute_0f(&mut self) {
        panic!("execute 0f");
    }

    fn execute_10(&mut self) {
        panic!("execute 10");
    }

    fn execute_11(&mut self) {
        panic!("execute 11");
    }

    fn execute_12(&mut self) {
        panic!("execute 12");
    }

    fn execute_13(&mut self) {
        panic!("execute 13");
    }

    fn execute_14(&mut self) {
        panic!("execute 14");
    }

    fn execute_15(&mut self) {
        panic!("execute 15");
    }

    fn execute_16(&mut self) {
        panic!("execute 16");
    }

    fn execute_17(&mut self) {
        panic!("execute 17");
    }

    fn execute_18(&mut self) {
        panic!("execute 18");
    }

    fn execute_19(&mut self) {
        panic!("execute 19");
    }

    fn execute_1a(&mut self) {
        panic!("execute 1a");
    }

    fn execute_1b(&mut self) {
        panic!("execute 1b");
    }

    fn execute_1c(&mut self) {
        panic!("execute 1c");
    }

    fn execute_1d(&mut self) {
        panic!("execute 1d");
    }

    fn execute_1e(&mut self) {
        panic!("execute 1e");
    }

    fn execute_1f(&mut self) {
        panic!("execute 1f");
    }

    fn execute_20(&mut self) {
        panic!("execute 20");
    }

    fn execute_21(&mut self) {
        panic!("execute 21");
    }

    fn execute_22(&mut self) {
        panic!("execute 22");
    }

    fn execute_23(&mut self) {
        panic!("execute 23");
    }

    fn execute_24(&mut self) {
        panic!("execute 24");
    }

    fn execute_25(&mut self) {
        panic!("execute 25");
    }

    fn execute_26(&mut self) {
        panic!("execute 26");
    }

    fn execute_27(&mut self) {
        panic!("execute 27");
    }

    fn execute_28(&mut self) {
        panic!("execute 28");
    }

    fn execute_29(&mut self) {
        panic!("execute 29");
    }

    fn execute_2a(&mut self) {
        panic!("execute 2a");
    }

    fn execute_2b(&mut self) {
        panic!("execute 2b");
    }

    fn execute_2c(&mut self) {
        panic!("execute 2c");
    }

    fn execute_2d(&mut self) {
        panic!("execute 2d");
    }

    fn execute_2e(&mut self) {
        panic!("execute 2e");
    }

    fn execute_2f(&mut self) {
        panic!("execute 2f");
    }

    fn execute_30(&mut self) {
        panic!("execute 30");
    }

    fn execute_31(&mut self) {
        panic!("execute 31");
    }

    fn execute_32(&mut self) {
        panic!("execute 32");
    }

    fn execute_33(&mut self) {
        panic!("execute 33");
    }

    fn execute_34(&mut self) {
        panic!("execute 34");
    }

    fn execute_35(&mut self) {
        panic!("execute 35");
    }

    fn execute_36(&mut self) {
        panic!("execute 36");
    }

    fn execute_37(&mut self) {
        panic!("execute 37");
    }

    fn execute_38(&mut self) {
        panic!("execute 38");
    }

    fn execute_39(&mut self) {
        panic!("execute 39");
    }

    fn execute_3a(&mut self) {
        panic!("execute 3a");
    }

    fn execute_3b(&mut self) {
        panic!("execute 3b");
    }

    fn execute_3c(&mut self) {
        panic!("execute 3c");
    }

    fn execute_3d(&mut self) {
        panic!("execute 3d");
    }

    fn execute_3e(&mut self) {
        panic!("execute 3e");
    }

    fn execute_3f(&mut self) {
        panic!("execute 3f");
    }

    fn execute_40(&mut self) {
        panic!("execute 40");
    }

    fn execute_41(&mut self) {
        panic!("execute 41");
    }

    fn execute_42(&mut self) {
        panic!("execute 42");
    }

    fn execute_43(&mut self) {
        panic!("execute 43");
    }

    fn execute_44(&mut self) {
        panic!("execute 44");
    }

    fn execute_45(&mut self) {
        panic!("execute 45");
    }

    fn execute_46(&mut self) {
        panic!("execute 46");
    }

    fn execute_47(&mut self) {
        panic!("execute 47");
    }

    fn execute_48(&mut self) {
        panic!("execute 48");
    }

    fn execute_49(&mut self) {
        panic!("execute 49");
    }

    fn execute_4a(&mut self) {
        panic!("execute 4a");
    }

    fn execute_4b(&mut self) {
        panic!("execute 4b");
    }

    fn execute_4c(&mut self) {
        panic!("execute 4c");
    }

    fn execute_4d(&mut self) {
        panic!("execute 4d");
    }

    fn execute_4e(&mut self) {
        panic!("execute 4e");
    }

    fn execute_4f(&mut self) {
        panic!("execute 4f");
    }

    fn execute_50(&mut self) {
        panic!("execute 50");
    }

    fn execute_51(&mut self) {
        panic!("execute 51");
    }

    fn execute_52(&mut self) {
        panic!("execute 52");
    }

    fn execute_53(&mut self) {
        panic!("execute 53");
    }

    fn execute_54(&mut self) {
        panic!("execute 54");
    }

    fn execute_55(&mut self) {
        panic!("execute 55");
    }

    fn execute_56(&mut self) {
        panic!("execute 56");
    }

    fn execute_57(&mut self) {
        panic!("execute 57");
    }

    fn execute_58(&mut self) {
        panic!("execute 58");
    }

    fn execute_59(&mut self) {
        panic!("execute 59");
    }

    fn execute_5a(&mut self) {
        panic!("execute 5a");
    }

    fn execute_5b(&mut self) {
        panic!("execute 5b");
    }

    fn execute_5c(&mut self) {
        panic!("execute 5c");
    }

    fn execute_5d(&mut self) {
        panic!("execute 5d");
    }

    fn execute_5e(&mut self) {
        panic!("execute 5e");
    }

    fn execute_5f(&mut self) {
        panic!("execute 5f");
    }

    fn execute_60(&mut self) {
        panic!("execute 60");
    }

    fn execute_61(&mut self) {
        panic!("execute 61");
    }

    fn execute_62(&mut self) {
        panic!("execute 62");
    }

    fn execute_63(&mut self) {
        panic!("execute 63");
    }

    fn execute_64(&mut self) {
        panic!("execute 64");
    }

    fn execute_65(&mut self) {
        panic!("execute 65");
    }

    fn execute_66(&mut self) {
        panic!("execute 66");
    }

    fn execute_67(&mut self) {
        panic!("execute 67");
    }

    fn execute_68(&mut self) {
        panic!("execute 68");
    }

    fn execute_69(&mut self) {
        panic!("execute 69");
    }

    fn execute_6a(&mut self) {
        panic!("execute 6a");
    }

    fn execute_6b(&mut self) {
        panic!("execute 6b");
    }

    fn execute_6c(&mut self) {
        panic!("execute 6c");
    }

    fn execute_6d(&mut self) {
        panic!("execute 6d");
    }

    fn execute_6e(&mut self) {
        panic!("execute 6e");
    }

    fn execute_6f(&mut self) {
        panic!("execute 6f");
    }

    fn execute_70(&mut self) {
        panic!("execute 70");
    }

    fn execute_71(&mut self) {
        panic!("execute 71");
    }

    fn execute_72(&mut self) {
        panic!("execute 72");
    }

    fn execute_73(&mut self) {
        panic!("execute 73");
    }

    fn execute_74(&mut self) {
        panic!("execute 74");
    }

    fn execute_75(&mut self) {
        panic!("execute 75");
    }

    fn execute_76(&mut self) {
        panic!("execute 76");
    }

    fn execute_77(&mut self) {
        panic!("execute 77");
    }

    fn execute_78(&mut self) {
        panic!("execute 78");
    }

    fn execute_79(&mut self) {
        panic!("execute 79");
    }

    fn execute_7a(&mut self) {
        panic!("execute 7a");
    }

    fn execute_7b(&mut self) {
        panic!("execute 7b");
    }

    fn execute_7c(&mut self) {
        panic!("execute 7c");
    }

    fn execute_7d(&mut self) {
        panic!("execute 7d");
    }

    fn execute_7e(&mut self) {
        panic!("execute 7e");
    }

    fn execute_7f(&mut self) {
        panic!("execute 7f");
    }

    fn execute_80(&mut self) {
        panic!("execute 80");
    }

    fn execute_81(&mut self) {
        panic!("execute 81");
    }

    fn execute_82(&mut self) {
        panic!("execute 82");
    }

    fn execute_83(&mut self) {
        panic!("execute 83");
    }

    fn execute_84(&mut self) {
        panic!("execute 84");
    }

    fn execute_85(&mut self) {
        panic!("execute 85");
    }

    fn execute_86(&mut self) {
        panic!("execute 86");
    }

    fn execute_87(&mut self) {
        panic!("execute 87");
    }

    fn execute_88(&mut self) {
        panic!("execute 88");
    }

    fn execute_89(&mut self) {
        panic!("execute 89");
    }

    fn execute_8a(&mut self) {
        panic!("execute 8a");
    }

    fn execute_8b(&mut self) {
        panic!("execute 8b");
    }

    fn execute_8c(&mut self) {
        panic!("execute 8c");
    }

    fn execute_8d(&mut self) {
        panic!("execute 8d");
    }

    fn execute_8e(&mut self) {
        panic!("execute 8e");
    }

    fn execute_8f(&mut self) {
        panic!("execute 8f");
    }

    fn execute_90(&mut self) {
        // XCHG - Exchange - Register with accumulator
        // 1 0 0 1 0 reg

        let addr = self.get_execution_address();

        let op_code = self.fetch_u8();
        println!("op_code: {:b}", op_code);
        let reg_encoding = op_code & 0b111;
        let register = encoding_to_word_register(reg_encoding);

        let ax = self.get_word_register(&WordRegister::AX);
        let reg_value = self.get_word_register(&register);

        self.set_word_register(&WordRegister::AX, reg_value);
        self.set_word_register(&register, ax);

        println!("{:#07X} xchg {}", addr, word_register_to_string(register));
    }

    fn execute_91(&mut self) {
        panic!("execute 91");
    }

    fn execute_92(&mut self) {
        panic!("execute 92");
    }

    fn execute_93(&mut self) {
        panic!("execute 93");
    }

    fn execute_94(&mut self) {
        panic!("execute 94");
    }

    fn execute_95(&mut self) {
        panic!("execute 95");
    }

    fn execute_96(&mut self) {
        panic!("execute 96");
    }

    fn execute_97(&mut self) {
        panic!("execute 97");
    }

    fn execute_98(&mut self) {
        panic!("execute 98");
    }

    fn execute_99(&mut self) {
        panic!("execute 99");
    }

    fn execute_9a(&mut self) {
        panic!("execute 9a");
    }

    fn execute_9b(&mut self) {
        panic!("execute 9b");
    }

    fn execute_9c(&mut self) {
        panic!("execute 9c");
    }

    fn execute_9d(&mut self) {
        panic!("execute 9d");
    }

    fn execute_9e(&mut self) {
        panic!("execute 9e");
    }

    fn execute_9f(&mut self) {
        panic!("execute 9f");
    }

    fn execute_a0(&mut self) {
        panic!("execute a0");
    }

    fn execute_a1(&mut self) {
        panic!("execute a1");
    }

    fn execute_a2(&mut self) {
        panic!("execute a2");
    }

    fn execute_a3(&mut self) {
        panic!("execute a3");
    }

    fn execute_a4(&mut self) {
        panic!("execute a4");
    }

    fn execute_a5(&mut self) {
        panic!("execute a5");
    }

    fn execute_a6(&mut self) {
        panic!("execute a6");
    }

    fn execute_a7(&mut self) {
        panic!("execute a7");
    }

    fn execute_a8(&mut self) {
        panic!("execute a8");
    }

    fn execute_a9(&mut self) {
        panic!("execute a9");
    }

    fn execute_aa(&mut self) {
        panic!("execute aa");
    }

    fn execute_ab(&mut self) {
        panic!("execute ab");
    }

    fn execute_ac(&mut self) {
        panic!("execute ac");
    }

    fn execute_ad(&mut self) {
        panic!("execute ad");
    }

    fn execute_ae(&mut self) {
        panic!("execute ae");
    }

    fn execute_af(&mut self) {
        panic!("execute af");
    }

    fn execute_b0_bf(&mut self) {
        // MOV - Move - Immediate to register
        // 1 0 1 1 w reg | data | data if w = 1

        let addr = self.get_execution_address();

        let op_code = self.fetch_u8();

        let data_is_word = (op_code & 0b00001000) == 0b00001000;
        println!("width: {}", data_is_word);
        let reg = op_code & 0b00000111;

        if data_is_word {
            let data = self.fetch_u16();

            let register = encoding_to_word_register(reg);

            self.set_word_register(&register, data);

            println!(
                "{:#07X} mov {}, {:#06X}",
                addr,
                word_register_to_string(register),
                data
            );
        } else {
            let data = self.fetch_u8();

            let register = encoding_to_byte_register(reg);

            self.set_byte_register(&register, data);

            println!(
                "{:#07X} mov {}, {:#04X}",
                addr,
                byte_register_to_string(register),
                data
            );
        }
    }

    fn execute_c0(&mut self) {
        panic!("execute c0");
    }

    fn execute_c1(&mut self) {
        panic!("execute c1");
    }

    fn execute_c2(&mut self) {
        panic!("execute c2");
    }

    fn execute_c3(&mut self) {
        panic!("execute c3");
    }

    fn execute_c4(&mut self) {
        panic!("execute c4");
    }

    fn execute_c5(&mut self) {
        panic!("execute c5");
    }

    fn execute_c6(&mut self) {
        panic!("execute c6");
    }

    fn execute_c7(&mut self) {
        panic!("execute c7");
    }

    fn execute_c8(&mut self) {
        panic!("execute c8");
    }

    fn execute_c9(&mut self) {
        panic!("execute c9");
    }

    fn execute_ca(&mut self) {
        panic!("execute ca");
    }

    fn execute_cb(&mut self) {
        panic!("execute cb");
    }

    fn execute_cc(&mut self) {
        panic!("execute cc");
    }

    fn execute_cd(&mut self) {
        panic!("execute cd");
    }

    fn execute_ce(&mut self) {
        panic!("execute ce");
    }

    fn execute_cf(&mut self) {
        panic!("execute cf");
    }

    fn execute_d0(&mut self) {
        panic!("execute d0");
    }

    fn execute_d1(&mut self) {
        panic!("execute d1");
    }

    fn execute_d2(&mut self) {
        panic!("execute d2");
    }

    fn execute_d3(&mut self) {
        panic!("execute d3");
    }

    fn execute_d4(&mut self) {
        panic!("execute d4");
    }

    fn execute_d5(&mut self) {
        panic!("execute d5");
    }

    fn execute_d6(&mut self) {
        panic!("execute d6");
    }

    fn execute_d7(&mut self) {
        panic!("execute d7");
    }

    fn execute_d8(&mut self) {
        panic!("execute d8");
    }

    fn execute_d9(&mut self) {
        panic!("execute d9");
    }

    fn execute_da(&mut self) {
        panic!("execute da");
    }

    fn execute_db(&mut self) {
        panic!("execute db");
    }

    fn execute_dc(&mut self) {
        panic!("execute dc");
    }

    fn execute_dd(&mut self) {
        panic!("execute dd");
    }

    fn execute_de(&mut self) {
        panic!("execute de");
    }

    fn execute_df(&mut self) {
        panic!("execute df");
    }

    fn execute_e0(&mut self) {
        panic!("execute e0");
    }

    fn execute_e1(&mut self) {
        panic!("execute e1");
    }

    fn execute_e2(&mut self) {
        panic!("execute e2");
    }

    fn execute_e3(&mut self) {
        panic!("execute e3");
    }

    fn execute_e4(&mut self) {
        panic!("execute e4");
    }

    fn execute_e5(&mut self) {
        panic!("execute e5");
    }

    fn execute_e6(&mut self) {
        panic!("execute e6");
    }

    fn execute_e7(&mut self) {
        panic!("execute e7");
    }

    fn execute_e8(&mut self) {
        panic!("execute e8");
    }

    fn execute_e9(&mut self) {
        panic!("execute e9");
    }

    fn execute_ea(&mut self) {
        // JMP - Unconditional - Direct intersegment
        // 1 1 1 0 1 0 1 0 | offset-low | offset-high | segment-low | segment-high

        let addr = self.get_execution_address();

        self.advance(1); // op_code
        let offset = self.fetch_u16();
        let segment = self.fetch_u16();

        self.set_segment(&Segment::CS, segment);
        self.ip = offset;

        println!("{:#07X} jmp {:#06X}:{:#06X}", addr, segment, offset);
    }

    fn execute_eb(&mut self) {
        panic!("execute eb");
    }

    fn execute_ec(&mut self) {
        panic!("execute ec");
    }

    fn execute_ed(&mut self) {
        panic!("execute ed");
    }

    fn execute_ee(&mut self) {
        panic!("execute ee");
    }

    fn execute_ef(&mut self) {
        panic!("execute ef");
    }

    fn execute_f0(&mut self) {
        panic!("execute f0");
    }

    fn execute_f1(&mut self) {
        panic!("execute f1");
    }

    fn execute_f2(&mut self) {
        panic!("execute f2");
    }

    fn execute_f3(&mut self) {
        panic!("execute f3");
    }

    fn execute_f4(&mut self) {
        panic!("execute f4");
    }

    fn execute_f5(&mut self) {
        panic!("execute f5");
    }

    fn execute_f6(&mut self) {
        panic!("execute f6");
    }

    fn execute_f7(&mut self) {
        panic!("execute f7");
    }

    fn execute_f8(&mut self) {
        panic!("execute f8");
    }

    fn execute_f9(&mut self) {
        panic!("execute f9");
    }

    fn execute_fa(&mut self) {
        // CLI - Clear interrupt
        // 1 1 1 1 1 0 1 0

        let addr = self.get_execution_address();

        self.advance(1); // op_code

        self.clear_flag(Flag::Interrupt);

        println!("{:#07X} cli", addr);
    }

    fn execute_fb(&mut self) {
        panic!("execute fb");
    }

    fn execute_fc(&mut self) {
        panic!("execute fc");
    }

    fn execute_fd(&mut self) {
        panic!("execute fd");
    }

    fn execute_fe(&mut self) {
        panic!("execute fe");
    }

    fn execute_ff(&mut self) {
        panic!("execute ff");
    }
}

fn main() -> std::result::Result<(), ()> {
    const MAX_MEMORY_SIZE: usize = 0xFFFFF;
    let mut memory: Vec<u8> = Vec::with_capacity(MAX_MEMORY_SIZE);
    memory.resize(MAX_MEMORY_SIZE - 0xFFFF, 0);
    // println!("len before read: {:#06X}", memory.len());

    if let Err(err) = load_bin_into_memory(r"C:\Code\my-rusty-computer\data\bios.bin", &mut memory)
    {
        eprintln!("{}", err);
        return Err(());
    }

    // println!("flat: {:#06X}", flat_address(0xFFFF, 0xF000));
    // println!("len after read: {:#06X}", memory.len());

    for i in 0..10 {
        print!("{:#04X} ", memory[0xF03F3 + i]);
    }
    println!();

    let mut cpu = CPU::new(&mut memory);

    loop {
        cpu.step();
    }

    Ok(())
}
