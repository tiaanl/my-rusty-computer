use bitflags::bitflags;
use mrc_instruction::{Register, Segment};
use std::fmt::{Display, Formatter};

bitflags! {
    pub struct Flags : u16 {
        const CARRY = 1 << 0;
        const RESERVED_1 = 1 << 1;
        const PARITY = 1 << 2;
        const RESERVED_3 = 1 << 3;
        const AUX_CARRY = 1 << 4;
        const RESERVED_5 = 1 << 5;
        const ZERO = 1 << 6;
        const SIGN = 1 << 7;
        const TRAP = 1 << 8;
        const INTERRUPT = 1 << 9;
        const DIRECTION = 1 << 10;
        const OVERFLOW = 1 << 11;
    }
}

#[derive(Copy, Clone, Default, PartialEq)]
struct WordRegisters {
    ax: u16,
    cx: u16,
    dx: u16,
    bx: u16,

    sp: u16,
    bp: u16,
    si: u16,
    di: u16,
}

#[cfg(target_endian = "little")]
#[derive(Copy, Clone, Default, PartialEq)]
struct ByteRegisters {
    al: u8,
    ah: u8,
    cl: u8,
    ch: u8,
    dl: u8,
    dh: u8,
    bl: u8,
    bh: u8,
}

#[cfg(target_endian = "big")]
#[derive(Copy, Clone, Default, PartialEq)]
struct ByteRegisters {
    ah: u8,
    al: u8,
    ch: u8,
    cl: u8,
    dh: u8,
    dl: u8,
    bh: u8,
    bl: u8,
}

#[derive(Copy, Clone)]
union Registers {
    byte: ByteRegisters,
    word: WordRegisters,
}

impl Default for Registers {
    fn default() -> Self {
        Self {
            word: WordRegisters::default(),
        }
    }
}

impl PartialEq for Registers {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.word == other.word }
    }
}

#[derive(Copy, Clone, Default, PartialEq)]
struct Segments {
    es: u16,
    cs: u16,
    ss: u16,
    ds: u16,
}

#[derive(Copy, Clone, PartialEq)]
pub struct State {
    registers: Registers,
    segments: Segments,

    // TODO: Do not have this public.  Right now it is required to set the reset vector. Maybe set
    //       with a builder config?
    pub ip: u16,
    pub flags: Flags,
}

impl State {
    pub fn with_flags(mut self, flags: Flags) -> Self {
        self.flags = flags;
        self
    }

    #[inline(always)]
    pub fn get_byte_register_value(&self, register: Register) -> u8 {
        use Register::*;

        unsafe {
            match register {
                AlAx => self.registers.byte.al,
                AhSp => self.registers.byte.ah,
                ClCx => self.registers.byte.cl,
                ChBp => self.registers.byte.ch,
                DlDx => self.registers.byte.dl,
                DhSi => self.registers.byte.dh,
                BlBx => self.registers.byte.bl,
                BhDi => self.registers.byte.bh,
            }
        }
    }

    #[inline(always)]
    pub fn get_word_register_value(&self, register: Register) -> u16 {
        use Register::*;

        unsafe {
            match register {
                AlAx => self.registers.word.ax,
                ClCx => self.registers.word.cx,
                DlDx => self.registers.word.dx,
                BlBx => self.registers.word.bx,
                AhSp => self.registers.word.sp,
                ChBp => self.registers.word.bp,
                DhSi => self.registers.word.si,
                BhDi => self.registers.word.di,
            }
        }
    }

    #[inline(always)]
    pub fn set_byte_register_value(&mut self, register: Register, value: u8) {
        use Register::*;

        match register {
            AlAx => self.registers.byte.al = value,
            AhSp => self.registers.byte.ah = value,
            ClCx => self.registers.byte.cl = value,
            ChBp => self.registers.byte.ch = value,
            DlDx => self.registers.byte.dl = value,
            DhSi => self.registers.byte.dh = value,
            BlBx => self.registers.byte.bl = value,
            BhDi => self.registers.byte.bh = value,
        };
    }

    #[inline(always)]
    pub fn set_word_register_value(&mut self, register: Register, value: u16) {
        use Register::*;

        match register {
            AlAx => self.registers.word.ax = value,
            AhSp => self.registers.word.sp = value,
            ClCx => self.registers.word.cx = value,
            ChBp => self.registers.word.bp = value,
            DlDx => self.registers.word.dx = value,
            DhSi => self.registers.word.si = value,
            BlBx => self.registers.word.bx = value,
            BhDi => self.registers.word.di = value,
        }
    }

    #[inline(always)]
    pub fn get_segment_value(&self, segment: Segment) -> u16 {
        use Segment::*;

        match segment {
            ES => self.segments.es,
            CS => self.segments.cs,
            SS => self.segments.ss,
            DS => self.segments.ds,
        }
    }

    #[inline(always)]
    pub fn set_segment_value(&mut self, segment: Segment, value: u16) {
        use Segment::*;

        match segment {
            ES => self.segments.es = value,
            CS => self.segments.cs = value,
            SS => self.segments.ss = value,
            DS => self.segments.ds = value,
        }
    }
}

impl Default for State {
    fn default() -> Self {
        Self {
            registers: Registers::default(),
            segments: Segments::default(),
            ip: 0,
            flags: Flags::empty(),
        }
    }
}

impl Display for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        unsafe {
            write!(f, "AX: {:04X} ", self.registers.word.ax)?;
            write!(f, "BX: {:04X} ", self.registers.word.bx)?;
            write!(f, "CX: {:04X} ", self.registers.word.cx)?;
            write!(f, "DX: {:04X} ", self.registers.word.dx)?;

            write!(f, "SP: {:04X} ", self.registers.word.sp)?;
            write!(f, "BP: {:04X} ", self.registers.word.bp)?;
            write!(f, "SI: {:04X} ", self.registers.word.si)?;
            write!(f, "DI: {:04X} ", self.registers.word.di)?;
        }

        write!(f, "ES: {:04X} ", self.segments.es)?;
        write!(f, "CS: {:04X} ", self.segments.cs)?;
        write!(f, "SS: {:04X} ", self.segments.ss)?;
        write!(f, "DS: {:04X} ", self.segments.ds)?;

        write!(f, "IP: {:04X} ", self.ip)?;

        macro_rules! print_flag {
            ($name:ident,$flag:expr) => {{
                if self.flags.contains($flag) {
                    write!(f, "{}", stringify!($name))?;
                } else {
                    write!(f, ".")?;
                }
            }};
        }

        print_flag!(O, Flags::OVERFLOW);
        print_flag!(D, Flags::DIRECTION);
        print_flag!(I, Flags::INTERRUPT);
        print_flag!(T, Flags::TRAP);
        print_flag!(S, Flags::SIGN);
        print_flag!(Z, Flags::ZERO);
        // print_flag!(R, Flags::RESERVED_5);
        print_flag!(A, Flags::AUX_CARRY);
        // print_flag!(R, Flags::RESERVED_3);
        print_flag!(P, Flags::PARITY);
        // print_flag!(R, Flags::RESERVED_1);
        print_flag!(C, Flags::CARRY);

        Ok(())
    }
}
