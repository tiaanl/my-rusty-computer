pub enum Code {
    Byte(u8),
    ImmediateByte,
    ImmediateWord,
    ModRegRM,
    ModRM(u8),
}
