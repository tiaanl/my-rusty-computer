/// Describes the first bytes in a DOS .EXE file.
/// Pages are 512 bytes in size.
#[repr(C, packed)]
pub struct MzHeader {
    pub id: u16,                 // EXE Signature (should = "MZ")
    pub extra_bytes: u16,        // Bytes on the last page
    pub pages: u16,              // Pages in file
    pub relocation_items: u16,   // Relocations in file
    pub header_size: u16,        // Header size in paragraphs
    pub minimum_allocation: u16, // Minimum extra paragraphs
    pub maximum_allocation: u16, // Maximum extra paragraphs
    pub initial_ss: u16,         // Initial location of stack segment (SS)
    pub initial_sp: u16,         // Initial value of the stack pointer (SP)
    pub checksum: u16,
    pub initial_ip: u16,
    pub initial_cs: u16,
    pub relocation_table: u16,
    pub overlay: u16,
    // pub overlay_information: u16,
}

impl MzHeader {
    /// Returns true if the header contains the `MZ` or `ZM` identifiers in the `id` field.
    pub fn is_valid(&self) -> bool {
        self.id == 0x5A4D || self.id == 0x4D5A
    }

    pub fn header_size_in_bytes(&self) -> u16 {
        //let extra_start = self.pages * 512 - (512 - self.extra_bytes);
        self.header_size * 16
    }

    pub fn file_size_in_bytes(&self) -> u32 {
        self.pages as u32 * 512 + self.extra_bytes as u32
    }
}
