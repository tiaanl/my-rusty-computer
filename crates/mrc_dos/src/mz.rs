use std::io::{Read, Seek, SeekFrom};

/// Values for an entry in the relocation table.
#[derive(Copy, Clone, Default)]
#[repr(C, packed)]
pub struct Relocation {
    pub segment: u16,
    pub offset: u16,
}

/// Describes the first bytes in a DOS .EXE file.
/// Pages are 512 bytes in size.
#[derive(Copy, Clone, Default)]
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
        (self.pages - 1) as u32 * 512u32 + self.extra_bytes as u32
    }

    pub fn code_offset(&self) -> u16 {
        self.header_size_in_bytes()
    }
}

#[derive(Default)]
pub struct ExeHeader {
    pub mz_header: MzHeader,
    pub relocation_table: Vec<Relocation>,
}

impl ExeHeader {
    pub fn new<R: Read + Seek>(reader: &mut R) -> Result<ExeHeader, std::io::Error> {
        let mut exe_header = ExeHeader::default();

        let header_slice = unsafe {
            std::slice::from_raw_parts_mut(
                &mut exe_header.mz_header as *mut _ as *mut u8,
                std::mem::size_of::<MzHeader>(),
            )
        };

        reader.read_exact(header_slice)?;

        let relocation_table_items = exe_header.mz_header.relocation_items as usize;

        exe_header
            .relocation_table
            .resize(relocation_table_items, Relocation::default());
        reader.seek(SeekFrom::Start(
            exe_header.mz_header.relocation_table.into(),
        ))?;

        let slice: &mut [u8] = unsafe {
            std::slice::from_raw_parts_mut(
                exe_header.relocation_table.as_mut_slice() as *mut _ as *mut u8,
                std::mem::size_of::<Relocation>() * relocation_table_items,
            )
        };

        reader.read_exact(slice)?;

        Ok(exe_header)
    }
}
