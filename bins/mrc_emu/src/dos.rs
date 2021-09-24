use super::memory::RandomAccessMemory;
use mrc_dos::mz::MzHeader;
use std::io::{Read, Seek, SeekFrom};

#[repr(C, packed)]
struct CommandTail {
    count: u8,         // number of bytes returned
    buffer: [u8; 127], // the buffer itself
}

impl Default for CommandTail {
    fn default() -> Self {
        let mut s = Self {
            count: 0,
            buffer: [0; 127],
        };

        s.count = 3;
        s.buffer[0] = ' ' as u8;
        s.buffer[1] = '1' as u8;
        s.buffer[2] = '2' as u8;

        s
    }
}

#[repr(C, packed)]
struct ProgramSegmentPrefix {
    exit: [u8; 2],    // CP/M-like exit point
    next_seg: u16,    // Segment of first byte beyond memory allocated or program
    fill_1: u8,       // single char fill
    far_call: u8,     // far call opcode
    cpm_entry: u32,   // CPM Service Request address
    int_22: u32,      // Terminate Address
    int_23: u32,      // Break Address
    int_24: u32,      // Critical Error Address
    psp_parent: u16,  // Parent PSP Segment
    files: [u8; 20],  // File Table - 0xff is unused
    environment: u16, // Segment of environment table
    stack: u32,       // SS:SP Save point for int 0x21 calls
    max_files: u16,   // Maximum open files
    file_table: u32,  // Pointer to File Table PSP:0x18
    prev_psp: u32,    // Pointer to previous PSP
    interim_flag: u8,
    true_name_flag: u8,
    nn_flags: u16,
    dos_version: u16,
    fill_2: [u8; 14], // Lot's of unused stuff i can't care above
    service: [u8; 3], // INT 0x21 Service call int 0x21;retf;
    fill_3: [u8; 9],  // This has some blocks with FCB info
    fcb1: [u8; 16],   // first FCB
    fcb2: [u8; 16],   // second FCB
    fill_4: [u8; 4],  // unused
    command_tail: CommandTail,
}

impl Default for ProgramSegmentPrefix {
    fn default() -> Self {
        Self {
            exit: [0; 2],
            next_seg: 0,
            fill_1: 0,
            far_call: 0,
            cpm_entry: 0,
            int_22: 0,
            int_23: 0,
            int_24: 0,
            psp_parent: 0,
            files: [0; 20],
            environment: 0,
            stack: 0,
            max_files: 0,
            file_table: 0,
            prev_psp: 0,
            interim_flag: 0,
            true_name_flag: 0,
            nn_flags: 0,
            dos_version: 0,
            fill_2: [0; 14],
            service: [0; 3],
            fill_3: [0; 9],
            fcb1: [0; 16],
            fcb2: [0; 16],
            fill_4: [0; 4],
            command_tail: CommandTail::default(),
        }
    }
}

#[repr(C, packed)]
struct ParamBlockExec {
    env_seg: u16,
    cmd_tail: u32,
    fcb1: u32,
    fcb2: u32,
    init_ss_sp: u32,
    init_cs_ip: u32,
}

#[repr(C, packed)]
struct ParamBlockOverlay {
    load_seg: u16,
    relocation: u16,
}

#[repr(C, packed)]
struct ParamBlock {
    exec: ParamBlockExec,
    overlay: ParamBlockOverlay,
}

pub fn load_binary(
    physical_memory: &mut RandomAccessMemory,
    file: &mut std::fs::File,
) -> std::io::Result<()> {
    // Write the program segment prefix to memory.
    let psp = ProgramSegmentPrefix::default(); // 0x100 in size
    let psp_slice = unsafe {
        std::slice::from_raw_parts(
            (&psp as *const ProgramSegmentPrefix) as *const u8,
            std::mem::size_of::<ProgramSegmentPrefix>(),
        )
    };

    physical_memory.data[0..0x100].clone_from_slice(psp_slice);

    let mut header: MzHeader = unsafe { std::mem::zeroed() };

    // Make the config_slice point to the header struct.
    let config_slice = unsafe {
        std::slice::from_raw_parts_mut(
            &mut header as *mut _ as *mut u8,
            std::mem::size_of::<MzHeader>(),
        )
    };

    let mut is_mz = false;
    if let Err(err) = file.read_exact(config_slice) {
        // It is OK If we could not read enough bytes from the file to fill physical memory.
        if err.kind() != std::io::ErrorKind::UnexpectedEof {
            return Err(err);
        }
    } else if header.is_valid() {
        is_mz = true
    }

    // Set the pointer in the file to where we want to continue reading the code.
    if is_mz {
        file.seek(SeekFrom::Start(header.header_size_in_bytes().into()))?;
    } else {
        file.seek(SeekFrom::Start(0))?;
    }

    // Read the code from the file into the physical memory.
    if let Err(err) = file.read_exact(&mut physical_memory.data[0x100..]) {
        if err.kind() != std::io::ErrorKind::UnexpectedEof {
            return Err(err);
        }
    }

    Ok(())
}
