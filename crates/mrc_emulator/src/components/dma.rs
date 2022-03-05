//! Simulation for an Intel 8237A DMA Controller
//!
//! IBM PC/XT model 5150 and up uses DMA channel 0 for memory refresh cycles and channel 2 for the
//! fixed disk controller.

use crate::{error::Result, Bus, Port};
use std::cell::Cell;

#[derive(Default)]
pub struct Channel {
    masked: bool,
    address_init: [u8; 2],
    address_current: [u8; 2],
    count_init: [u8; 2],
    count_current: [u8; 2],
    page: u8,
    mode: u8,
}

#[derive(Default)]
pub struct DirectMemoryAccessController {
    command: u8,
    // request: u8,
    index: Cell<u8>,
    channels: [Channel; 4],
}

impl DirectMemoryAccessController {
    fn flip_index(&self) {
        self.index.set(self.index.get() ^ 1);
    }

    fn read_channel_addr(&self, index: usize) -> u8 {
        let channel = &self.channels[index];

        let byte_index = self.index.get() as usize;
        let value = channel.address_current[byte_index];

        log::info!("Reading DMA channel {} address: {:#04X}", index, value);

        self.flip_index();

        value
    }

    fn write_channel_addr(&mut self, channel_index: usize, value: u8) {
        self.flip_index();

        let channel = &mut self.channels[channel_index];

        let byte_index = self.index.get() as usize;
        channel.address_current[byte_index] = value;
        channel.address_init[byte_index] = value;

        log::info!(
            "Write DMA channel {} address: current {:?}, init: {:?}",
            channel_index,
            channel.address_init,
            channel.address_current
        );
    }

    fn read_channel_count(&self, channel_index: usize) -> u8 {
        let channel = &self.channels[channel_index];

        let byte_index = self.index.get() as usize;
        let value = channel.count_current[byte_index];

        log::info!(
            "Reading DMA channel {} count: {:#04X}",
            channel_index,
            value
        );

        self.flip_index();

        value
    }

    fn write_channel_count(&mut self, channel_index: usize, value: u8) {
        self.flip_index();

        let channel = &mut self.channels[channel_index];

        let byte_index = self.index.get() as usize;
        channel.count_current[byte_index] = value;
        channel.count_init[byte_index] = value;

        log::info!(
            "Write DMA channel {} count: current {:?}, init: {:?}",
            channel_index,
            channel.count_init,
            channel.count_current
        );
    }

    fn write_channel_page_register(&mut self, channel_index: usize, value: u8) {
        self.channels[channel_index].page = value;

        log::info!(
            "Write DMA channel {} page regsiter: {:?}",
            channel_index,
            value,
        );
    }

    fn write_command(&mut self, value: u8) {
        self.command = value;

        log::info!("Write DMA command: {:#04X}", value);
    }

    fn write_request(&mut self, _value: u8) {
        todo!()
    }

    fn write_mask(&mut self, value: u8) {
        let channel_index = (value & 0b11) as usize;
        let channel = &mut self.channels[channel_index];

        let is_masked = value >> 2 & 1 != 0;
        channel.masked = is_masked;
        if is_masked {
            self.request(channel_index);
        }
    }

    fn write_mode(&mut self, value: u8) {
        let channel_index = (value & 0b11) as usize;
        self.channels[channel_index].mode = value;

        log::info!("Write DMA channel {} mode: {:#04X}", channel_index, value);
    }

    fn write_reset(&mut self, _value: u8) {
        todo!()
    }

    fn write_master_clear(&mut self, _value: u8) {
        for index in 0..self.channels.len() {
            self.channels[index] = Channel::default();
        }

        log::info!("Write DMA master clear");
    }

    fn request(&mut self, _channel_index: usize) {
        todo!()
    }
}

impl Bus<Port> for DirectMemoryAccessController {
    fn read(&self, port: u16) -> Result<u8> {
        log::info!("Reading from DMA controller on port {:#06x}.", port);

        if port >> 7 == 0 {
            let channel_index = (port >> 1 & 0b11) as usize;
            if port & 1 == 0 {
                // Address
                Ok(self.read_channel_addr(channel_index))
            } else {
                // Count
                Ok(self.read_channel_count(channel_index))
            }
        } else {
            // Page Register
            todo!()
        }
    }

    fn write(&mut self, port: u16, value: u8) -> Result<()> {
        log::info!(
            "Writing {:#04x} to DMA controller on port {:#06x}.",
            value,
            port
        );

        let channel_index = (port >> 1 & 0b11) as usize;

        if port >> 7 & 1 == 0 {
            if port >> 3 & 1 == 0 {
                if port & 1 == 0 {
                    // Address
                    self.write_channel_addr(channel_index, value);
                } else {
                    // Count
                    self.write_channel_count(channel_index, value);
                }
            } else {
                match port & 0b111 {
                    0x00 => self.write_command(value),
                    0x01 => self.write_request(value),
                    0x02 => self.write_mask(value),
                    0x03 => self.write_mode(value),
                    0x04 => self.write_reset(value),
                    0x05 => self.write_master_clear(value),
                    _ => unreachable!(),
                }
            }
        } else {
            // Page Register
            self.write_channel_page_register(channel_index, value);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn init() {
        let mut dmac = DirectMemoryAccessController::default();
        dmac.write(0x00, 0x00).unwrap();
        dmac.write(0x01, 0x00).unwrap();
        dmac.write(0x02, 0x00).unwrap();
        dmac.write(0x03, 0x00).unwrap();
        dmac.write(0x04, 0x00).unwrap();
        dmac.write(0x05, 0x00).unwrap();
        dmac.write(0x06, 0x00).unwrap();
        dmac.write(0x07, 0x00).unwrap();
    }
}
