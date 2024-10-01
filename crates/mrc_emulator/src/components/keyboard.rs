use crate::{Address, Bus};
use std::collections::VecDeque;

#[derive(Default)]
pub struct Keyboard {
    value: u8,

    buffer: VecDeque<u8>,

    clock: bool,
    data: bool,
    reset_on_enable: bool,
}

impl Keyboard {
    fn enable(&mut self, data: bool, clock: bool) -> bool {
        let mut reset = false;

        if self.clock != clock {
            log::info!("Keyboard clock line changing to {}", clock);

            // Toggling the clock line low and then high signals a "reset", which we acknowledge
            // once the data line is high as well.
            self.clock = clock;
            self.reset_on_enable = clock;
        }

        if self.data != data {
            log::info!("Keyboard data line changing to {}", data);

            self.data = data;

            if data && !self.reset_on_enable {
                self.transmit_data(true);
            }
        }

        if self.data && self.reset_on_enable {
            self.reset_device();
            self.reset_on_enable = false;
            reset = true;
        }

        reset
    }

    fn transmit_data(&mut self, ready: bool) {
        // TODO: Check transmit timer
        if ready {
            let value = match self.buffer.front() {
                Some(value) => *value,
                None => 0,
            };

            if true
            /*send_value_to_cpu(value)*/
            {
                log::info!("Keyboard data {:#04X} transmitted.", value);
                self.buffer.pop_front();
            }

            // TODO: Update the transmit timer.
        }
    }

    fn reset_device(&mut self) {
        log::info!("Keyboard reset");

        self.buffer.clear();

        self.set_response(0xAA);
    }

    fn set_response(&mut self, value: u8) {
        self.buffer.push_back(value);

        log::info!(
            "Keyboard response {:#04X} buffered : buffer = {:?}",
            value,
            self.buffer
        );

        self.transmit_data(false);
    }
}

impl Bus for Keyboard {
    fn read(&self, _address: Address) -> u8 {
        log::info!("Writing to keyboard port: {:#04X}", self.value);

        self.value
    }

    fn write(&mut self, _address: Address, value: u8) {
        log::info!("Writing to keyboard port: {:#04X}", value);
        self.value = value;

        let data = value & 0b10000000 == 0;
        let clock = value & 0b01000000 != 0;

        self.enable(data, clock);
    }
}
