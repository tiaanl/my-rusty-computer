use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::cpu::CPU;

pub trait InterruptHandler {
    fn handle(&mut self, state: &CPU);
}

#[derive(Default)]
pub struct InterruptController {
    handlers: HashMap<u8, Rc<RefCell<dyn InterruptHandler>>>,
}

impl InterruptController {
    pub fn with_handlers(handlers: HashMap<u8, Rc<RefCell<dyn InterruptHandler>>>) -> Self {
        Self { handlers }
    }

    pub fn map(&mut self, interrupt: u8, handler: Rc<RefCell<dyn InterruptHandler>>) {
        self.handlers.insert(interrupt, handler);
    }

    pub fn handle(&self, interrupt: u8, cpu: &CPU) {
        if let Some(handler) = self.handlers.get(&interrupt) {
            handler.borrow_mut().handle(cpu);
        }
    }
}
