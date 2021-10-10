use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::cpu::{CPU, State};

pub trait InterruptHandler {
    fn handle(&mut self, state: &CPU);
}

impl Default for InterruptController {
    fn default() -> Self {
        Self {
            handlers: HashMap::new(),
        }
    }
}

pub struct InterruptController {
    handlers: HashMap<u8, Rc<RefCell<dyn InterruptHandler>>>,
}

impl InterruptController {
    pub fn map(&mut self, interrupt: u8, handler: Rc<RefCell<dyn InterruptHandler>>) {
        self.handlers.insert(interrupt, handler);
    }

    pub fn handle(&self, interrupt: u8, cpu: &CPU) {
        if let Some(handler) = self.handlers.get(&interrupt) {
            handler.borrow_mut().handle(cpu);
        }
    }
}
