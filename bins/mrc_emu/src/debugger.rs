use glium::backend::glutin::glutin::event::{ScanCode, VirtualKeyCode, WindowEvent};
use glium::glutin::event::{ElementState, Event};
use glium::glutin::event_loop::{ControlFlow, EventLoop};
use mrc_emulator::cpu::State;
use mrc_screen::{Screen, TextMode};
use mrc_x86::{Register, Segment};
use std::cell::RefCell;
use std::rc::Rc;

pub enum DebuggerAction {
    Step,
}

pub struct Debugger {
    text_mode: Rc<RefCell<TextMode>>,
    screen: Screen,
    state: State,
}

impl Debugger {
    pub fn new(event_loop: &EventLoop<()>) -> Self {
        let text_mode = Rc::new(RefCell::new(TextMode::default()));
        Self {
            text_mode: text_mode.clone(),
            screen: Screen::new(event_loop, text_mode),
            state: State::default(),
        }
    }

    pub fn update(&mut self, state: &State) {
        self.state = *state;
    }

    pub fn handle_events(&self, event: &Event<()>) -> Option<DebuggerAction> {
        match event {
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::KeyboardInput { input, .. } => {
                    if input.state == ElementState::Pressed {
                        match input.virtual_keycode {
                            None => None,
                            Some(code) => match code {
                                VirtualKeyCode::F10 => Some(DebuggerAction::Step),
                                _ => None,
                            },
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            },
            _ => None,
        }
    }

    pub fn tick(&mut self) {
        {
            let mut text_mode = self.text_mode.borrow_mut();
            text_mode.set_cursor_position(0, 0);

            self.print_register(
                &mut text_mode,
                "AX",
                self.state.get_word_register_value(Register::AlAx),
            );
            text_mode.teletype_output(' ' as u8);
            self.print_register(
                &mut text_mode,
                "BX",
                self.state.get_word_register_value(Register::BlBx),
            );
            text_mode.teletype_output(' ' as u8);
            self.print_register(
                &mut text_mode,
                "CX",
                self.state.get_word_register_value(Register::ClCx),
            );
            text_mode.teletype_output(' ' as u8);
            self.print_register(
                &mut text_mode,
                "DX",
                self.state.get_word_register_value(Register::DlDx),
            );
            text_mode.teletype_output(' ' as u8);
            self.print_register(
                &mut text_mode,
                "SP",
                self.state.get_word_register_value(Register::AhSp),
            );
            text_mode.teletype_output(' ' as u8);
            self.print_register(
                &mut text_mode,
                "BP",
                self.state.get_word_register_value(Register::ChBp),
            );
            text_mode.teletype_output(' ' as u8);
            self.print_register(
                &mut text_mode,
                "SI",
                self.state.get_word_register_value(Register::DhSi),
            );
            text_mode.teletype_output(' ' as u8);
            self.print_register(
                &mut text_mode,
                "DI",
                self.state.get_word_register_value(Register::BhDi),
            );

            text_mode.set_cursor_position(0, 1);

            self.print_register(
                &mut text_mode,
                "ES",
                self.state.get_segment_value(Segment::Es),
            );
            text_mode.teletype_output(' ' as u8);
            self.print_register(
                &mut text_mode,
                "CS",
                self.state.get_segment_value(Segment::Cs),
            );
            text_mode.teletype_output(' ' as u8);
            self.print_register(
                &mut text_mode,
                "SS",
                self.state.get_segment_value(Segment::Ss),
            );
            text_mode.teletype_output(' ' as u8);
            self.print_register(
                &mut text_mode,
                "DS",
                self.state.get_segment_value(Segment::Ds),
            );
            text_mode.teletype_output(' ' as u8);
            self.print_register(&mut text_mode, "IP", self.state.ip);

            self.print_string(&mut text_mode, "test".to_owned());
        }

        self.screen.tick();
    }

    fn print_string(&self, text_mode: &mut TextMode, value: String) {
        for c in value.into_bytes() {
            text_mode.teletype_output(c as u8);
        }
    }

    fn print_register(&self, text_mode: &mut TextMode, name: &str, value: u16) {
        self.print_string(text_mode, format!("{}:{:04X}", name, value));
    }
}
