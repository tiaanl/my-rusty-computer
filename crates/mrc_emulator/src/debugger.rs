use crate::cpu::State;
use egui::Widget;
use egui_glium;
use egui_glium::egui_winit::winit::event::WindowEvent;
use glium::{Display, Frame};
use mrc_instruction::{Register, Segment};
use std::sync::mpsc::{SendError, Sender};

#[derive(Debug)]
pub enum EmulatorCommand {
    Run,
    Stop,
    Step,
}

pub struct DebuggerState {
    pub state: State,
}

impl Default for DebuggerState {
    fn default() -> Self {
        Self {
            state: Default::default(),
        }
    }
}

pub struct Debugger {
    egui: egui_glium::EguiGlium,
    command_sender: Sender<EmulatorCommand>,
    state: DebuggerState,
}

impl Debugger {
    pub fn new(display: &glium::Display, command_sender: Sender<EmulatorCommand>) -> Self {
        Self {
            egui: egui_glium::EguiGlium::new(display),
            command_sender,
            state: DebuggerState::default(),
        }
    }

    fn registers(ui: &mut egui::Ui, state: &State) {
        ui.horizontal(|ui| {
            ui.vertical(|ui| {
                egui::Label::new("AX").ui(ui);
                egui::Label::new(format!(
                    "{:#06X}",
                    state.get_word_register_value(Register::AlAx)
                ))
                .ui(ui);
            });

            ui.vertical(|ui| {
                egui::Label::new("BX").ui(ui);
                egui::Label::new(format!(
                    "{:#06X}",
                    state.get_word_register_value(Register::BlBx)
                ))
                .ui(ui);
            });

            ui.vertical(|ui| {
                egui::Label::new("CX").ui(ui);
                egui::Label::new(format!(
                    "{:#06X}",
                    state.get_word_register_value(Register::ClCx)
                ))
                .ui(ui);
            });

            ui.vertical(|ui| {
                egui::Label::new("DX").ui(ui);
                egui::Label::new(format!(
                    "{:#06X}",
                    state.get_word_register_value(Register::DlDx)
                ))
                .ui(ui);
            });

            ui.vertical(|ui| {
                egui::Label::new("SP").ui(ui);
                egui::Label::new(format!(
                    "{:#06X}",
                    state.get_word_register_value(Register::AhSp)
                ))
                .ui(ui);
            });

            ui.vertical(|ui| {
                egui::Label::new("BP").ui(ui);
                egui::Label::new(format!(
                    "{:#06X}",
                    state.get_word_register_value(Register::ChBp)
                ))
                .ui(ui);
            });

            ui.vertical(|ui| {
                egui::Label::new("SI").ui(ui);
                egui::Label::new(format!(
                    "{:#06X}",
                    state.get_word_register_value(Register::DhSi)
                ))
                .ui(ui);
            });

            ui.vertical(|ui| {
                egui::Label::new("DI").ui(ui);
                egui::Label::new(format!(
                    "{:#06X}",
                    state.get_word_register_value(Register::BhDi)
                ))
                .ui(ui);
            });
        });

        ui.horizontal(|ui| {
            ui.vertical(|ui| {
                egui::Label::new("ES").ui(ui);
                egui::Label::new(format!("{:#06X}", state.get_segment_value(Segment::ES))).ui(ui);
            });

            ui.vertical(|ui| {
                egui::Label::new("CS").ui(ui);
                egui::Label::new(format!("{:#06X}", state.get_segment_value(Segment::CS))).ui(ui);
            });

            ui.vertical(|ui| {
                egui::Label::new("SS").ui(ui);
                egui::Label::new(format!("{:#06X}", state.get_segment_value(Segment::SS))).ui(ui);
            });

            ui.vertical(|ui| {
                egui::Label::new("DS").ui(ui);
                egui::Label::new(format!("{:#06X}", state.get_segment_value(Segment::DS))).ui(ui);
            });

            ui.vertical(|ui| {
                egui::Label::new("IP").ui(ui);
                egui::Label::new(format!("{:#06X}", state.ip)).ui(ui);
            });
        });
    }

    pub fn send_command(sender: &Sender<EmulatorCommand>, command: EmulatorCommand) {
        if let Err(SendError(command)) = sender.send(command) {
            log::warn!("Could not send emulator command: {:?}", command);
        }
    }

    pub fn needs_redraw(&mut self, display: &glium::Display) -> bool {
        self.egui.run(&display, |ctx| {
            let state = &self.state.state;
            egui::CentralPanel::default().show(ctx, |ui| {
                Self::registers(ui, state);

                if egui::Button::new("Run").ui(ui).clicked() {
                    Self::send_command(&self.command_sender, EmulatorCommand::Run);
                }

                if egui::Button::new("Stop").ui(ui).clicked() {
                    Self::send_command(&self.command_sender, EmulatorCommand::Stop);
                }

                if egui::Button::new("Step").ui(ui).clicked() {
                    Self::send_command(&self.command_sender, EmulatorCommand::Step);
                }
            });
        })
    }

    pub fn draw(&mut self, display: &Display, frame: &mut Frame) {
        self.egui.paint(display, frame);
    }

    pub fn on_event(&mut self, event: &WindowEvent) {
        self.egui.on_event(event);
    }

    pub fn set_state(&mut self, state: DebuggerState) {
        self.state = state;
    }
}
