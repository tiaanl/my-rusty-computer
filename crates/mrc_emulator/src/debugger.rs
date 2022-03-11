use crate::cpu::State;
use egui::Button;
use egui_glium;
use egui_glium::egui_winit::winit::event::WindowEvent;
use glium::{Display, Frame};
use mrc_instruction::{Register, Segment};
use std::sync::mpsc::{SendError, Sender};
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub enum EmulatorCommand {
    Run,
    Stop,
    Step,
}

#[derive(Clone, Default)]
pub struct DebuggerState {
    pub state: State,
    pub source: [String; 5],
}

pub struct Debugger {
    egui: egui_glium::EguiGlium,
    debugger_state: Arc<Mutex<DebuggerState>>,
    command_sender: Sender<EmulatorCommand>,
}

fn label(ui: &mut egui::Ui, label: String, value: String) {
    ui.with_layout(egui::Layout::top_down(egui::Align::Center), |ui| {
        ui.heading(value);
        ui.label(label);
    });
}

impl Debugger {
    pub fn new(
        display: &glium::Display,
        debugger_state: Arc<Mutex<DebuggerState>>,
        command_sender: Sender<EmulatorCommand>,
    ) -> Self {
        Self {
            egui: egui_glium::EguiGlium::new(display),
            debugger_state,
            command_sender,
        }
    }

    fn registers(ui: &mut egui::Ui, state: &State) {
        ui.columns(8, |ui| {
            label(
                &mut ui[0],
                "AX".into(),
                format!("{:04X}", state.get_word_register_value(Register::AlAx)),
            );

            label(
                &mut ui[1],
                "BX".into(),
                format!("{:04X}", state.get_word_register_value(Register::BlBx)),
            );

            label(
                &mut ui[2],
                "CX".into(),
                format!("{:04X}", state.get_word_register_value(Register::ClCx)),
            );

            label(
                &mut ui[3],
                "DX".into(),
                format!("{:04X}", state.get_word_register_value(Register::DlDx)),
            );

            label(
                &mut ui[4],
                "SP".into(),
                format!("{:04X}", state.get_word_register_value(Register::AhSp)),
            );

            label(
                &mut ui[5],
                "BP".into(),
                format!("{:04X}", state.get_word_register_value(Register::ChBp)),
            );

            label(
                &mut ui[6],
                "SI".into(),
                format!("{:04X}", state.get_word_register_value(Register::DhSi)),
            );

            label(
                &mut ui[7],
                "DI".into(),
                format!("{:04X}", state.get_word_register_value(Register::BhDi)),
            );
        });

        ui.columns(8, |ui| {
            label(
                &mut ui[0],
                "ES".into(),
                format!("{:04X}", state.get_segment_value(Segment::ES)),
            );

            label(
                &mut ui[1],
                "CS".into(),
                format!("{:04X}", state.get_segment_value(Segment::CS)),
            );

            label(
                &mut ui[2],
                "SS".into(),
                format!("{:04X}", state.get_segment_value(Segment::SS)),
            );

            label(
                &mut ui[3],
                "DS".into(),
                format!("{:04X}", state.get_segment_value(Segment::DS)),
            );

            label(&mut ui[4], "IP".into(), format!("{:04X}", state.ip));
        });
    }

    pub fn send_command(sender: &Sender<EmulatorCommand>, command: EmulatorCommand) {
        if let Err(SendError(command)) = sender.send(command) {
            log::warn!("Could not send emulator command: {:?}", command);
        }
    }

    pub fn needs_redraw(&mut self, display: &glium::Display) -> bool {
        self.egui.run(display, |ctx| {
            let debugger_state = { self.debugger_state.lock().unwrap().clone() };

            let state = &debugger_state.state;

            egui::CentralPanel::default().show(ctx, |ui| {
                Self::registers(ui, state);

                ui.add_space(10.0);

                for line in &debugger_state.source {
                    ui.monospace(line);
                }

                ui.add_space(10.0);

                ui.horizontal(|ui| {
                    if ui
                        .add_sized([100.0, 25.0], Button::new("Run [F5]"))
                        .clicked()
                    {
                        Self::send_command(&self.command_sender, EmulatorCommand::Run);
                    }

                    if ui
                        .add_sized([100.0, 25.0], Button::new("Stop [F12]"))
                        .clicked()
                    {
                        Self::send_command(&self.command_sender, EmulatorCommand::Stop);
                    }

                    if ui
                        .add_sized([100.0, 25.0], Button::new("Step [F10]"))
                        .clicked()
                    {
                        Self::send_command(&self.command_sender, EmulatorCommand::Step);
                    }
                });
            });
        });

        true
    }

    pub fn draw(&mut self, display: &Display, frame: &mut Frame) {
        self.egui.paint(display, frame);
    }

    pub fn on_event(&mut self, event: &WindowEvent) {
        self.egui.on_event(event);
    }
}
