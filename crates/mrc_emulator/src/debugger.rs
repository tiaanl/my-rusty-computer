use crate::cpu::{State, WordRegister::*};
use egui::Button;
use egui_glium;
use egui_glium::egui_winit::winit::event::WindowEvent;
use glium::{Display, Frame};
use mrc_instruction::Segment;
use std::sync::{
    mpsc::{SendError, Sender},
    Arc, Mutex,
};
use Segment::*;

#[derive(Debug)]
pub enum EmulatorCommand {
    Run,
    Stop,
    Step,
}

#[derive(Clone)]
pub struct SourceLine {
    pub address: mrc_instruction::Address,
    pub instruction: String,
}

impl Default for SourceLine {
    fn default() -> Self {
        Self {
            address: mrc_instruction::Address::new(0, 0),
            instruction: "".to_string(),
        }
    }
}

#[derive(Clone, Default)]
pub struct DebuggerState {
    pub state: State,
    pub source: Vec<SourceLine>,
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
                format!("{:04X}", state.register(AX)),
            );

            label(
                &mut ui[1],
                "BX".into(),
                format!("{:04X}", state.register(BX)),
            );

            label(
                &mut ui[2],
                "CX".into(),
                format!("{:04X}", state.register(CX)),
            );

            label(
                &mut ui[3],
                "DX".into(),
                format!("{:04X}", state.register(DX)),
            );

            label(
                &mut ui[4],
                "SP".into(),
                format!("{:04X}", state.register(SP)),
            );

            label(
                &mut ui[5],
                "BP".into(),
                format!("{:04X}", state.register(BP)),
            );

            label(
                &mut ui[6],
                "SI".into(),
                format!("{:04X}", state.register(SI)),
            );

            label(
                &mut ui[7],
                "DI".into(),
                format!("{:04X}", state.register(DI)),
            );
        });

        ui.columns(8, |ui| {
            label(
                &mut ui[0],
                "ES".into(),
                format!("{:04X}", state.segment(ES)),
            );

            label(
                &mut ui[1],
                "CS".into(),
                format!("{:04X}", state.segment(CS)),
            );

            label(
                &mut ui[2],
                "SS".into(),
                format!("{:04X}", state.segment(SS)),
            );

            label(
                &mut ui[3],
                "DS".into(),
                format!("{:04X}", state.segment(DS)),
            );

            label(&mut ui[4], "IP".into(), format!("{:04X}", state.ip));
        });
    }

    pub fn send_command(sender: &Sender<EmulatorCommand>, command: EmulatorCommand) {
        if let Err(SendError(command)) = sender.send(command) {
            log::warn!("Could not send emulator command: {:?}", command);
        }
    }

    pub fn update(&mut self, display: &glium::Display) -> bool {
        self.egui.run(display, |ctx| {
            let debugger_state = { self.debugger_state.lock().unwrap().clone() };

            let state = &debugger_state.state;

            egui::CentralPanel::default().show(ctx, |ui| {
                Self::registers(ui, state);

                ui.add_space(10.0);

                for line in &debugger_state.source {
                    ui.horizontal(|ui| {
                        if ui.button(line.address.to_string()).clicked() {
                            println!("Breakpoint: {}", line.address.to_string());
                        }
                        ui.monospace(&line.instruction);
                    });
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
        })
    }

    pub fn draw(&mut self, display: &Display, frame: &mut Frame) {
        self.egui.paint(display, frame);
    }

    pub fn on_event(&mut self, event: &WindowEvent) {
        self.egui.on_event(event);
    }
}
