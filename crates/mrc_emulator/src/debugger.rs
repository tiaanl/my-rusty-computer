use crate::cpu::Flags;
use crate::{
    cpu::{State, WordRegister::*},
    Address,
};
// use egui::{Button, Color32, Key, Label, Modifiers, RichText, Widget, WidgetText};
// use egui_glium;
// use egui_glium::egui_winit::winit::event::WindowEvent;
// use glium::{Display, Frame};
use mrc_instruction::Segment;
use std::collections::LinkedList;
use std::sync::{
    mpsc::{SendError, Sender},
    Arc, Mutex,
};
use Segment::*;

pub const DEBUGGER_HISTORY_SIZE: usize = 10;

#[derive(Debug)]
pub enum EmulatorCommand {
    Run,
    Step,
    Reset,
    SetBreakpoint(Address),
}

#[derive(Clone)]
pub struct SourceLine {
    address: mrc_instruction::Address,
    instruction: String,
}

impl Default for SourceLine {
    fn default() -> Self {
        Self {
            address: mrc_instruction::Address::new(0, 0),
            instruction: "".to_string(),
        }
    }
}

impl SourceLine {
    pub fn new(address: mrc_instruction::Address, instruction: String) -> Self {
        Self {
            address,
            instruction,
        }
    }
}

#[derive(Clone)]
pub struct DebuggerState {
    pub status: String,
    // pub state: State,
    pub source: Vec<SourceLine>,
    pub history: LinkedList<SourceLine>,
    pub breakpoints: Vec<String>,
}

impl Default for DebuggerState {
    fn default() -> Self {
        let mut history = LinkedList::new();
        for _ in 0..DEBUGGER_HISTORY_SIZE {
            history.push_back(SourceLine::default());
        }

        Self {
            status: "Unknown".to_owned(),
            // state: State::default(),
            source: vec![SourceLine::default(); 5],
            history,
            breakpoints: vec![],
        }
    }
}

pub struct Debugger {
    // egui: egui_glium::EguiGlium,
    debugger_state: Arc<Mutex<DebuggerState>>,
    command_sender: Sender<EmulatorCommand>,
}

fn label(ui: &mut egui::Ui, label: String, value: String) {
    ui.with_layout(egui::Layout::top_down(egui::Align::Center), |ui| {
        ui.heading(value);
        Label::new(WidgetText::RichText(
            RichText::new(label).color(Color32::DARK_GRAY),
        ))
        .ui(ui);
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

            ui[5].horizontal(|ui| {
                fn flag(ui: &mut egui::Ui, is_set: bool, label: &'static str) {
                    egui::Label::new(WidgetText::RichText(RichText::new(label).heading().color(
                        if is_set {
                            Color32::LIGHT_GREEN
                        } else {
                            Color32::DARK_GRAY
                        },
                    )))
                    .ui(ui);
                }

                flag(ui, state.flags.contains(Flags::CARRY), "C");
                flag(ui, state.flags.contains(Flags::PARITY), "P");
                flag(ui, state.flags.contains(Flags::AUX_CARRY), "A");
                flag(ui, state.flags.contains(Flags::ZERO), "Z");
                flag(ui, state.flags.contains(Flags::SIGN), "S");
                flag(ui, state.flags.contains(Flags::TRAP), "T");
                flag(ui, state.flags.contains(Flags::INTERRUPT), "I");
                flag(ui, state.flags.contains(Flags::DIRECTION), "D");
                flag(ui, state.flags.contains(Flags::OVERFLOW), "O");
            });
        });
    }

    pub fn send_command(sender: &Sender<EmulatorCommand>, command: EmulatorCommand) {
        if let Err(SendError(command)) = sender.send(command) {
            warn!("Could not send emulator command: {:?}", command);
        }
    }

    pub fn update(&mut self, display: &glium::Display) -> bool {
        self.egui.run(display, |ctx| {
            let debugger_state = { self.debugger_state.lock().unwrap().clone() };

            // let state = &debugger_state.state;

            // egui::CentralPanel::default().show(ctx, |ui| {
            //     ui.label(debugger_state.status);
            //
            //     Self::registers(ui, state);
            //
            //     ui.add_space(10.0);
            //
            //     let source_line = |ui: &mut egui::Ui,
            //                        command_sender: &Sender<EmulatorCommand>,
            //                        line: &SourceLine,
            //                        color: Color32| {
            //         ui.horizontal(|ui| {
            //             if ui
            //                 .button(WidgetText::RichText(
            //                     RichText::new(line.address.to_string()).monospace(),
            //                 ))
            //                 .clicked()
            //             {
            //                 Self::send_command(
            //                     command_sender,
            //                     EmulatorCommand::SetBreakpoint(line.address.flat()),
            //                 );
            //                 println!("Breakpoint: {}", line.address);
            //             }
            //
            //             Label::new(WidgetText::RichText(
            //                 RichText::new(&line.instruction).color(color).monospace(),
            //             ))
            //             .ui(ui);
            //         });
            //
            //         ui.add_space(3.0);
            //     };
            //
            //     for line in &debugger_state.history {
            //         source_line(ui, &self.command_sender, line, Color32::DARK_GRAY);
            //     }
            //
            //     for (i, line) in debugger_state.source.iter().enumerate() {
            //         source_line(
            //             ui,
            //             &self.command_sender,
            //             line,
            //             if i == 0 {
            //                 Color32::LIGHT_GREEN
            //             } else {
            //                 Color32::LIGHT_GRAY
            //             },
            //         );
            //     }
            //
            //     ui.add_space(10.0);
            //
            //     ui.horizontal(|ui| {
            //         {
            //             let mut input = ui.input_mut();
            //
            //             if input.consume_key(Modifiers::NONE, Key::S) {
            //                 Self::send_command(&self.command_sender, EmulatorCommand::Step);
            //             }
            //
            //             if input.consume_key(Modifiers::NONE, Key::R) {
            //                 Self::send_command(&self.command_sender, EmulatorCommand::Run);
            //             }
            //
            //             if input.consume_key(Modifiers::NONE, Key::J) {
            //                 Self::send_command(&self.command_sender, EmulatorCommand::Reset);
            //             }
            //         }
            //
            //         if ui
            //             .add_sized([100.0, 25.0], Button::new("Run [R]"))
            //             .clicked()
            //         {
            //             Self::send_command(&self.command_sender, EmulatorCommand::Run);
            //         }
            //
            //         if ui
            //             .add_sized([100.0, 25.0], Button::new("Step [S]"))
            //             .clicked()
            //         {
            //             Self::send_command(&self.command_sender, EmulatorCommand::Step);
            //         }
            //
            //         if ui
            //             .add_sized([100.0, 25.0], Button::new("Reset [J]"))
            //             .clicked()
            //         {
            //             Self::send_command(&self.command_sender, EmulatorCommand::Reset);
            //         }
            //     });
            //
            //     ui.add_space(10.0);
            //
            //     ui.vertical(|ui| {
            //         for addr in debugger_state.breakpoints {
            //             ui.monospace(addr);
            //         }
            //     });
            // });
        })
    }

    pub fn draw(&mut self, display: &Display, frame: &mut Frame) {
        self.egui.paint(display, frame);
    }

    pub fn on_event(&mut self, event: &WindowEvent) {
        self.egui.on_event(event);
    }
}
