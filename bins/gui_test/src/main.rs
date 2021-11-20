use egui::{FontDefinitions, FontFamily, TextStyle, Widget};
use egui_glium::EguiGlium;
use glium::{glutin, Display};

fn create_display(event_loop: &glutin::event_loop::EventLoop<()>) -> Display {
    let window_builder = glutin::window::WindowBuilder::new()
        .with_resizable(true)
        .with_inner_size(glutin::dpi::LogicalSize {
            width: 800.0,
            height: 600.0,
        })
        .with_title("GUI Test");

    let context_builder = glutin::ContextBuilder::new()
        .with_depth_buffer(0)
        .with_srgb(true)
        .with_stencil_buffer(0)
        .with_vsync(true);

    Display::new(window_builder, context_builder, event_loop).unwrap()
}

fn gui(ctx: &egui::CtxRef) -> bool {
    let mut quit = false;

    egui::CentralPanel::default().show(ctx, |ui| {
        ui.heading("Registers");

        ui.horizontal(|ui| {
            for value in [0u16, 0, 0, 0, 0, 0, 0, 0] {
                egui::Label::new(format!("{:04X}", value))
                    .monospace()
                    .ui(ui);
            }
        });

        ui.horizontal(|ui| {
            for name in ["AX", "BX", "CX", "DX", "BP", "SP", "SI", "DI"] {
                egui::Label::new(format!(" {} ", name))
                    .monospace()
                    .small()
                    .ui(ui);
            }
        });

        if ui.button("Quit").clicked() {
            quit = true;
        }
    });

    quit
}

fn main() {
    let event_loop = glutin::event_loop::EventLoop::with_user_event();
    let display = create_display(&event_loop);

    let mut egui = EguiGlium::new(&display);

    let mut font_definitions = FontDefinitions::default();
    font_definitions
        .family_and_size
        .insert(TextStyle::Body, (FontFamily::Proportional, 32.0));
    font_definitions
        .family_and_size
        .insert(TextStyle::Monospace, (FontFamily::Monospace, 32.0));
    font_definitions
        .family_and_size
        .insert(TextStyle::Heading, (FontFamily::Proportional, 56.0));

    egui.ctx().set_fonts(font_definitions);

    event_loop.run(move |event, _, control_flow| {
        let mut redraw = || {
            egui.begin_frame(&display);

            let quit = gui(egui.ctx());

            let (needs_repaint, shapes) = egui.end_frame(&display);

            *control_flow = if quit {
                glutin::event_loop::ControlFlow::Exit
            } else if needs_repaint {
                display.gl_window().window().request_redraw();
                glutin::event_loop::ControlFlow::Poll
            } else {
                glutin::event_loop::ControlFlow::Wait
            };

            {
                use glium::Surface as _;

                let mut target = display.draw();

                let color = egui::Rgba::from_rgb(0.1, 0.3, 0.2);
                target.clear_color(color[0], color[1], color[2], color[3]);

                egui.paint(&display, &mut target, shapes);

                target.finish().unwrap()
            }
        };

        match event {
            glutin::event::Event::RedrawEventsCleared if cfg!(windows) => redraw(),
            glutin::event::Event::RedrawRequested(_) if cfg!(windows) => redraw(),

            glutin::event::Event::WindowEvent { event, .. } => {
                if egui.is_quit_event(&event) {
                    *control_flow = glutin::event_loop::ControlFlow::Exit;
                }

                egui.on_event(&event);

                display.gl_window().window().request_redraw();
            }

            _ => (),
        }
    });
}
