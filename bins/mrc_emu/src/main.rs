mod components;
mod config;
mod emulator;
mod interrupts;

use crate::config::Config;
use crate::emulator::BIOS_SIZE;
use crate::interrupts::InterruptManager;
// use glium::{glutin, Surface};
use mrc_emulator::components::rom::ReadOnlyMemory;
// use mrc_emulator::debugger::{Debugger, DebuggerState};
use std::io::Read;

fn load_rom(path: impl AsRef<std::path::Path>) -> std::io::Result<Vec<u8>> {
    let metadata = std::fs::metadata(&path)?;
    let file_size = metadata.len();

    let mut data = vec![0; file_size as usize];

    std::fs::File::open(&path)?.read_exact(data.as_mut_slice())?;

    Ok(data)
}

/// Create a [ReadOnlyMemory] instance that has a fixed `size`.  If the path is valid and could
/// be loaded from disk, then it will be mapped to the beginning of the block.
fn create_bios(path: Option<String>) -> ReadOnlyMemory {
    if let Some(path) = path {
        log::info!("Loading BIOS from: {path}");
        if let Ok(mut data) = load_rom(&path) {
            if data.len() > BIOS_SIZE {
                log::warn!(
                    "BIOS file truncated from {} to {}. ({})",
                    data.len(),
                    BIOS_SIZE,
                    &path
                );
            }
            data.resize(BIOS_SIZE, 0);
            return ReadOnlyMemory::from_vec(data);
        }
    }

    log::info!("Using internal BIOS.");
    let bytes = include_bytes!("../ext/mrc_bios/bios.bin");
    ReadOnlyMemory::from_vec(Vec::from(&bytes[..]))
}

fn main() {
    pretty_env_logger::init();

    let config = match Config::load() {
        Ok(cfg) => cfg,
        Err(err) => {
            log::error!("Could not load configuration! {:?}", err);
            std::process::exit(1);
        }
    };

    // let event_loop = glutin::event_loop::EventLoop::new();

    let bios = create_bios(config.bios);

    // let window_builder = glutin::window::WindowBuilder::new()
    //     .with_resizable(true)
    //     .with_position(glutin::dpi::LogicalPosition::new(
    //         config.debugger_position[0],
    //         config.debugger_position[1],
    //     ))
    //     .with_inner_size(glutin::dpi::LogicalSize {
    //         width: 600.0,
    //         height: 800.0,
    //     })
    //     .with_title("My Rusty Computer (Debugger)");

    // let context_builder = glutin::ContextBuilder::new()
    //     .with_gl_profile(glutin::GlProfile::Core)
    //     .with_depth_buffer(0)
    //     .with_srgb(true)
    //     .with_stencil_buffer(0)
    //     .with_vsync(true);

    // let display = glium::Display::new(window_builder, context_builder, &event_loop)
    //     .expect("Could not create display.");

    // let (command_sender, command_receiver) = std::sync::mpsc::channel();

    // let debugger_state = Arc::new(Mutex::new(DebuggerState::default()));
    // let mut debugger = Debugger::new(&display, debugger_state.clone(), command_sender);

    // std::thread::spawn(move || {
    emulator::Emulator::new(bios, /*debugger_state,*/ |_cpu| {
        // Set the CPU reset vector: 0xFFFF0
        // cpu.jump_to(0xF000, 0xFFF0);
    })
    .run(/*command_receiver*/);
    // });

    // event_loop.run(move |event, _, control_flow| {
    //     // The default action for the next frame is to wait.
    //     // let next_frame_time =
    //     //     std::time::Instant::now() + std::time::Duration::from_nanos(16_666_667);
    //     // *control_flow = glutin::event_loop::ControlFlow::WaitUntil(next_frame_time);
    //     *control_flow = glutin::event_loop::ControlFlow::Poll;
    //
    //     match event {
    //         glutin::event::Event::WindowEvent { event, .. } => match event {
    //             glutin::event::WindowEvent::CloseRequested => {
    //                 *control_flow = glutin::event_loop::ControlFlow::Exit;
    //             }
    //             _ => {
    //                 debugger.on_event(&event);
    //             }
    //         },
    //
    //         glutin::event::Event::MainEventsCleared => {
    //             debugger.update(&display);
    //
    //             let mut frame = display.draw();
    //             frame.clear_color(0.0, 0.0, 0.0, 1.0);
    //             debugger.draw(&display, &mut frame);
    //             frame.finish().unwrap();
    //         }
    //
    //         _ => {}
    //     }
    // })
}
