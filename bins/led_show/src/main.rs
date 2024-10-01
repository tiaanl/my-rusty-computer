mod panel;
mod screen;

use crate::panel::Panel;
use glium::{glutin, Surface};
use mrc_emulator::components::pit::ProgrammableIntervalTimer8253;
use mrc_emulator::{Address, Bus};
use std::sync::{Arc, RwLock};

struct ThreadSafeBus<Inner: Bus> {
    inner: Arc<RwLock<Inner>>,
}

impl<Inner: Bus> Clone for ThreadSafeBus<Inner> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<Inner: Bus> ThreadSafeBus<Inner> {
    pub fn new(inner: Inner) -> Self {
        Self {
            inner: Arc::new(RwLock::new(inner)),
        }
    }
}

impl<Inner: Bus> Bus for ThreadSafeBus<Inner> {
    fn read(&self, address: Address) -> u8 {
        self.inner.read().unwrap().read(address)
    }

    fn write(&mut self, address: Address, value: u8) {
        self.inner.write().unwrap().write(address, value)
    }
}

struct Chipset {
    pit: ProgrammableIntervalTimer8253,
    panel: ThreadSafeBus<Panel>,
}

impl Bus for Chipset {
    fn read(&self, address: Address) -> u8 {
        match address {
            0x00 => self.panel.read(address),
            0x40..=0x43 => self.pit.read(address),
            _ => 0,
        }
    }

    fn write(&mut self, address: Address, value: u8) {
        match address {
            0x00 => self.panel.write(address, value),
            0x40..=0x43 => self.pit.write(address, value),
            _ => {}
        }
    }
}

fn main() {
    pretty_env_logger::init();

    let event_loop = glutin::event_loop::EventLoop::new();
    let wb = glutin::window::WindowBuilder::new();
    let cb = glutin::ContextBuilder::new().with_gl_profile(glutin::GlProfile::Core);
    let display = glium::Display::new(wb, cb, &event_loop).unwrap();

    println!("OpenGL version: {}", display.get_opengl_version_string());
    println!("OpenGL renderer: {}", display.get_opengl_renderer_string());
    println!("OpenGL profile: {:?}", display.get_opengl_profile());

    let mut screen = screen::Screen::new(&display);
    let panel = ThreadSafeBus::new(Panel::default());

    let cloned = panel.clone();

    std::thread::spawn(|| {
        let code = include_bytes!("../assets/main.bin");

        let mut data = mrc_emulator::components::ram::RandomAccessMemory::with_capacity(0x100000);

        for (i, &byte) in code.iter().enumerate() {
            data.write(i as u32, byte);
        }

        let io = Chipset {
            pit: ProgrammableIntervalTimer8253::default(),
            panel: cloned,
        };

        let mut cpu = mrc_emulator::cpu::CPU::new(data, io);
        cpu.start().unwrap();
        println!("Done");
    });

    event_loop.run(move |event, _, control_flow| {
        match event {
            glutin::event::Event::WindowEvent {
                event: glutin::event::WindowEvent::CloseRequested,
                ..
            } => {
                *control_flow = glutin::event_loop::ControlFlow::Exit;
                return;
            }
            glutin::event::Event::NewEvents(cause) => match cause {
                glutin::event::StartCause::ResumeTimeReached { .. } => (),
                glutin::event::StartCause::Init => (),
                _ => return,
            },
            _ => return,
        }

        let next_frame_time =
            std::time::Instant::now() + std::time::Duration::from_nanos(16_666_667);
        *control_flow = glutin::event_loop::ControlFlow::WaitUntil(next_frame_time);

        let mut target = display.draw();
        target.clear_color(0.0, 0.0, 1.0, 1.0);

        let data = { *panel.inner.read().unwrap().colors() };

        screen.draw(&mut target, &data);

        target.finish().unwrap();
    });
}
