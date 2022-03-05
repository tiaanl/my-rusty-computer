mod panel;
mod screen;

use crate::panel::Panel;
use glium::{glutin, Surface};
use mrc_emulator::{
    error::{Error, Result},
    Bus, Port,
};
use std::marker::PhantomData;
use std::sync::{Arc, RwLock};

struct ThreadSafeBus<Inner: Bus<Size>, Size> {
    inner: Arc<RwLock<Inner>>,
    phantom_data: PhantomData<Size>,
}

impl<Inner: Bus<Size>, Size> Clone for ThreadSafeBus<Inner, Size> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            phantom_data: self.phantom_data,
        }
    }
}

impl<Inner: Bus<Size>, Size> ThreadSafeBus<Inner, Size> {
    pub fn new(inner: Inner) -> Self {
        Self {
            inner: Arc::new(RwLock::new(inner)),
            phantom_data: PhantomData::default(),
        }
    }
}

impl<Inner: Bus<Size>, Size> Bus<Size> for ThreadSafeBus<Inner, Size> {
    fn read(&self, address: Size) -> Result<u8> {
        self.inner.read().unwrap().read(address)
    }

    fn write(&mut self, address: Size, value: u8) -> Result<()> {
        self.inner.write().unwrap().write(address, value)
    }
}

struct Io {
    panel: ThreadSafeBus<Panel, Port>,
}

impl Bus<Port> for Io {
    fn read(&self, address: Port) -> Result<u8> {
        if address == 0x00 {
            self.panel.read(address)
        } else {
            Err(Error::InvalidPort(address))
        }
    }

    fn write(&mut self, address: Port, value: u8) -> Result<()> {
        if address == 0x00 {
            self.panel.write(address, value)
        } else {
            Err(Error::InvalidPort(address))
        }
    }
}

fn main() {
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
        for i in 0..code.len() {
            data.write(i as u32, code[i]).unwrap();
        }

        let io = Io { panel: cloned };

        let mut cpu = mrc_emulator::cpu::CPU::new(data, io);
        cpu.start().unwrap();
        println!("Done");
    });

    event_loop.run(move |event, _, control_flow| {
        match event {
            glutin::event::Event::WindowEvent { event, .. } => match event {
                glutin::event::WindowEvent::CloseRequested => {
                    *control_flow = glutin::event_loop::ControlFlow::Exit;
                    return;
                }
                _ => return,
            },
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

        let data = { panel.inner.read().unwrap().colors().clone() };

        screen.draw(&mut target, &data);

        target.finish().unwrap();
    });
}
