fn inner_loop() -> Result<(), String> {
    use sdl2::event::Event;
    use sdl2::keyboard::Keycode;
    use sdl2::pixels::Color;
    use std::time::Duration;

    let context = sdl2::init()?;
    let video = context.video()?;
    let window = match video
        .window("my-rusty-computer Emulator", 640, 400)
        .position_centered()
        .build()
    {
        Ok(w) => w,
        Err(err) => return Err(format!("{}", err)),
    };

    let mut canvas = match window.into_canvas().build() {
        Ok(c) => c,
        Err(err) => return Err(format!("{}", err)),
    };

    canvas.set_draw_color(Color::RGB(0, 255, 255));
    canvas.clear();
    canvas.present();

    let mut event_pump = context.event_pump()?;

    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }

        // Render.

        canvas.present();
        std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }

    Ok(())
}

pub fn run_loop() {
    if let Err(err) = inner_loop() {
        eprintln!("{}", err);
    }
}
