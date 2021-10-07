use glium::{Display, implement_vertex, Surface, uniform};
use glium::glutin::{ContextBuilder, event, event_loop, window};

#[derive(Copy, Clone)]
struct Vertex {
    position: [f32; 2],
    tex_coords: [f32; 2],
}

implement_vertex!(Vertex, position, tex_coords);

const VERTEX_SHADER_SRC: &str = r#"
        #version 140
        in vec2 position;
        in vec2 tex_coords;
        out vec2 v_tex_coords;
        uniform mat4 matrix;
        void main() {
            v_tex_coords = tex_coords;
            gl_Position = matrix * vec4(position, 0.0, 1.0);
        }
    "#;

const FRAGMENT_SHADER_SRC: &str = r#"
        #version 140
        in vec2 v_tex_coords;
        out vec4 color;
        // uniform sampler2D tex;
        void main() {
            // color = texture(tex, v_tex_coords);
            color = vec4(1.0, 0.0, 1.0, 1.0);
        }
    "#;

pub struct Monitor {}

impl Monitor {
    pub fn _start(&mut self) {
        let event_loop = event_loop::EventLoop::new();
        let wb = window::WindowBuilder::new().with_title("My Rusty Computer - Emulator");
        let cb = ContextBuilder::new().with_vsync(false);
        let display = Display::new(wb, cb, &event_loop).unwrap();

        let vertex1 = Vertex {
            position: [-0.5, -0.5],
            tex_coords: [0.0, 0.0],
        };
        let vertex2 = Vertex {
            position: [0.0, 0.5],
            tex_coords: [0.0, 1.0],
        };
        let vertex3 = Vertex {
            position: [0.5, -0.25],
            tex_coords: [1.0, 0.0],
        };
        let shape = vec![vertex1, vertex2, vertex3];

        let vertex_buffer = glium::VertexBuffer::new(&display, &shape).unwrap();
        let indices = glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList);

        let program =
            glium::Program::from_source(&display, VERTEX_SHADER_SRC, FRAGMENT_SHADER_SRC, None)
                .unwrap();

        let mut t = -0.5;

        event_loop.run(move |event, _, control_flow| {
            match &event {
                event::Event::WindowEvent { event, .. } => match event {
                    event::WindowEvent::CloseRequested => {
                        *control_flow = event_loop::ControlFlow::Exit;
                        return;
                    }
                    _ => return,
                },
                event::Event::NewEvents(cause) => match cause {
                    event::StartCause::ResumeTimeReached { .. } => (),
                    event::StartCause::Init => (),
                    _ => return,
                },
                _ => return,
            }

            let next_frame_time =
                std::time::Instant::now() + std::time::Duration::from_nanos(16_666_667);
            *control_flow = event_loop::ControlFlow::WaitUntil(next_frame_time);

            // we update `t`
            t += 0.002;
            if t > 0.5 {
                t = -0.5;
            }

            let mut target = display.draw();
            target.clear_color(0.0, 0.0, 1.0, 1.0);

            let uniforms = uniform! {
                matrix: [
                    [1.0, 0.0, 0.0, 0.0],
                    [0.0, 1.0, 0.0, 0.0],
                    [0.0, 0.0, 1.0, 0.0],
                    [t, 0.0, 0.0, 1.0f32],
                ],
                // tex: &texture,
            };

            target
                .draw(
                    &vertex_buffer,
                    &indices,
                    &program,
                    &uniforms,
                    &Default::default(),
                )
                .unwrap();

            target.finish().unwrap();
        });
    }
}
