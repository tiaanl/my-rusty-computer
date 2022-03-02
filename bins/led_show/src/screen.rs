use glium::{implement_vertex, uniform, Surface};
use std::io::Cursor;

#[derive(Copy, Clone)]
struct Vertex {
    position: [f32; 2],
    tex_coords: [f32; 2],
}

implement_vertex!(Vertex, position, tex_coords);

pub struct Screen {
    texture: glium::texture::SrgbTexture2d,
    vertex_buffer: glium::VertexBuffer<Vertex>,
    indices: glium::index::NoIndices,
    program: glium::Program,
}

impl Screen {
    pub fn new(display: &impl glium::backend::Facade) -> Self {
        let image = image::load(
            Cursor::new(&include_bytes!("../assets/led.png")),
            image::ImageFormat::Png,
        )
        .unwrap()
        .to_rgba8();
        let image_dimensions = image.dimensions();
        let image =
            glium::texture::RawImage2d::from_raw_rgba_reversed(&image.into_raw(), image_dimensions);
        let texture = glium::texture::SrgbTexture2d::new(display, image).unwrap();

        let shape = vec![
            Vertex {
                position: [-0.5, -0.5],
                tex_coords: [0.0, 0.0],
            },
            Vertex {
                position: [0.5, -0.5],
                tex_coords: [1.0, 0.0],
            },
            Vertex {
                position: [0.5, 0.5],
                tex_coords: [1.0, 1.0],
            },
            Vertex {
                position: [-0.5, 0.5],
                tex_coords: [0.0, 1.0],
            },
        ];

        let vertex_buffer = glium::VertexBuffer::new(display, &shape).unwrap();
        let indices = glium::index::NoIndices(glium::index::PrimitiveType::Points);

        let vertex_shader_src = r#"
            #version 330 core
            in vec2 position;
            void main() {
                gl_Position = vec4(position, 0.0, 1.0);
            }
        "#;

        let geometry_shader_src = r#"
            #version 330 core

            layout (points) in;
            layout (triangle_strip, max_vertices = 4) out;

            in VS_OUT {
                vec2 tex_coord;
            } vs_out[];

            out vec2 tex_coord;

            void main() {
                float block_size = 1.0 / 8.0;

                gl_Position = gl_in[0].gl_Position - vec4(-block_size, -block_size, 0.0, 0.0);
                tex_coord = vec2(0.0, 1.0);
                EmitVertex();

                gl_Position = gl_in[0].gl_Position - vec4(-block_size, block_size, 0.0, 0.0);
                tex_coord = vec2(0.0, 0.0);
                EmitVertex();

                gl_Position = gl_in[0].gl_Position - vec4( block_size, -block_size, 0.0, 0.0);
                tex_coord = vec2(1.0, 1.0);
                EmitVertex();

                gl_Position = gl_in[0].gl_Position - vec4( block_size, block_size, 0.0, 0.0);
                tex_coord = vec2(1.0, 0.0);
                EmitVertex();
            }
        "#;

        let fragment_shader_src = r#"
            #version 330 core
            in vec2 tex_coord;
            out vec4 color;
            uniform sampler2D tex;
            void main() {
                color = texture(tex, tex_coord);
                // color = vec4(1.0, 0.0, 0.0, 1.0);
            }
        "#;

        let program = glium::Program::from_source(
            display,
            vertex_shader_src,
            fragment_shader_src,
            Some(geometry_shader_src),
        )
        .unwrap();

        Self {
            texture,
            vertex_buffer,
            indices,
            program,
        }
    }

    pub fn draw(&self, target: &mut glium::Frame) {
        let uniforms = uniform! {
            tex: &self.texture,
        };

        target
            .draw(
                &self.vertex_buffer,
                &self.indices,
                &self.program,
                &uniforms,
                &Default::default(),
            )
            .unwrap();
    }
}
