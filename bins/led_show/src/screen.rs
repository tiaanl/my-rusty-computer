use glium::{implement_vertex, uniform, Surface};
use std::io::Cursor;

#[derive(Copy, Clone)]
struct Vertex {
    position: [f32; 3],
}

implement_vertex!(Vertex, position);

pub struct Screen {
    texture: glium::texture::SrgbTexture2d,
    vertex_buffer: glium::VertexBuffer<Vertex>,
    indices: glium::index::NoIndices,
    program: glium::Program,

    dots: Vec<Vertex>,
}

impl Screen {
    pub fn new(display: &impl glium::backend::Facade) -> Self {
        let image = image::load(
            Cursor::new(&include_bytes!("../assets/leds.png")),
            image::ImageFormat::Png,
        )
        .unwrap()
        .to_rgba8();
        let image_dimensions = image.dimensions();
        let image =
            glium::texture::RawImage2d::from_raw_rgba_reversed(&image.into_raw(), image_dimensions);
        let texture = glium::texture::SrgbTexture2d::new(display, image).unwrap();

        let mut dots = Vec::new();
        for y in 0..4 {
            for x in 0..4 {
                dots.push(Vertex {
                    position: [x as f32 / 2.0 - 0.75, ((y as f32 / 2.0) - 0.75) * -1.0, 0.0],
                });
            }
        }

        let vertex_buffer = glium::VertexBuffer::dynamic(display, &dots).unwrap();
        let indices = glium::index::NoIndices(glium::index::PrimitiveType::Points);

        let vertex_shader_src = r#"
            #version 330 core

            in vec3 position;

            void main() {
                gl_Position = vec4(position, 1.0);
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
                float tex_block = 1.0 / 4.0;
                float block_size = 1.0 / 4.0;

                float p = gl_in[0].gl_Position.z;

                float x = float(uint(p) % uint(4)) / 4.0;
                float y = 1.0 - float(uint(p) / uint(4)) / 4.0;

                vec4 pos = vec4(gl_in[0].gl_Position.x, gl_in[0].gl_Position.y, 0.0, 1.0);

                gl_Position = pos - vec4(-block_size, -block_size, 0.0, 0.0);
                tex_coord = vec2(x + tex_block, y);
                EmitVertex();

                gl_Position = pos - vec4(-block_size, block_size, 0.0, 0.0);
                tex_coord = vec2(x + tex_block, y - tex_block);
                EmitVertex();

                gl_Position = pos - vec4(block_size, -block_size, 0.0, 0.0);
                tex_coord = vec2(x, y);
                EmitVertex();

                gl_Position = pos - vec4(block_size, block_size, 0.0, 0.0);
                tex_coord = vec2(x, y - tex_block);
                EmitVertex();
            }
        "#;

        let fragment_shader_src = r#"
            #version 330 core
            in vec2 tex_coord;
            out vec4 frag_color;
            uniform sampler2D tex;
            void main() {
                frag_color = texture(tex, tex_coord);
                // frag_color = vec4(1.0, 0.0, 0.0, 1.0);
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
            dots,
        }
    }

    pub fn draw(&mut self, target: &mut glium::Frame, colors: &[u8; 16]) {
        for (i, col) in colors.iter().enumerate() {
            self.dots[i].position[2] = *col as f32;
        }

        self.vertex_buffer.write(self.dots.as_slice());

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
