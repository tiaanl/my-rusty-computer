use std::sync::{Arc, RwLock};

use glium::glutin::event::Event;
use glium::glutin::event_loop::EventLoop;
use glium::glutin::{event, event_loop, window, ContextBuilder};
use glium::uniforms::{MagnifySamplerFilter, MinifySamplerFilter};
use glium::{implement_vertex, uniform, Display, Program, Surface, VertexBuffer};

use mrc_emulator::bus::Address;
use mrc_emulator::BusInterface;

// const VGA_FONT: [u8; 1 * 8] = [
//     0x7e,  // .111111.
//     0x81,  // 1......1
//     0xa5,  // 1.1..1.1
//     0x81,  // 1......1
//     0xbd,  // 1.1111.1
//     0x99,  // 1..11..1
//     0x81,  // 1......1
//     0x7e,  // .111111.
// ];

// const VGA_FONT: [u8; 1 * 8] = [
//     0x3c,  // ..1111..
//     0x66,  // .11..11.
//     0xc0,  // 11......
//     0xc0,  // 11......
//     0xc0,  // 11......
//     0x66,  // .11..11.
//     0x3c,  // ..1111..
//     0x00,  // ........
// ];

// Video modes:
//
// Mode     Type        Resolution      Adapter(s)     Colors            Address
// 00h    Text        40 x 25           All but MDA    16 gray           B8000
// 01h    Text        40 x 25           All but MDA    16 fore/8 back    B8000
// 02h    Text        80 x 25           All but MDA    16 gray           B8000
// 03h    Text        80 x 25           All but MDA    16 fore/8 back    B8000
// 04h    Graphics    320 x 200         All but MDA    4                 B8000
// 05h    Graphics    320 x 200         All but MDA    4 gray            B8000
// 06h    Graphics    640 x 200         All but MDA    2                 B8000
// 07h    Text        80 x 25           MDA            EGA               B0000 (b/w)
// 08h    Graphics    160 x 200         PCjr           16                B0000
// 09h    Graphics    320 x 200         PCjr           16                B0000
// 0Ah    Graphics    640 x 200         PCjr           4                 B0000
// 0Bh    Reserved    (EGA internal)
// 0Ch    Reserved    (EGA internal)
// 0Dh    Graphics    320 x 200         EGA            16                A0000
// 0Eh    Graphics    640 x 200         EGA            16                A0000
// 0Fh    Graphics    640 x 350         EGA            b/w               A0000
// 10h    Graphics    640 x 350         EGA            16                A0000

const VGA_FONT: [u8; 128 * 8] = [
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, //   0  00  NUL
    0x7e, 0x81, 0xa5, 0x81, 0xbd, 0x99, 0x81, 0x7e, //   1  01  SOH
    0x7e, 0xff, 0xdb, 0xff, 0xc3, 0xe7, 0xff, 0x7e, //   2  02  STX
    0x6c, 0xfe, 0xfe, 0xfe, 0x7c, 0x38, 0x10, 0x00, //   3  03  ETX
    0x10, 0x38, 0x7c, 0xfe, 0x7c, 0x38, 0x10, 0x00, //   4  04  EOT
    0x38, 0x7c, 0x38, 0xfe, 0xfe, 0x7c, 0x38, 0x7c, //   5  05  ENQ
    0x10, 0x10, 0x38, 0x7c, 0xfe, 0x7c, 0x38, 0x7c, //   6  06  ACK
    0x00, 0x00, 0x18, 0x3c, 0x3c, 0x18, 0x00, 0x00, //   7  07  BEL
    0xff, 0xff, 0xe7, 0xc3, 0xc3, 0xe7, 0xff, 0xff, //   8  08  BS
    0x00, 0x3c, 0x66, 0x42, 0x42, 0x66, 0x3c, 0x00, //   9  09  HT
    0xff, 0xc3, 0x99, 0xbd, 0xbd, 0x99, 0xc3, 0xff, //  10  0A  LF
    0x0f, 0x07, 0x0f, 0x7d, 0xcc, 0xcc, 0xcc, 0x78, //  11  0B  VT
    0x3c, 0x66, 0x66, 0x66, 0x3c, 0x18, 0x7e, 0x18, //  12  0C  FF
    0x3f, 0x33, 0x3f, 0x30, 0x30, 0x70, 0xf0, 0xe0, //  13  0D  CR
    0x7f, 0x63, 0x7f, 0x63, 0x63, 0x67, 0xe6, 0xc0, //  14  0E  SO
    0x99, 0x5a, 0x3c, 0xe7, 0xe7, 0x3c, 0x5a, 0x99, //  15  0F  SI
    0x80, 0xe0, 0xf8, 0xfe, 0xf8, 0xe0, 0x80, 0x00, //  16  10  DLE
    0x02, 0x0e, 0x3e, 0xfe, 0x3e, 0x0e, 0x02, 0x00, //  17  11  DC1
    0x18, 0x3c, 0x7e, 0x18, 0x18, 0x7e, 0x3c, 0x18, //  18  12  DC2
    0x66, 0x66, 0x66, 0x66, 0x66, 0x00, 0x66, 0x00, //  19  13  DC3
    0x7f, 0xdb, 0xdb, 0x7b, 0x1b, 0x1b, 0x1b, 0x00, //  20  14  DC4
    0x3e, 0x63, 0x38, 0x6c, 0x6c, 0x38, 0xcc, 0x78, //  21  15  NAK
    0x00, 0x00, 0x00, 0x00, 0x7e, 0x7e, 0x7e, 0x00, //  22  16  SYN
    0x18, 0x3c, 0x7e, 0x18, 0x7e, 0x3c, 0x18, 0xff, //  23  17  ETB
    0x18, 0x3c, 0x7e, 0x18, 0x18, 0x18, 0x18, 0x00, //  24  18  CAN
    0x18, 0x18, 0x18, 0x18, 0x7e, 0x3c, 0x18, 0x00, //  25  19  EM
    0x00, 0x18, 0x0c, 0xfe, 0x0c, 0x18, 0x00, 0x00, //  26  1A  SUB
    0x00, 0x30, 0x60, 0xfe, 0x60, 0x30, 0x00, 0x00, //  27  1B  ESC
    0x00, 0x00, 0xc0, 0xc0, 0xc0, 0xfe, 0x00, 0x00, //  28  1C  FS
    0x00, 0x24, 0x66, 0xff, 0x66, 0x24, 0x00, 0x00, //  29  1D  GS
    0x00, 0x18, 0x3c, 0x7e, 0xff, 0xff, 0x00, 0x00, //  30  1E  RS
    0x00, 0xff, 0xff, 0x7e, 0x3c, 0x18, 0x00, 0x00, //  31  1F  US
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, //  32  20  space
    0x30, 0x78, 0x78, 0x30, 0x30, 0x00, 0x30, 0x00, //  33  21  !
    0x6c, 0x6c, 0x6c, 0x00, 0x00, 0x00, 0x00, 0x00, //  34  22  "
    0x6c, 0x6c, 0xfe, 0x6c, 0xfe, 0x6c, 0x6c, 0x00, //  35  23  #
    0x30, 0x7c, 0xc0, 0x78, 0x0c, 0xf8, 0x30, 0x00, //  36  24  $
    0x00, 0xc6, 0xcc, 0x18, 0x30, 0x66, 0xc6, 0x00, //  37  25  %
    0x38, 0x6c, 0x38, 0x76, 0xdc, 0xcc, 0x76, 0x00, //  38  26  &
    0x60, 0x60, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, //  39  27  '
    0x18, 0x30, 0x60, 0x60, 0x60, 0x30, 0x18, 0x00, //  40  28  (
    0x60, 0x30, 0x18, 0x18, 0x18, 0x30, 0x60, 0x00, //  41  29  )
    0x00, 0x66, 0x3c, 0xff, 0x3c, 0x66, 0x00, 0x00, //  42  2A  *
    0x00, 0x30, 0x30, 0xfc, 0x30, 0x30, 0x00, 0x00, //  43  2B  +
    0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x30, 0x60, //  44  2C  ,
    0x00, 0x00, 0x00, 0xfc, 0x00, 0x00, 0x00, 0x00, //  45  2D  -
    0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x30, 0x00, //  46  2E  .
    0x06, 0x0c, 0x18, 0x30, 0x60, 0xc0, 0x80, 0x00, //  47  2F  /
    0x7c, 0xc6, 0xce, 0xde, 0xf6, 0xe6, 0x7c, 0x00, //  48  30  0
    0x30, 0x70, 0x30, 0x30, 0x30, 0x30, 0xfc, 0x00, //  49  31  1
    0x78, 0xcc, 0x0c, 0x38, 0x60, 0xcc, 0xfc, 0x00, //  50  32  2
    0x78, 0xcc, 0x0c, 0x38, 0x0c, 0xcc, 0x78, 0x00, //  51  33  3
    0x1c, 0x3c, 0x6c, 0xcc, 0xfe, 0x0c, 0x1e, 0x00, //  52  34  4
    0xfc, 0xc0, 0xf8, 0x0c, 0x0c, 0xcc, 0x78, 0x00, //  53  35  5
    0x38, 0x60, 0xc0, 0xf8, 0xcc, 0xcc, 0x78, 0x00, //  54  36  6
    0xfc, 0xcc, 0x0c, 0x18, 0x30, 0x30, 0x30, 0x00, //  55  37  7
    0x78, 0xcc, 0xcc, 0x78, 0xcc, 0xcc, 0x78, 0x00, //  56  38  8
    0x78, 0xcc, 0xcc, 0x7c, 0x0c, 0x18, 0x70, 0x00, //  57  39  9
    0x00, 0x30, 0x30, 0x00, 0x00, 0x30, 0x30, 0x00, //  58  3A  :
    0x00, 0x30, 0x30, 0x00, 0x00, 0x30, 0x30, 0x60, //  59  3B  ;
    0x18, 0x30, 0x60, 0xc0, 0x60, 0x30, 0x18, 0x00, //  60  3C  <
    0x00, 0x00, 0xfc, 0x00, 0x00, 0xfc, 0x00, 0x00, //  61  3D  =
    0x60, 0x30, 0x18, 0x0c, 0x18, 0x30, 0x60, 0x00, //  62  3E  >
    0x78, 0xcc, 0x0c, 0x18, 0x30, 0x00, 0x30, 0x00, //  63  3F  ?
    0x7c, 0xc6, 0xde, 0xde, 0xde, 0xc0, 0x78, 0x00, //  64  40  @
    0x30, 0x78, 0xcc, 0xcc, 0xfc, 0xcc, 0xcc, 0x00, //  65  41  A
    0xfc, 0x66, 0x66, 0x7c, 0x66, 0x66, 0xfc, 0x00, //  66  42  B
    0x3c, 0x66, 0xc0, 0xc0, 0xc0, 0x66, 0x3c, 0x00, //  67  43  C
    0xf8, 0x6c, 0x66, 0x66, 0x66, 0x6c, 0xf8, 0x00, //  68  44  D
    0xfe, 0x62, 0x68, 0x78, 0x68, 0x62, 0xfe, 0x00, //  69  45  E
    0xfe, 0x62, 0x68, 0x78, 0x68, 0x60, 0xf0, 0x00, //  70  46  F
    0x3c, 0x66, 0xc0, 0xc0, 0xce, 0x66, 0x3e, 0x00, //  71  47  G
    0xcc, 0xcc, 0xcc, 0xfc, 0xcc, 0xcc, 0xcc, 0x00, //  72  48  H
    0x78, 0x30, 0x30, 0x30, 0x30, 0x30, 0x78, 0x00, //  73  49  I
    0x1e, 0x0c, 0x0c, 0x0c, 0xcc, 0xcc, 0x78, 0x00, //  74  4A  J
    0xe6, 0x66, 0x6c, 0x78, 0x6c, 0x66, 0xe6, 0x00, //  75  4B  K
    0xf0, 0x60, 0x60, 0x60, 0x62, 0x66, 0xfe, 0x00, //  76  4C  L
    0xc6, 0xee, 0xfe, 0xfe, 0xd6, 0xc6, 0xc6, 0x00, //  77  4D  M
    0xc6, 0xe6, 0xf6, 0xde, 0xce, 0xc6, 0xc6, 0x00, //  78  4E  N
    0x38, 0x6c, 0xc6, 0xc6, 0xc6, 0x6c, 0x38, 0x00, //  79  4F  O
    0xfc, 0x66, 0x66, 0x7c, 0x60, 0x60, 0xf0, 0x00, //  80  50  P
    0x78, 0xcc, 0xcc, 0xcc, 0xdc, 0x78, 0x1c, 0x00, //  81  51  Q
    0xfc, 0x66, 0x66, 0x7c, 0x6c, 0x66, 0xe6, 0x00, //  82  52  R
    0x78, 0xcc, 0xe0, 0x70, 0x1c, 0xcc, 0x78, 0x00, //  83  53  S
    0xfc, 0xb4, 0x30, 0x30, 0x30, 0x30, 0x78, 0x00, //  84  54  T
    0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xfc, 0x00, //  85  55  U
    0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0x78, 0x30, 0x00, //  86  56  V
    0xc6, 0xc6, 0xc6, 0xd6, 0xfe, 0xee, 0xc6, 0x00, //  87  57  W
    0xc6, 0xc6, 0x6c, 0x38, 0x38, 0x6c, 0xc6, 0x00, //  88  58  X
    0xcc, 0xcc, 0xcc, 0x78, 0x30, 0x30, 0x78, 0x00, //  89  59  Y
    0xfe, 0xc6, 0x8c, 0x18, 0x32, 0x66, 0xfe, 0x00, //  90  5A  Z
    0x78, 0x60, 0x60, 0x60, 0x60, 0x60, 0x78, 0x00, //  91  5B  [
    0xc0, 0x60, 0x30, 0x18, 0x0c, 0x06, 0x02, 0x00, //  92  5C  \
    0x78, 0x18, 0x18, 0x18, 0x18, 0x18, 0x78, 0x00, //  93  5D  ]
    0x10, 0x38, 0x6c, 0xc6, 0x00, 0x00, 0x00, 0x00, //  94  5E  ^
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, //  95  5F  _
    0x30, 0x30, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, //  96  60  `
    0x00, 0x00, 0x78, 0x0c, 0x7c, 0xcc, 0x76, 0x00, //  97  61  a
    0xe0, 0x60, 0x60, 0x7c, 0x66, 0x66, 0xdc, 0x00, //  98  62  b
    0x00, 0x00, 0x78, 0xcc, 0xc0, 0xcc, 0x78, 0x00, //  99  63  c
    0x1c, 0x0c, 0x0c, 0x7c, 0xcc, 0xcc, 0x76, 0x00, // 100  64  d
    0x00, 0x00, 0x78, 0xcc, 0xfc, 0xc0, 0x78, 0x00, // 101  65  e
    0x38, 0x6c, 0x60, 0xf0, 0x60, 0x60, 0xf0, 0x00, // 102  66  f
    0x00, 0x00, 0x76, 0xcc, 0xcc, 0x7c, 0x0c, 0xf8, // 103  67  g
    0xe0, 0x60, 0x6c, 0x76, 0x66, 0x66, 0xe6, 0x00, // 104  68  h
    0x30, 0x00, 0x70, 0x30, 0x30, 0x30, 0x78, 0x00, // 105  69  i
    0x0c, 0x00, 0x0c, 0x0c, 0x0c, 0xcc, 0xcc, 0x78, // 106  6A  j
    0xe0, 0x60, 0x66, 0x6c, 0x78, 0x6c, 0xe6, 0x00, // 107  6B  k
    0x70, 0x30, 0x30, 0x30, 0x30, 0x30, 0x78, 0x00, // 108  6C  l
    0x00, 0x00, 0xcc, 0xfe, 0xfe, 0xd6, 0xc6, 0x00, // 109  6D  m
    0x00, 0x00, 0xf8, 0xcc, 0xcc, 0xcc, 0xcc, 0x00, // 110  6E  n
    0x00, 0x00, 0x78, 0xcc, 0xcc, 0xcc, 0x78, 0x00, // 111  6F  o
    0x00, 0x00, 0xdc, 0x66, 0x66, 0x7c, 0x60, 0xf0, // 112  70  p
    0x00, 0x00, 0x76, 0xcc, 0xcc, 0x7c, 0x0c, 0x1e, // 113  71  q
    0x00, 0x00, 0xdc, 0x76, 0x66, 0x60, 0xf0, 0x00, // 114  72  r
    0x00, 0x00, 0x7c, 0xc0, 0x78, 0x0c, 0xf8, 0x00, // 115  73  s
    0x10, 0x30, 0x7c, 0x30, 0x30, 0x34, 0x18, 0x00, // 116  74  t
    0x00, 0x00, 0xcc, 0xcc, 0xcc, 0xcc, 0x76, 0x00, // 117  75  u
    0x00, 0x00, 0xcc, 0xcc, 0xcc, 0x78, 0x30, 0x00, // 118  76  v
    0x00, 0x00, 0xc6, 0xd6, 0xfe, 0xfe, 0x6c, 0x00, // 119  77  w
    0x00, 0x00, 0xc6, 0x6c, 0x38, 0x6c, 0xc6, 0x00, // 120  78  x
    0x00, 0x00, 0xcc, 0xcc, 0xcc, 0x7c, 0x0c, 0xf8, // 121  79  y
    0x00, 0x00, 0xfc, 0x98, 0x30, 0x64, 0xfc, 0x00, // 122  7A  z
    0x1c, 0x30, 0x30, 0xe0, 0x30, 0x30, 0x1c, 0x00, // 123  7B  {
    0x18, 0x18, 0x18, 0x00, 0x18, 0x18, 0x18, 0x00, // 124  7C  |
    0xe0, 0x30, 0x30, 0x1c, 0x30, 0x30, 0xe0, 0x00, // 125  7D  }
    0x76, 0xdc, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // 126  7E  ~
    0x00, 0x10, 0x38, 0x6c, 0xc6, 0xc6, 0xfe, 0x00, // 127  7F  DEL
];

#[derive(Copy, Clone, Default)]
struct Character {
    letter: u32,
    color: u32,
}

const TEXT_MODE_BUFFER_MAX_SIZE: usize = 80 * 25;

pub struct TextMode {
    size: (u8, u8),
    cursor_position: (u8, u8),
    buffer: Vec<Character>,
}

impl Default for TextMode {
    fn default() -> Self {
        Self {
            size: (80, 25),
            cursor_position: (0, 0),
            buffer: vec![Character::default(); TEXT_MODE_BUFFER_MAX_SIZE],
        }
    }
}

impl TextMode {
    pub fn set_cursor_position(&mut self, x: u8, y: u8) {
        self.cursor_position = (x, y);
    }

    pub fn teletype_output(&mut self, character: u8) {
        let index = self.index_for(self.cursor_position);
        if let Some(c) = self.buffer.get_mut(index) {
            match character {
                0x0D => {
                    // Carriage return
                    self.cursor_position.0 = 0;
                }

                0x0A => {
                    // Line feed
                    self.cursor_position.1 += 1;
                }

                _ => {
                    c.letter = character as u32;
                    self.cursor_position.0 += 1;
                    if self.cursor_position.0 >= self.size.0 {
                        self.cursor_position.0 = 0;
                        self.cursor_position.1 += 1;
                        // TODO: Scroll if we go off the bottom of the screen.
                    }
                }
            }
        }
    }

    fn index_for(&self, position: (u8, u8)) -> usize {
        position.1 as usize * 80usize + position.0 as usize
    }
}

pub struct Screen {
    display: Display,
    program: Program,
    vertex_buffer: Option<VertexBuffer<Character>>,
    texture: glium::Texture2d,

    text_mode: Arc<RwLock<TextMode>>,
}

impl Screen {
    pub fn new(event_loop: &EventLoop<()>) -> Self {
        let wb = window::WindowBuilder::new().with_title("My Rusty Computer - Emulator");
        let cb = ContextBuilder::new().with_vsync(true);
        let display = Display::new(wb, cb, event_loop).unwrap();

        println!("OpenGL version: {}", display.get_opengl_version_string());
        println!("OpenGL renderer: {}", display.get_opengl_renderer_string());
        println!("OpenGL profile: {:?}", display.get_opengl_profile());

        let program = glium::Program::from_source(
            &display,
            VERTEX_SHADER_SRC,
            FRAGMENT_SHADER_SRC,
            Some(GEOMETRY_SHADER_SRC),
        )
        .unwrap();

        let texture = create_texture(&display);

        Self {
            display,
            program,
            vertex_buffer: None,
            texture,
            text_mode: Arc::new(RwLock::new(TextMode::default())),
        }
    }

    pub fn text_mode(&self) -> Arc<RwLock<TextMode>> {
        self.text_mode.clone()
    }
}

implement_vertex!(Character, letter, color);

const VERTEX_SHADER_SRC: &str = r#"
    #version 330 core

    layout (location = 0) in uint letter;
    layout (location = 1) in uint color;

    out VS_OUT {
        uint letter;
        uint color;
        ivec2 screen_pos;
    } vs_out;

    uniform int u_screen_width;
    uniform int u_screen_height;

    void main() {
        float x = gl_VertexID % u_screen_width;
        float y = gl_VertexID / u_screen_width;
        gl_Position = vec4(x * 5.0, y * 8.0, 0.0, 1.0);

        vs_out.letter = letter;
        vs_out.color = color;
        vs_out.screen_pos = ivec2(gl_VertexID % u_screen_width, gl_VertexID / 80);
    }
    "#;

const GEOMETRY_SHADER_SRC: &str = r#"
    #version 330 core

    layout (points) in;
    layout (triangle_strip, max_vertices = 4) out;

    float COLORS[] = float[](
        0.0,   0.0,   0.0,        // black
        0.0,   0.0,   170.0,      // blue
        0.0,   170.0, 0.0,        // green
        0.0,   170.0, 170.0,      // cyan
        170.0, 0.0,   0.0,        // red
        170.0, 0.0,   170.0,      // magenta
        170.0, 85.0,  0.0,        // yellow / brown
        170.0, 170.0, 170.0,      // white / light gray
        85.0,  85.0,  85.0,       // dark gray / bright black
        85.0,  85.0,  255.0,      // bright blue
        85.0,  255.0, 85.0,       // bright green
        85.0,  255.0, 255.0,      // bright cyan
        255.0, 85.0,  85.0,       // bright red
        255.0, 85.0,  255.0,      // bright magenta
        255.0, 255.0, 85.0,       // bright yellow
        255.0, 255.0, 255.0       // bright white
    );

    in VS_OUT {
        uint letter;
        uint color;
        ivec2 screen_pos;
    } gs_in[];

    out vec2 tex_coord;
    out vec4 back_color;

    uniform mat4 matrix;

    void emit(vec4 position, mat4 matrix, uint letter, vec2 adjust) {
        vec4 pos = position + vec4(adjust * vec2(5.0, 8.0), 0.0, 0.0);
        gl_Position = matrix * pos;
        tex_coord = vec2(letter % 16u, letter / 16u) + adjust;
        tex_coord = tex_coord * 8.0 / vec2(128.0, 64.0);
        EmitVertex();
    }

    void main() {
        // back_color = gs_in[0].color;

        uint idx = (gs_in[0].color % 16u) * 4u;
        back_color = vec4(COLORS[idx + 0u], COLORS[idx + 1u], COLORS[idx + 2u], COLORS[idx + 3u]);

        emit(gl_in[0].gl_Position, matrix, gs_in[0].letter, vec2(0.0, 0.0));
        emit(gl_in[0].gl_Position, matrix, gs_in[0].letter, vec2(1.0, 0.0));
        emit(gl_in[0].gl_Position, matrix, gs_in[0].letter, vec2(0.0, 1.0));
        emit(gl_in[0].gl_Position, matrix, gs_in[0].letter, vec2(1.0, 1.0));

        EndPrimitive();
    }
    "#;

const FRAGMENT_SHADER_SRC: &str = r#"
    #version 330 core

    in vec2 tex_coord;
    in vec4 back_color;

    uniform sampler2D u_texture;

    out vec4 color;

    void main() {
        color = texture(u_texture, tex_coord);
        // color = back_color;
    }
    "#;

fn orthogonal_projection(
    left: f32,
    right: f32,
    top: f32,
    bottom: f32,
    near: f32,
    far: f32,
) -> [[f32; 4]; 4] {
    [
        [2.0 / (right - left), 0.0, 0.0, 0.0],
        [0.0, 2.0 / (top - bottom), 0.0, 0.0],
        [0.0, 0.0, -2.0 / (far - near), 0.0],
        [
            -(right + left) / (right - left),
            -(top + bottom) / (top - bottom),
            -(far + near) / (far - near),
            1.0,
        ],
    ]
}

fn create_texture(display: &Display) -> glium::Texture2d {
    let cols_per_char = 8usize; // width
    let rows_per_char = 8usize; // height
    let chars_per_row = 16usize;
    let chars_per_column = 8usize;
    let bytes_per_pixel = 4usize;

    let texture_width = cols_per_char * chars_per_row;
    let texture_height = rows_per_char * chars_per_column;

    let stride = texture_width * bytes_per_pixel;

    let mut converted = vec![0u8; texture_width * texture_height * bytes_per_pixel];

    let mut set_pixel = |x, y, color| {
        let idx = y * stride + x * bytes_per_pixel;
        converted[idx] = color;
        converted[idx + 1] = color;
        converted[idx + 2] = color;
        converted[idx + 3] = 255;
    };

    for y in 0..8 {
        for x in 0..16 {
            for char_row in 0..rows_per_char {
                let row_char = VGA_FONT[(y * chars_per_row + x) * rows_per_char + char_row];
                for bit in 0..=7 {
                    let white = row_char & 1 << (7 - bit) != 0;
                    let color = if white { 255 } else { 0 };
                    set_pixel(x * cols_per_char + bit, y * rows_per_char + char_row, color);
                }
            }
        }
    }

    let image = glium::texture::RawImage2d::from_raw_rgba(
        converted,
        (texture_width as u32, texture_height as u32),
    );
    glium::Texture2d::new(display, image).unwrap()
}

impl Screen {
    pub fn handle_events(&self, event: &Event<()>) -> Option<event_loop::ControlFlow> {
        match event {
            event::Event::WindowEvent {
                event: event::WindowEvent::CloseRequested,
                ..
            } => Some(event_loop::ControlFlow::Exit),

            _ => None,
        }
    }

    pub fn tick(&mut self) {
        let indices = glium::index::NoIndices(glium::index::PrimitiveType::Points);

        let mut _screen_size = (0i32, 0i32);

        {
            let text_mode = self.text_mode.read().unwrap();

            _screen_size = (text_mode.size.0 as i32, text_mode.size.1 as i32);

            match self.vertex_buffer {
                Some(ref vertex_buffer) => vertex_buffer.write(text_mode.buffer.as_slice()),
                None => {
                    self.vertex_buffer = Some(
                        VertexBuffer::new(&self.display, text_mode.buffer.as_slice()).unwrap(),
                    );
                }
            }
        }

        let mut target = self.display.draw();
        target.clear_color(0.0, 0.0, 1.0, 1.0);

        let behavior = glium::uniforms::SamplerBehavior {
            minify_filter: MinifySamplerFilter::Nearest,
            magnify_filter: MagnifySamplerFilter::Nearest,
            ..Default::default()
        };
        let uniforms = uniform! {
            matrix: orthogonal_projection(0.0, 320.0, 0.0, 200.0, 1.0, -1.0),
            u_screen_width: _screen_size.0,
            u_screen_height: _screen_size.1,
            u_texture: glium::uniforms::Sampler(&self.texture, behavior),
        };

        target
            .draw(
                self.vertex_buffer.as_ref().unwrap(),
                &indices,
                &self.program,
                &uniforms,
                &Default::default(),
            )
            .unwrap();

        target.finish().unwrap();
    }
}

pub struct TextModeInterface {
    text_mode: Arc<RwLock<TextMode>>,
}

impl TextModeInterface {
    pub fn new(text_mode: Arc<RwLock<TextMode>>) -> Self {
        Self { text_mode }
    }
}

impl BusInterface for TextModeInterface {
    fn read(&self, address: Address) -> mrc_emulator::error::Result<u8> {
        let text_mode = self.text_mode.read().unwrap();

        let index = address as usize / 2;
        let value = if address % 2 == 0 {
            text_mode.buffer[index].letter as u8
        } else {
            text_mode.buffer[index].color as u8
        };

        // log::info!("Read \"{:02X}\" from VGA memory at [{:05X}]", value, address);

        Ok(value)
    }

    fn write(&mut self, address: Address, value: u8) -> mrc_emulator::error::Result<()> {
        let mut text_mode = self.text_mode.write().unwrap();

        let index = address as usize / 2;
        if address % 2 == 0 {
            text_mode.buffer[index].letter = value as u32;
        } else {
            text_mode.buffer[index].color = value as u32;
        }

        // log::info!("Wrote \"{:02X}\" to VGA memory at [{:05X}] ", value, address);

        Ok(())
    }
}

/*
impl InterruptHandler for TextModeInterface {
    fn handle(&mut self, cpu: &CPU) {
        let ah = cpu.state.get_byte_register_value(Register::AhSp);
        match ah {
            0x00 => {
                // AL = video mode

                let _video_mode = cpu.state.get_byte_register_value(Register::AlAx);

                // log::info!("Setting video mode. | video_mode: {:02X}", video_mode);
            }

            0x01 => {
                // Set text mode cursor shape.
                // CH = scan row start
                // CL = scan row end

                let _scan_line_start = cpu.state.get_byte_register_value(Register::ChBp);
                let _scan_line_end = cpu.state.get_byte_register_value(Register::ClCx);

                // log::info!(
                //     "Setting cursor shape: {:02X}..{:02X}",
                //     scan_line_start,
                //     scan_line_end
                // );
            }

            0x02 => {
                // BH = page number
                // DH = row
                // DL = column
                let _page_number = cpu.state.get_byte_register_value(Register::BhDi);
                let row = cpu.state.get_byte_register_value(Register::DhSi);
                let column = cpu.state.get_byte_register_value(Register::DlDx);

                // log::info!("Set cursor position. | page_number: {:02X} | row: {:02X} | column: {:02X}", page_number, row, column);

                self.text_mode.borrow_mut().cursor_position = (column, row);
            }

            0x05 => {
                // Select active display page
                // AL = page number

                let _page_number = cpu.state.get_byte_register_value(Register::AlAx);

                // log::info!(
                //     "Select active display page. | page_number: {:02X}",
                //     page_number
                // );
            }

            0x06 => {
                // Scroll up window.
                // AL = lines to scroll (0 = clear, CH, CL, DH, Dl are used)
                // BH = background color and foreground color
                // CH = upper row number
                // CL = left column number
                // DH = lower row number
                // DL = right column number

                let _lines_to_scroll = cpu.state.get_byte_register_value(Register::AlAx);
                let _color = cpu.state.get_byte_register_value(Register::BhDi);
                let _upper_row_number = cpu.state.get_byte_register_value(Register::ChBp);
                let _left_column_number = cpu.state.get_byte_register_value(Register::ClCx);
                let _lower_row_number = cpu.state.get_byte_register_value(Register::DhSi);
                let _right_column_number = cpu.state.get_byte_register_value(Register::DlDx);

                // log::info!("Scroll up window. | lines_to_scroll: {:02X} | color: {:02X} | upper_row_number: {:02X} | left_column_number: {:02X} | lower_row_number: {:02X} | right_column_number: {:02X}", lines_to_scroll, color, upper_row_number, left_column_number, lower_row_number, right_column_number);
            }

            0x09 => {
                // AL = character
                // BH = page number
                // BL = color
                // CX = number of times to print character
                let character = cpu.state.get_byte_register_value(Register::AlAx);
                let _page_number = cpu.state.get_byte_register_value(Register::BhDi);
                let _color = cpu.state.get_byte_register_value(Register::BlBx);
                let _count = cpu.state.get_word_register_value(Register::ClCx);

                // log::info!("Write character and attribute at cursor position. | character: {:02X} \"{}\" | page_number: {:02X} | color: {:02X} | count: {:04X}",
                //     character, character as char, page_number, color, count);

                {
                    let mut text_mode = self.text_mode.borrow_mut();

                    let index = text_mode.cursor_position.1 * text_mode.size.0
                        + text_mode.cursor_position.0;

                    if let Some(c) = text_mode.buffer.get_mut(index as usize) {
                        c.letter = character as u32;
                    }
                }
            }

            0x0E => {
                // Teletype output
                // AL = character
                // BH = page_number
                // BL = color (only in graphic mode)

                let character = cpu.state.get_byte_register_value(Register::AlAx);
                let _page_number = cpu.state.get_byte_register_value(Register::BhDi);
                let _color = cpu.state.get_byte_register_value(Register::BlBx);

                // log::info!(
                //     "Teletype output. | character: {:02X} | page_number: {:02X} | color: {:02X}",
                //     character,
                //     page_number,
                //     color
                // );

                self.text_mode.borrow_mut().teletype_output(character);
            }

            0x12 => {
                // Some undocumented function??
                // log::info!("Undocumented int 0x10 function.");
            }

            _ => panic!("Invalid function number: {:02X}", ah),
        }
    }
}
*/
