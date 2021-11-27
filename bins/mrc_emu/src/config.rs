use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(name = "My Rusty Computer")]
pub struct Config {
    /// Path to an optional BIOS binary to load when the emulator starts
    pub bios: Option<String>,
}
