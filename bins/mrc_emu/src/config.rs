use clap::{App, Arg};

const BIOS_ARG: &'static str = "bios";

pub struct Config {
    pub bios_path: Option<String>,
}

impl<'a, 'b> Config {
    fn create_app() -> App<'a, 'b> {
        App::new("mrc-emu").version("0.0.1").arg(
            Arg::with_name(BIOS_ARG)
                .help("Specify a file to use as the BIOS")
                .required(false),
        )
    }
}

impl Default for Config {
    fn default() -> Self {
        let matches = Config::create_app().get_matches();
        Self {
            bios_path: matches
                .value_of(BIOS_ARG)
                .map(|bios_path| bios_path.to_string()),
        }
    }
}
