use serde::Deserialize;
use std::io::{Error, Read};
use std::path::Path;

#[derive(Debug)]
#[allow(unused)]
pub enum ConfigError {
    Io(std::io::Error),
    Deserialize(toml::de::Error),
}

pub struct Config {
    pub config_file_path: std::path::PathBuf,

    /// Path to an optional BIOS binary to load when the emulator starts.
    pub bios: Option<String>,

    /// Initial debugger window position.
    pub debugger_position: [i32; 2],
}

impl Config {
    pub fn load() -> Result<Self, ConfigError> {
        let mut config = Self {
            config_file_path: {
                let config_file_path =
                    Path::new(&dirs::home_dir().expect("Could not detect home directory!"))
                        .join(".mrc");
                std::fs::create_dir_all(&config_file_path)?;
                config_file_path.join("config.toml")
            },
            bios: None,
            debugger_position: [100, 100],
        };

        config.load_config_file()?;
        config.load_command_line()?;

        Ok(config)
    }
}

#[derive(clap::Parser)]
struct CommandLine {
    /// Path to an optional BIOS binary to load when the emulator starts
    pub bios: Option<String>,
}

#[derive(Debug, Deserialize)]
struct EmulatorConfig {
    bios: Option<String>,
}

#[derive(Debug, Deserialize)]
struct DebuggerConfig {
    left: Option<i32>,
    top: Option<i32>,
}

#[derive(Debug, Deserialize)]
struct ConfigFile {
    emulator: Option<EmulatorConfig>,
    debugger: Option<DebuggerConfig>,
}

impl Config {
    fn load_config_file(&mut self) -> Result<(), ConfigError> {
        let mut file = match std::fs::File::open(&self.config_file_path) {
            Ok(file) => file,
            Err(err) => {
                return if matches!(err.kind(), std::io::ErrorKind::NotFound) {
                    log::warn!(
                        "Configuration file does not exist: {}",
                        self.config_file_path.display()
                    );
                    Ok(())
                } else {
                    Err(ConfigError::Io(err))
                }
            }
        };

        log::info!(
            "Loading configuration from: {}",
            self.config_file_path.display()
        );

        let mut data = String::new();
        let _ = file.read_to_string(&mut data)?;

        let data: ConfigFile = toml::from_str(data.as_str())?;

        if let Some(emulator) = data.emulator {
            if let Some(bios) = emulator.bios {
                self.bios = Some(bios);
            }
        }

        if let Some(debugger) = data.debugger {
            if let Some(left) = debugger.left {
                self.debugger_position[0] = left;
            }
            if let Some(top) = debugger.top {
                self.debugger_position[1] = top;
            }
        }

        Ok(())
    }

    fn load_command_line(&mut self) -> Result<(), ConfigError> {
        use clap::Parser;

        let command_line = CommandLine::parse();

        if let Some(bios) = command_line.bios {
            self.bios = Some(bios);
        }

        Ok(())
    }
}

impl From<std::io::Error> for ConfigError {
    fn from(err: Error) -> Self {
        Self::Io(err)
    }
}

impl From<toml::de::Error> for ConfigError {
    fn from(err: toml::de::Error) -> Self {
        Self::Deserialize(err)
    }
}
