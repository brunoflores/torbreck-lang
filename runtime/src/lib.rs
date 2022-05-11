use std::error::Error;
use std::fmt;
use std::fs;

#[derive(Debug)]
pub struct ConfigError;

impl fmt::Display for ConfigError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "End of Stream")
    }
}

impl std::error::Error for ConfigError {}

pub struct Config {
    pub filename: String,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, ConfigError> {
        if args.len() != 2 {
            return Err(ConfigError);
        }

        let filename = args[1].clone();
        Ok(Config { filename })
    }
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(config.filename)?;
    println!("with text:\n{}", contents);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sane() {
        let cfg = Config::new(&["prog".to_string(), "filename.txt".to_string()]);
        assert_eq!("filename.txt", cfg.unwrap().filename);
    }

    #[test]
    fn can_fail() {
        let cfg = Config::new(&["prog".to_string()]);
        assert!(cfg.is_err());
    }
}
