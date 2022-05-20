use std::env;
use std::process;

use std::fmt;
use std::fs;

use std::error::Error;

mod runtime;
use runtime::machine::Machine;

fn usage() -> &'static str {
  "Usage: breckrun filename"
}

fn crash(msg: &str) -> ! {
  println!("{}", msg);
  process::exit(1);
}

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
  let contents = fs::read(config.filename)?;

  println!("with bytes: {:?}", contents);
  println!("number of global variables: {}", contents[0]);

  let mut machine = Machine::new(&contents[1..]);
  println!("starting interpretation - we might never return");
  let accu = machine.interpret();
  println!("returned - accumulator is: {:?}", accu);
  Ok(())
}

fn main() {
  let args: Vec<String> = env::args().collect();
  let config = match Config::new(&args) {
    Err(ConfigError) => crash(usage()),
    Ok(config) => config,
  };
  if let Err(e) = run(config) {
    crash(&format!("error: {}", e));
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn cli_is_sane() {
    let cfg = Config::new(&["prog".to_string(), "filename.txt".to_string()]);
    assert_eq!("filename.txt", cfg.unwrap().filename);
  }

  #[test]
  fn cli_can_fail() {
    let cfg = Config::new(&["prog".to_string()]);
    assert!(cfg.is_err());
  }
}
