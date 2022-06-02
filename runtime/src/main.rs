use std::env;
use std::error::Error;
use std::fs;
use std::io::Read;
use std::process;

mod runtime;
use runtime::machine::Machine;

fn usage() -> &'static str {
  "Usage: breckrun [filename]"
}

#[derive(Debug)]
pub struct ConfigError;

pub struct Config {
  pub filename: String,
}

impl Config {
  pub fn new(args: &[String]) -> Result<Config, ConfigError> {
    if args.len() == 1 {
      Ok(Config {
        filename: "-".into(),
      })
    } else if args.len() == 2 {
      Ok(Config {
        filename: args[1].clone(),
      })
    } else {
      Err(ConfigError)
    }
  }
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
  let fname = config.filename.clone();
  let input: Vec<u8> = if fname == "-" {
    let mut buff: Vec<u8> = vec![];
    for b in std::io::stdin().bytes() {
      buff.push(b.unwrap());
    }
    buff
  } else if let Ok(c) = fs::read(fname) {
    c
  } else {
    println!("could not open file: {}", config.filename);
    process::exit(1);
  };

  let contents: Vec<u8> = if !input.is_empty() && input[0] == b'#' {
    let mut buff: Vec<u8> = vec![];
    let mut donewithshebang = false;
    for b in input.iter() {
      let byte = *b;
      if !donewithshebang && byte != b'\n' {
        continue;
      } else if !donewithshebang {
        donewithshebang = true;
        continue;
      }
      buff.push(byte);
    }
    buff
  } else {
    input
  };

  // println!("with unsigned bytes: {:?}", contents);

  let signed: Vec<i32> = contents
    .into_iter()
    .map(|e| if e <= 127 { e as i32 } else { (e - 128) as i32 })
    .collect();

  // println!("with signed bytes: {:?}", signed);
  // println!("number of global variables: {}", contents[0]);

  let mut machine = Machine::new(&signed[1..]);
  // println!("starting interpretation - we might never return");
  let _accu = machine.interpret();
  // println!("returned - accumulator is: {:?}", accu);
  Ok(())
}

fn main() {
  let args: Vec<String> = env::args().collect();
  let config = match Config::new(&args) {
    Err(ConfigError) => {
      println!("{}", usage());
      process::exit(1)
    }
    Ok(config) => config,
  };
  if let Err(e) = run(config) {
    println!("error: {e}");
    process::exit(1);
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
