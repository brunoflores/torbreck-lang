use std::env;
use std::error::Error;
use std::fs;
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
    panic!("input from stdin not implemented");
  } else if let Ok(u8bytes) = fs::read(fname) {
    let mut buff: Vec<u8> = vec![];
    let mut donewithshebang = false;
    // let mut val32: i32;
    // let mut position = 0;
    for b in u8bytes.iter() {
      let byte = *b;
      // if byte == b'#' {
      if !donewithshebang && byte != b'\n' {
        continue;
      } else if !donewithshebang {
        donewithshebang = true;
        continue;
      }
      // }

      // TODO: this will likely move to the Machine struct in some form...
      // val32 = (byte >> (8 * position)) as i32;
      // if position == 3 {
      //   buff.push(val32);
      //   position = 0;
      // } else {
      //   position += 1;
      // }
      buff.push(byte);
    }
    buff
  } else {
    println!("could not open file: {}", config.filename);
    process::exit(1);
  };

  // Print input bytes:
  // println!("{:?}", input);

  let mut machine = Machine::new(&input[0..]);
  let accu = machine.interpret();
  println!("returned - accumulator is: {:?}", accu);
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
}
