use std::error::Error;
use std::fmt;
use std::fs;

mod opcodes;
use opcodes::Instruction;

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

struct Machine {
    mem: Vec<u8>,
    pc: u8,
    asp: u8,
    rsp: u8,
    accu: u8,
}

impl Machine {
    pub fn new(mem: Vec<u8>) -> Self {
        Machine {
            mem,
            pc: 0,
            accu: 0,
            asp: 0,
            rsp: 0,
        }
    }

    pub fn interpret(&mut self) {
        loop {
            match self.decode() {
                Instruction::Stop => break,
                Instruction::Constbyte => {
                    self.step(None);
                    let valofpc = self.mem[self.pc as usize];
                    self.accu = self.mem[valofpc as usize];
                    self.step(None);
                }
                _ => panic!("not implemented"),
            }
        }
    }

    fn step(&mut self, n: Option<u8>) {
        match n {
            Some(n) => self.pc += n,
            None => self.pc += 1,
        }
    }

    pub fn accu(&self) -> u8 {
        self.accu
    }

    fn decode(&self) -> Instruction {
        opcodes::decode(self.mem[self.pc as usize])
    }
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let contents = fs::read(config.filename)?;

    // println!("with bytes: {:?}", contents);
    // println!("first instruction: {:?}", opcodes::decode(contents[0]));

    let mut machine = Machine::new(contents);
    machine.interpret();
    println!("accumulator is: {}", machine.accu());
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
