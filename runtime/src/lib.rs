use std::error::Error;
use std::fmt;
use std::fs;

mod opcodes;
use opcodes::Instruction;

mod gc;

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

// Derive Copy and Clone for cases such as copying the accumulator to the
// argument stack.
#[derive(Debug, Copy, Clone)]
enum Value {
    Byte(u8),
    Header(gc::Header),
    Mark,
}

struct Machine {
    mem: Vec<u8>,
    pc: u8,
    asp: Vec<Value>,
    rsp: Vec<Value>,
    accu: Value,

    // Allocated once and for all.
    first_atoms: [gc::Header; 256],

    // ZINC Experiment: page 84,
    //   The values of initialized globals, that is a sequence of one integer
    //   (the slot number of the global) and one ZINC value (in prefix form).
    //   The integer -1 terminates this list.
    globals: Vec<u8>,
}

fn init_atoms() -> [gc::Header; 256] {
    let mut arr = [gc::Header::new(); 256];
    for (n, h) in arr.iter_mut().enumerate() {
        *h = gc::Header::newtag(n);
    }
    arr
}

impl Machine {
    pub fn new(mem: Vec<u8>) -> Self {
        Machine {
            mem,
            pc: 0,
            accu: Value::Byte(0),
            asp: vec![Value::Byte(0)],
            rsp: vec![Value::Byte(0)],
            first_atoms: init_atoms(),
            globals: vec![0], // TODO
        }
    }

    pub fn interpret(&mut self) {
        // Will loop until an explicit break - probably from a Stop instruction.
        loop {
            // PC is always incremented by one after this.
            match self.decode() {
                Instruction::Stop => break,
                Instruction::Constbyte => {
                    self.step(None);
                    let valofpc = self.mem[self.pc as usize];
                    self.accu = Value::Byte(self.mem[valofpc as usize]);
                }
                Instruction::Constshort => {
                    self.panic_pc("Constshort not implemented"); // TODO
                }
                Instruction::Atom0 => {
                    self.accu = Value::Header(self.first_atoms[0])
                }
                Instruction::Atom1 => {
                    self.accu = Value::Header(self.first_atoms[1])
                }
                Instruction::Atom2 => {
                    self.accu = Value::Header(self.first_atoms[2])
                }
                Instruction::Atom3 => {
                    self.accu = Value::Header(self.first_atoms[3])
                }
                Instruction::Atom4 => {
                    self.accu = Value::Header(self.first_atoms[4])
                }
                Instruction::Atom5 => {
                    self.accu = Value::Header(self.first_atoms[5])
                }
                Instruction::Atom6 => {
                    self.accu = Value::Header(self.first_atoms[6])
                }
                Instruction::Atom7 => {
                    self.accu = Value::Header(self.first_atoms[7])
                }
                Instruction::Atom8 => {
                    self.accu = Value::Header(self.first_atoms[8])
                }
                Instruction::Atom9 => {
                    self.accu = Value::Header(self.first_atoms[9])
                }
                Instruction::Atom => {
                    self.step(None);
                    let valofpc = self.mem[self.pc as usize];
                    self.accu =
                        Value::Header(self.first_atoms[valofpc as usize]);
                }
                Instruction::Getglobal => {
                    // I'm not so sure about this one looking at the sources.
                    // TODO
                    self.step(None);
                    let valofpc = self.mem[self.pc as usize];
                    self.accu = Value::Byte(self.globals[valofpc as usize]);
                }
                Instruction::Setglobal => {
                    // I'm not so sure about this one looking at the sources.
                    // TODO
                    self.step(None);
                    let valofpc = self.mem[self.pc as usize];
                    self.globals[valofpc as usize] = match self.accu {
                        Value::Byte(n) => n,
                        _ => self.panic_pc(
                            "don't know how to update a global with a header",
                        ),
                    }
                }
                Instruction::Push => self.asp.push(self.accu),
                Instruction::Pop => {
                    self.accu = match self.asp.pop() {
                        Some(value) => value,
                        None => {
                            self.panic_pc("popping an empty argument stack")
                        }
                    }
                }
                Instruction::Pushmark => self.asp.push(Value::Mark),
                _ => self.panic_pc("not implemented"), // TODO
            };
            self.step(None);
        }
    }

    fn step(&mut self, n: Option<u8>) {
        match n {
            Some(n) => self.pc += n,
            None => self.pc += 1,
        }
    }

    pub fn accu(&self) -> &Value {
        &self.accu
    }

    fn decode(&self) -> Instruction {
        opcodes::decode(self.mem[self.pc as usize])
    }

    fn panic_pc(&self, msg: &str) -> ! {
        panic!("pc: {}\t{}", self.pc, msg);
    }
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let contents = fs::read(config.filename)?;

    println!("with bytes: {:?}", contents);
    println!("number of global variables: {}", contents[0]);

    let mut machine = Machine::new(contents[1..].to_owned());
    machine.interpret();
    println!("accumulator is: {:?}", machine.accu());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sane() {
        let cfg =
            Config::new(&["prog".to_string(), "filename.txt".to_string()]);
        assert_eq!("filename.txt", cfg.unwrap().filename);
    }

    #[test]
    fn can_fail() {
        let cfg = Config::new(&["prog".to_string()]);
        assert!(cfg.is_err());
    }
}
