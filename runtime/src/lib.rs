use std::error::Error;
use std::fmt;
use std::fs;

mod opcodes;
use opcodes::Instruction;

mod gc;
use gc::Header;
use gc::FIRST_ATOMS;

// A something in the heap.
#[derive(Debug)]
struct Boxed {
  header: Header,
  fields: Vec<u8>,
}

#[derive(Debug, Copy, Clone)]
enum Value<'a> {
  // An unboxed integer.
  Int(u8),

  // Pointer to Header here can point to the heap or
  // to some other statically allocated region.
  Hd(&'a Header),
}

#[derive(Debug)]
struct ReturnFrame<'a> {
  pc: u8,
  env: Value<'a>,
  cache_size: u8,
}

#[derive(Debug)]
enum AspValue<'a> {
  Val(Value<'a>),
  Mark,
}

#[derive(Debug)]
enum RspValue<'a> {
  RetFrame(ReturnFrame<'a>),
}

#[derive(Debug)]
struct Machine<'a> {
  pc: u8,
  accu: Value<'a>,
  mem: Vec<u8>,
  asp: Vec<AspValue<'a>>,
  rsp: Vec<RspValue<'a>>,

  // Allocated statically once and for all.
  first_atoms: &'static [Header; 256],

  // The ZINC Experiment: page 84,
  //   The values of initialized globals, that is a sequence of one integer
  //   (the slot number of the global) and one ZINC value (in prefix form).
  //   The integer -1 terminates this list.
  globals: Vec<u8>,
}

impl<'a> Machine<'a> {
  pub fn new(mem: Vec<u8>) -> Self {
    Machine {
      mem,
      pc: 0,
      accu: Value::Int(0),
      asp: vec![],
      rsp: vec![],
      first_atoms: &FIRST_ATOMS,
      globals: vec![0], // TODO
    }
  }

  pub fn interpret(&'a mut self) -> Value {
    // Will loop until an explicit break - probably from a Stop instruction.
    loop {
      // PC is always incremented by one after this.
      match self.decode() {
        Instruction::Stop => return self.accu,
        Instruction::Constbyte => {
          self.step(None);
          let valofpc = self.mem[self.pc as usize];
          self.accu = Value::Int(self.mem[valofpc as usize]);
        }
        Instruction::Constshort => {
          self.panic_pc("Constshort not implemented"); // TODO
        }
        Instruction::Atom0 => self.accu = Value::Hd(&self.first_atoms[0]),
        Instruction::Atom1 => self.accu = Value::Hd(&self.first_atoms[1]),
        Instruction::Atom2 => self.accu = Value::Hd(&self.first_atoms[2]),
        Instruction::Atom3 => self.accu = Value::Hd(&self.first_atoms[3]),
        Instruction::Atom4 => self.accu = Value::Hd(&self.first_atoms[4]),
        Instruction::Atom5 => self.accu = Value::Hd(&self.first_atoms[5]),
        Instruction::Atom6 => self.accu = Value::Hd(&self.first_atoms[6]),
        Instruction::Atom7 => self.accu = Value::Hd(&self.first_atoms[7]),
        Instruction::Atom8 => self.accu = Value::Hd(&self.first_atoms[8]),
        Instruction::Atom9 => self.accu = Value::Hd(&self.first_atoms[9]),
        Instruction::Atom => {
          self.step(None);
          let valofpc = self.mem[self.pc as usize];
          // TODO: I think the following index can panic at run-time:
          self.accu = Value::Hd(&self.first_atoms[valofpc as usize]);
        }
        Instruction::Getglobal => {
          // I'm not so sure about this one looking at the sources.
          // TODO
          self.step(None);
          let valofpc = self.mem[self.pc as usize];
          self.accu = Value::Int(self.globals[valofpc as usize]);
        }
        Instruction::Setglobal => {
          // I'm not so sure about this one looking at the sources.
          // TODO
          self.step(None);
          let valofpc = self.mem[self.pc as usize];
          self.globals[valofpc as usize] = match self.accu {
            Value::Int(n) => n,
            _ => {
              self.panic_pc("don't know how to update a global with a header")
            }
          }
        }
        Instruction::Push => self.asp.push(AspValue::Val(self.accu)),
        Instruction::Pop => {
          self.accu = match self.asp.pop() {
            Some(AspValue::Val(value)) => value,
            Some(AspValue::Mark) => self.panic_pc("popping a mark"),
            None => self.panic_pc("popping an empty argument stack"),
          }
        }
        Instruction::Pushmark => self.asp.push(AspValue::Mark),
        Instruction::Apply => {}
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

  fn decode(&self) -> Instruction {
    opcodes::decode(self.mem[self.pc as usize])
  }

  fn panic_pc(&self, msg: &str) -> ! {
    panic!("pc: {}\t{}", self.pc, msg);
  }
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

  let mut machine = Machine::new(contents[1..].to_owned());
  println!("starting interpretation - we might never return");
  let accu = machine.interpret();
  println!("returned - accumulator is: {:?}", accu);
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
