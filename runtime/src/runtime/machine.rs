use crate::runtime::opcodes;
use crate::runtime::opcodes::Instruction;

use crate::runtime::alloc;

// TODO I don't like Value being public.
#[derive(Debug, Clone)]
pub enum Value<'a> {
  // An unboxed integer.
  Int(u8),

  // Pointer to Header here can point to the heap or
  // to some other statically allocated region.
  Hd(&'a alloc::Header),

  Tuple { fields: Vec<u8> },
  Closure { code: u8, env: Box<Value<'a>> },
}

#[derive(Debug)]
struct ReturnFrame<'a> {
  pc: u8,
  env: Value<'a>,
  cache_size: u8,
}

#[derive(Debug)]
struct TrapFrame {}

#[derive(Debug)]
enum AspValue<'a> {
  Val(Value<'a>),
  Mark,
}

#[derive(Debug)]
enum RspValue<'a> {
  Val(Value<'a>),
  RetFrame(ReturnFrame<'a>),
  TrapFrame(TrapFrame),
}

#[derive(Debug)]
pub struct Machine<'a> {
  pc: u8,
  accu: Value<'a>,
  env: Value<'a>,
  mem: Vec<u8>,
  asp: Vec<AspValue<'a>>,
  rsp: Vec<RspValue<'a>>,
  cache_size: u8,

  // Allocated statically once and for all.
  first_atoms: &'static [alloc::Header; 256],

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
      env: Value::Int(0),
      asp: vec![],
      rsp: vec![],
      first_atoms: &alloc::FIRST_ATOMS,
      globals: vec![0], // TODO
      cache_size: 0,
    }
  }

  pub fn interpret(&'a mut self) -> Value {
    // Will loop until an explicit break - probably from a Stop instruction.
    loop {
      // PC is always incremented by one after this.
      // TODO might not apply to every instruction.
      match self.decode() {
        Instruction::Stop => return self.accu.clone(), // TODO why if it's there?
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
          // TODO: I think the following index can be out of bounds at run-time:
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
              // TODO
              self.panic_pc("don't know how to update a global with a header")
            }
          }
        }
        Instruction::Push => self.asp.push(AspValue::Val(self.accu.clone())), // TODO
        Instruction::Pop => {
          self.accu = match self.asp.pop() {
            Some(AspValue::Val(value)) => value,
            Some(AspValue::Mark) => self.panic_pc("popping a mark"),
            None => self.panic_pc("popping an empty argument stack"),
          }
        }
        Instruction::Pushmark => self.asp.push(AspValue::Mark),
        Instruction::Apply => {
          self.rsp.push(RspValue::RetFrame(ReturnFrame {
            pc: self.pc,
            env: self.accu.clone(), // TODO
            cache_size: self.cache_size,
          }));
          if let Some(AspValue::Val(v)) = self.asp.pop() {
            self.rsp.push(RspValue::Val(v));
          } else {
            // TODO
            self.panic_pc("got something else - don't know what to do with it");
          }
          self.cache_size = 1;
          if let Value::Closure { code, env } = &self.accu {
            self.pc = *code;
            self.env = *env.clone();
          } else {
            // TODO
            self.panic_pc("didn't get a closure in the accumulator");
          }
        }
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
    if let Some(i) = self.mem.get(self.pc as usize) {
      opcodes::decode(*i)
    } else {
      panic!("instruction out of bounds");
    }
  }

  fn panic_pc(&self, msg: &str) -> ! {
    panic!("pc: {}: {}", self.pc, msg);
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn machine_halts() {
    let code = vec![1, 0, 59];
    let mut machine = Machine::new(code);
    let accu = machine.interpret();
    if let Value::Int(int) = accu {
      assert_eq!(int, 1);
    } else {
      assert!(false);
    }
  }
}
