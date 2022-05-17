use crate::runtime::opcodes;
use crate::runtime::opcodes::Instruction;

use crate::runtime::gc::Header;
use crate::runtime::gc::FIRST_ATOMS;

// A something in the heap.
#[derive(Debug)]
struct Boxed {
  header: Header,
  fields: Vec<u8>,
}

#[derive(Debug, Copy, Clone)]
pub enum Value<'a> {
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
pub struct Machine<'a> {
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
