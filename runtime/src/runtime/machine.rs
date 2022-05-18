use crate::runtime::opcodes;
use crate::runtime::opcodes::Instruction;

// use crate::runtime::alloc;

// TODO I don't like Value being public.
#[derive(Debug, Clone)]
pub enum Value<'a> {
  // An unboxed integer.
  Int(u8),

  // Pointer to Header here can point to the heap or
  // to some other statically allocated region.
  Hd(&'a Value<'a>),

  Tuple { fields: &'a [u8] },
  Closure { code: u8, env: Box<Value<'a>> },

  Atom0,
  Atom1,
  Atom2,
  Atom3,
  Atom4,
  Atom5,
  Atom6,
  Atom7,
  Atom8,
  Atom9,
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

pub static FIRST_ATOMS: [Value; 10] = [
  Value::Atom0,
  Value::Atom1,
  Value::Atom2,
  Value::Atom3,
  Value::Atom4,
  Value::Atom5,
  Value::Atom6,
  Value::Atom7,
  Value::Atom8,
  Value::Atom9,
];

#[derive(Debug)]
pub struct Machine<'a> {
  pc: u8,
  accu: Value<'a>,
  env: Value<'a>,
  mem: Vec<u8>,
  asp: Vec<AspValue<'a>>,
  rsp: Vec<RspValue<'a>>,
  cache_size: u8,
  first_atoms: &'a [Value<'a>; 10],

  // The ZINC Experiment: page 84,
  //   The values of initialized globals, that is a sequence of one integer
  //   (the slot number of the global) and one ZINC value (in prefix form).
  //   The integer -1 terminates this list.
  // TODO
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
      first_atoms: &FIRST_ATOMS,
      globals: vec![0], // TODO
      cache_size: 0,
    }
  }

  pub fn interpret(&'a mut self) -> Value {
    loop {
      let instr = self.decode();
      match instr {
        Instruction::Stop => return self.accu.clone(),
        Instruction::Constbyte => {
          self.step(None);
          let valofpc = self.mem[self.pc as usize];
          self.accu = Value::Int(self.mem[valofpc as usize]);
          self.step(None);
        }
        Instruction::Constshort => {
          self.panic_pc("Constshort not implemented", instr); // TODO
        }
        Instruction::Atom0 => {
          self.accu = Value::Hd(&self.first_atoms[0]);
          self.step(None);
        }
        Instruction::Atom1 => {
          self.accu = Value::Hd(&self.first_atoms[1]);
          self.step(None);
        }
        Instruction::Atom2 => {
          self.accu = Value::Hd(&self.first_atoms[2]);
          self.step(None);
        }
        Instruction::Atom3 => {
          self.accu = Value::Hd(&self.first_atoms[3]);
          self.step(None);
        }
        Instruction::Atom4 => {
          self.accu = Value::Hd(&self.first_atoms[4]);
          self.step(None);
        }
        Instruction::Atom5 => {
          self.accu = Value::Hd(&self.first_atoms[5]);
          self.step(None);
        }
        Instruction::Atom6 => {
          self.accu = Value::Hd(&self.first_atoms[6]);
          self.step(None);
        }
        Instruction::Atom7 => {
          self.accu = Value::Hd(&self.first_atoms[7]);
          self.step(None);
        }
        Instruction::Atom8 => {
          self.accu = Value::Hd(&self.first_atoms[8]);
          self.step(None);
        }
        Instruction::Atom9 => {
          self.accu = Value::Hd(&self.first_atoms[9]);
          self.step(None);
        }
        Instruction::Atom => {
          self.step(None);
          let valofpc = self.mem[self.pc as usize];
          // TODO: I think the following index can be out of bounds at run-time:
          self.accu = Value::Hd(&self.first_atoms[valofpc as usize]);
          self.step(None);
        }
        Instruction::Getglobal => {
          // I'm not so sure about this one looking at the sources.
          // TODO
          self.step(None);
          let valofpc = self.mem[self.pc as usize];
          self.accu = Value::Int(self.globals[valofpc as usize]);
          self.step(None);
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
              self.panic_pc(
                "don't know how to update a global with a header",
                instr,
              )
            }
          };
          self.step(None);
        }
        Instruction::Push => {
          self.asp.push(AspValue::Val(self.accu.clone()));
          self.step(None);
        }
        Instruction::Pop => {
          self.accu = match self.asp.pop() {
            Some(AspValue::Val(value)) => value,
            Some(AspValue::Mark) => self.panic_pc("popping a mark", instr),
            None => self.panic_pc("popping an empty argument stack", instr),
          };
          self.step(None);
        }
        Instruction::Pushmark => {
          self.asp.push(AspValue::Mark);
          self.step(None);
        }
        Instruction::Apply => {
          self.push_ret_frame();
          if let Some(AspValue::Val(v)) = self.asp.pop() {
            self.rsp.push(RspValue::Val(v));
          } else {
            // TODO
            self.panic_pc(
              "got something else - don't know what to do with it",
              instr,
            );
          }
          self.cache_size = 1;
          if let Value::Closure { code, env } = &self.accu {
            self.pc = *code;
            self.env = *env.clone();
          } else {
            // TODO
            self.panic_pc("didn't get a closure in the accumulator", instr);
          }
        }
        Instruction::Appterm => {
          if let Some(AspValue::Val(v)) = self.asp.pop() {
            self.rsp.push(RspValue::Val(v));
          } else {
            // TODO
            self.panic_pc(
              "got something else - don't know what to do with it",
              instr,
            );
          }
          self.cache_size = 1;
          if let Value::Closure { code, env } = &self.accu {
            self.pc = *code;
            self.env = *env.clone();
          } else {
            // TODO
            self.panic_pc("didn't get a closure in the accumulator", instr);
          }
        }
        Instruction::Return => {
          // Page 80:
          //
          // If there is a mark on top of the stack, pop it and return to
          // the caller.
          //
          // Otherwise, jump to the closure contained in the accumulator.
          //
          if let Some(AspValue::Mark) = self.asp.pop() {
            // Peek at the return stack, then pop.
            // TODO No sure it's needed.
            if let Some(RspValue::RetFrame(ReturnFrame {
              pc,
              env,
              cache_size,
            })) = self.rsp.last()
            {
              self.pc = *pc; // Go here next.
              self.env = env.clone();
              self.cache_size = *cache_size;
            }
            self.pop_ret_frame();
            // Proceed to instruction pointed at above.
          } else {
            // TODO Anything to do here? Assume next instruction is Appterm?
            // https://github.com/brunoflores/camllight/blob/master/sources/src/runtime/interp.c#L292
          }
        }
        Instruction::Grab => {
          match self.asp.pop() {
            Some(AspValue::Mark) => {
              // Build a closure and return it to the caller.
              self.heapify_env();
              self.accu = Value::Closure {
                code: self.pc,
                env: Box::new(self.env.clone()),
              };
              // Peek at the return stack, then pop.
              // TODO No sure it's needed.
              if let Some(RspValue::RetFrame(ReturnFrame {
                pc,
                env,
                cache_size,
              })) = self.rsp.last()
              {
                self.pc = *pc; // Go here next.
                self.env = env.clone();
                self.cache_size = *cache_size;
              } else {
                self.panic_pc("not a return frame", instr);
              }
              self.pop_ret_frame();
            }
            Some(AspValue::Val(v)) => {
              self.rsp.push(RspValue::Val(v));
              self.cache_size += 1;
            }
            _ => self.panic_pc("don't know what to do", instr),
          }
        }
        _ => self.panic_pc("not implemented", instr), // TODO
      };
    }
  }

  fn heapify_env(&mut self) {}

  fn push_ret_frame(&mut self) {
    self.rsp.push(RspValue::RetFrame(ReturnFrame {
      pc: self.pc,
      env: self.accu.clone(),
      cache_size: self.cache_size,
    }));
  }

  fn pop_ret_frame(&mut self) {
    // Assume the caller has already checked whether rsp.last() is
    // a return frame.
    let _ = self.rsp.pop();
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
      panic!("pc: {}: instruction out of bounds", self.pc);
    }
  }

  fn panic_pc(&self, msg: &str, instr: Instruction) -> ! {
    panic!("pc: {}: {}: {}", self.pc, instr, msg);
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
