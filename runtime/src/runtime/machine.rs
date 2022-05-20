use crate::runtime::opcodes;
use crate::runtime::opcodes::Instruction;

// TODO Public?
#[derive(Debug, Clone)]
pub enum Value<'a> {
  Int(u8),
  Closure {
    code: u8, // A code pointer.
    env: Vec<Value<'a>>,
  },
  ConcreteTy {
    // Constant constructors are a zero-length slice.
    constructors: &'a [Value<'a>],
  },
  Record, // TODO
  Unit,
}

#[derive(Debug)]
struct ReturnFrame<'a> {
  pc: u8,
  env: Vec<Value<'a>>,
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

pub static FIRST_ATOMS: [Value; 1] = [Value::Unit];

#[derive(Debug)]
pub struct Machine<'a> {
  pc: u8,
  accu: Value<'a>,
  // Represent memory as a slice because it does not change in size.
  mem: &'a [u8],
  asp: Vec<AspValue<'a>>,
  rsp: Vec<RspValue<'a>>,
  env: Vec<Value<'a>>, // A stack of pointers to our heap.
  cache_size: u8,      // The number of entries in the volatile part of the env.
  first_atoms: &'a [Value<'a>; 1],

  // The ZINC Experiment: page 84,
  //   The values of initialized globals, that is a sequence of one integer
  //   (the slot number of the global) and one ZINC value (in prefix form).
  //   The integer -1 terminates this list.
  // TODO
  globals: Vec<u8>,
}

impl<'a> Machine<'a> {
  pub fn new(mem: &'a [u8]) -> Self {
    Machine {
      mem,
      pc: 0,
      accu: Value::Int(0),

      // TODO Consider Vec::with_capacity
      env: vec![],
      asp: vec![],
      rsp: vec![],

      first_atoms: &FIRST_ATOMS, // Statically allocated.

      globals: vec![], // TODO
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
          //self.accu = self.first_atoms[0];
          self.accu = Value::Unit;
          self.step(None);
        }
        Instruction::Atom1 => {
          // self.accu = self.first_atoms[1];
          self.accu = Value::Unit;
          self.step(None);
        }
        Instruction::Atom2 => {
          // self.accu = self.first_atoms[2];
          self.accu = Value::Unit;
          self.step(None);
        }
        Instruction::Atom3 => {
          // self.accu = self.first_atoms[3];
          self.accu = Value::Unit;
          self.step(None);
        }
        Instruction::Atom4 => {
          // self.accu = self.first_atoms[4];
          self.accu = Value::Unit;
          self.step(None);
        }
        Instruction::Atom5 => {
          // self.accu = self.first_atoms[5];
          self.accu = Value::Unit;
          self.step(None);
        }
        Instruction::Atom6 => {
          // self.accu = self.first_atoms[6];
          self.accu = Value::Unit;
          self.step(None);
        }
        Instruction::Atom7 => {
          // self.accu = self.first_atoms[7];
          self.accu = Value::Unit;
          self.step(None);
        }
        Instruction::Atom8 => {
          // self.accu = self.first_atoms[8];
          self.accu = Value::Unit;
          self.step(None);
        }
        Instruction::Atom9 => {
          // self.accu = self.first_atoms[9];
          self.accu = Value::Unit;
          self.step(None);
        }
        Instruction::Atom => {
          self.step(None);
          let valofpc = self.mem[self.pc as usize];
          // TODO: I think the following index can be out of bounds at run-time:
          // self.accu = self.first_atoms[valofpc as usize];
          self.accu = Value::Unit;
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
            self.env = env.to_vec();
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
            self.env = env.to_vec();
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
              self.env = env.to_vec();
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
                env: self.env.clone(),
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
                self.env = env.to_vec();
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
        Instruction::Cur => {
          self.heapify_env();
          self.step(None);
          self.accu = Value::Closure {
            code: self.pc,
            env: self.env.clone(),
          };
          self.step(None);
        }
        Instruction::Acc0 => {
          self.accu = self.access(0);
          self.step(None);
        }
        Instruction::Acc1 => {
          self.accu = self.access(1);
          self.step(None);
        }
        Instruction::Acc2 => {
          self.accu = self.access(2);
          self.step(None);
        }
        Instruction::Acc3 => {
          self.accu = self.access(3);
          self.step(None);
        }
        Instruction::Acc4 => {
          self.accu = self.access(4);
          self.step(None);
        }
        Instruction::Acc5 => {
          self.accu = self.access(5);
          self.step(None);
        }
        Instruction::Access => {
          self.step(None);
          self.accu = self.access(self.mem[self.pc as usize]);
          self.step(None);
        }
        Instruction::Let => {
          // Put the value of the accumulator in front of the environment.
          self.rsp.push(RspValue::Val(self.accu.clone()));
          self.cache_size += 1;
          self.step(None);
        }
        Instruction::Endlet1 => {
          if self.cache_size > 0 {
            self.cache_size -= 1;
            let _ = self.rsp.pop();
          } else {
            let _ = self.env.pop();
          }
          self.step(None);
        }
        Instruction::Endlet => {
          // Throw away the first n local variables from the environment.
          self.step(None);
          let valofpc = self.mem[self.pc as usize];
          if self.cache_size >= valofpc {
            self.cache_size -= valofpc;
            for _ in 0..valofpc {
              let _ = self.rsp.pop();
            }
            // TODO Consider shrink_to_fit
          } else {
            for _ in 0..(valofpc - self.cache_size) {
              // Pop, discard and de-allocates the respective boxed values
              // from our heap.
              let _ = self.env.pop();
              // TODO Consider shrink_to_fit
            }
            for _ in 0..self.cache_size {
              let _ = self.rsp.pop();
              // TODO Consider shrink_to_fit
            }
            self.cache_size = 0;
          }
          self.step(None);
        }
        Instruction::Dummy => {
          // Put n dummy closures in front of the environment.
          self.step(None);
          let valofpc = self.mem[self.pc as usize];
          assert!(valofpc > 0);
          // TODO ...
          self.step(None);
        }
        _ => self.panic_pc("not implemented", instr), // TODO
      };
    }
  }

  fn access(&self, i: u8) -> Value<'a> {
    if self.cache_size > i {
      if let Some(RspValue::Val(v)) = self.rsp.get(i as usize) {
        v.clone()
      } else {
        panic!("not a value");
      }
    } else {
      Value::Unit // TODO
    }

    //    if let Value::Tuple { fields } = self.accu {
    //      fields[i as usize].clone()
    //    } else {
    //      panic!("value does not have fields");
    //    }
  }

  fn heapify_env(&mut self) {}

  fn push_ret_frame(&mut self) {
    self.rsp.push(RspValue::RetFrame(ReturnFrame {
      pc: self.pc,
      env: self.env.clone(),
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
    let mut machine = Machine::new(&code);
    let accu = machine.interpret();
    if let Value::Int(int) = accu {
      assert_eq!(int, 1);
    } else {
      assert!(false);
    }
  }
}
