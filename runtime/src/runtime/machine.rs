use crate::runtime::opcodes;
use crate::runtime::opcodes::Instruction;

// TODO Public?
// Value is either a closure representing a function, or a constant.
#[derive(Debug, Clone)]
pub enum Value {
  Closure {
    code: u8, // A code pointer.
    env: Vec<Value>,
  },
  Int(i8),
  Bool(bool),
  ConcreteTy {
    tag: u8,
    // Constant constructors are a zero-length slice.
    constructors: Vec<Value>,
  },
  Record, // TODO
  Unit,
  Dummy,
}

impl Value {
  fn empty_from_tag(t: u8) -> Value {
    match t {
      0 => Value::Closure {
        code: 0,
        env: vec![],
      },
      1 => Value::ConcreteTy {
        tag: 0,
        constructors: vec![],
      },
      2 => Value::Record,
      _ => panic!("unknown tag: {}", t),
    }
  }
}

// Pages 32 and 33.
#[derive(Debug)]
struct ReturnFrame {
  pc: u8,               // Code pointer.
  pers_env: Vec<Value>, // Persistent part of environment.
  vol_env: Vec<Value>,  // Volatile part of environment.
}

#[derive(Debug)]
struct TrapFrame {}

// Value in the argument stack is either a value or a mark.
#[derive(Debug)]
enum AspValue {
  Val(Value),
  Mark,
}

// Value in the return stack is ... TODO
#[derive(Debug)]
enum RspValue {
  Val(Value),
  RetFrame(ReturnFrame),
  TrapFrame(TrapFrame),
}

pub static FIRST_ATOMS: [Value; 1] = [Value::Unit];

#[derive(Debug)]
pub struct Machine<'a> {
  pc: u8,             // Code pointer.
  env: Vec<Value>,    // Current environment.
  cache_size: u8,     // Number of entries in the volatile part of env.
  accu: Value,        // Accumulator for intermediate results.
  mem: &'a [u8],      // Program memory in bytes.
  asp: Vec<AspValue>, // Argument stack.
  rsp: Vec<RspValue>, // Return stack.
  first_atoms: &'a [Value; 1],

  // The ZINC Experiment: page 84,
  //   The values of initialized globals, that is a sequence of one integer
  //   (the slot number of the global) and one ZINC value (in prefix form).
  //   The integer -1 terminates this list.
  // TODO
  globals: Vec<i8>,
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
          self.accu = Value::Int(self.mem[valofpc as usize] as i8);
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
          // let valofpc = self.mem[self.pc as usize];
          //
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
              pers_env,
              vol_env,
            })) = self.rsp.last()
            {
              self.pc = *pc; // Go here next.
              self.env = pers_env.to_vec();
              self.cache_size = vol_env.len() as u8;
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
                pers_env,
                vol_env,
              })) = self.rsp.last()
              {
                self.pc = *pc; // Go here next.
                self.env = pers_env.to_vec();
                self.cache_size = vol_env.len() as u8;
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
        // I thought this was Dummy(n), but there's no n being used here?
        Instruction::Dummy => {
          // Put n dummy closures in front of the environment.
          self.step(None);
          let valofpc = self.mem[self.pc as usize];
          assert!(valofpc > 0);
          self.accu = Value::Dummy;
          // This seemed wrong...
          //  for _ in 0..valofpc {
          //    self.env.push(Value::Dummy);
          //  }
          self.step(None);
        }
        // I thought this was Update(n), but there's no n being used here?
        Instruction::Update => {
          self.accu = if let Some(AspValue::Val(v)) = self.asp.pop() {
            v
          } else {
            self.panic_pc("not a value", instr);
          }
        }
        Instruction::Letrec1 => {
          // Same as [Dummy; Cur ofs; Update], a frequent sequence
          // corresponding to [let ref f = function .. in ..].
          self.step(None);
          self.cache_size += 1;
          self.rsp.push(RspValue::RetFrame(ReturnFrame {
            pc: self.pc,
            pers_env: vec![], // TODO
            vol_env: vec![],
          }));
          self.step(None);
        }
        Instruction::Makeblock1 => {
          self.accu = match Value::empty_from_tag(self.mem[self.pc as usize]) {
            Value::Closure { code: _, env: _ } => Value::Closure {
              code: if let Value::Int(i) = self.accu {
                i as u8 // Can crash.
              } else {
                self.panic_pc("not an integer value", instr);
              },
              env: vec![],
            },
            Value::ConcreteTy {
              tag: _,
              constructors: _,
            } => Value::ConcreteTy {
              // TODO Wild guess.
              tag: if let Value::Int(i) = self.accu {
                i as u8 // Can crash.
              } else {
                self.panic_pc("not an integer value", instr);
              },
              constructors: vec![],
            },
            _ => panic!("not implemented"), // TODO
          };
          self.step(None);
        }
        Instruction::Makeblock2 => {
          self.accu = match Value::empty_from_tag(self.mem[self.pc as usize]) {
            Value::Closure { code: _, env: _ } => Value::Closure {
              code: if let Value::Int(i) = self.accu {
                i as u8 // Can crash.
              } else {
                self.panic_pc("not an integer value", instr);
              },
              env: if let Some(AspValue::Val(v)) = self.asp.pop() {
                vec![v]
              } else {
                // Do nothing.
                // TODO Should we crash?
                vec![]
              },
            },
            Value::ConcreteTy {
              tag: _,
              constructors: _,
            } => Value::ConcreteTy {
              // TODO Wild guess.
              tag: if let Value::Int(i) = self.accu {
                i as u8 // Can crash.
              } else {
                self.panic_pc("not an integer value", instr);
              },
              constructors: if let Some(AspValue::Val(v)) = self.asp.pop() {
                vec![v]
              } else {
                // Do nothing.
                // TODO Should we crash?
                vec![]
              },
            },
            _ => panic!("not implemented"), // TODO
          };
          self.step(None);
        }
        Instruction::Makeblock3 => {
          self.panic_pc("same sa above", instr);
        }
        Instruction::Makeblock4 => {
          self.panic_pc("same sa above", instr);
        }
        Instruction::Makeblock => {
          self.panic_pc("same sa above", instr); // TODO Maybe?
        }
        Instruction::Getfield0 => {
          self.accu = match self.accu {
            Value::Closure { code, env: _ } => Value::Int(code as i8),
            Value::ConcreteTy {
              tag,
              constructors: _,
            } => Value::Int(tag as i8),
            _ => self.panic_pc("not implemented", instr),
          };
          self.step(None);
        }
        Instruction::Getfield1 => {
          self.accu = match &self.accu {
            Value::Closure { code: _, env } => env[0].clone(), // Can panic.
            Value::ConcreteTy {
              tag: _,
              constructors,
            } => constructors[0].clone(), // Can panic.
            _ => self.panic_pc("not implemented", instr),
          };
          self.step(None);
        }
        Instruction::Getfield2 => {
          self.panic_pc("not implemented", instr); // TODO
        }
        Instruction::Getfield3 => {
          self.panic_pc("not implemented", instr); // TODO
        }
        Instruction::Getfield => {
          self.step(None);
          self.accu = match &self.accu {
            Value::Closure { code: _, env } => {
              env[self.mem[self.pc as usize] as usize].clone()
            } // Can panic.
            Value::ConcreteTy {
              tag: _,
              constructors,
            } => constructors[self.mem[self.pc as usize] as usize].clone(), // Can panic.
            _ => self.panic_pc("not implemented", instr),
          };
          self.step(None);
        }
        Instruction::Setfield0 => {
          self.accu = match &self.accu {
            Value::Closure { code: _, env } => {
              if let Some(AspValue::Val(Value::Int(code))) = self.asp.pop() {
                Value::Closure {
                  code: code as u8, // Can crash.
                  env: env.clone(),
                }
              } else {
                panic!();
              }
            }
            _ => panic!(),
          };
          self.step(None);
        }
        Instruction::Setfield1 => {
          self.accu = match self.accu {
            Value::Closure { code, env: _ } => {
              if let Some(AspValue::Val(v)) = self.asp.pop() {
                Value::Closure { code, env: vec![v] }
              } else {
                panic!();
              }
            }
            _ => panic!(),
          };
          self.step(None);
        }
        Instruction::Setfield2 => {
          self.panic_pc("not implemented", instr); // TODO
        }
        Instruction::Setfield3 => {
          self.panic_pc("not implemented", instr); // TODO
        }
        Instruction::Setfield => {
          self.panic_pc("not implemented", instr); // TODO
        }
        Instruction::Succint => {
          if let Value::Int(i) = self.accu {
            self.accu = Value::Int(i + 1);
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        Instruction::Predint => {
          if let Value::Int(i) = self.accu {
            self.accu = Value::Int(i - 1);
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        Instruction::Negint => {
          if let Value::Int(i) = self.accu {
            self.accu = Value::Int(-i);
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        Instruction::Addint => {
          if let Value::Int(i) = self.accu {
            self.accu =
              if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
                Value::Int(i + y)
              } else {
                self.panic_pc("not an integer in asp", instr);
              }
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        Instruction::Subint => {
          if let Value::Int(i) = self.accu {
            self.accu =
              if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
                Value::Int(i - y)
              } else {
                self.panic_pc("not an integer in asp", instr);
              }
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        Instruction::Mulint => {
          if let Value::Int(i) = self.accu {
            self.accu =
              if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
                Value::Int(i * y)
              } else {
                self.panic_pc("not an integer in asp", instr);
              }
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        Instruction::Divint => {
          if let Value::Int(i) = self.accu {
            self.accu = match self.asp.pop() {
              Some(AspValue::Val(Value::Int(y))) if y > 0 => Value::Int(i / y),
              Some(AspValue::Val(Value::Int(_))) => {
                self.panic_pc("division by zero", instr)
              }
              _ => self.panic_pc("not an integer in asp", instr),
            }
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        Instruction::Modint => {
          if let Value::Int(i) = self.accu {
            self.accu = match self.asp.pop() {
              Some(AspValue::Val(Value::Int(y))) if y > 0 => Value::Int(i % y),
              Some(AspValue::Val(Value::Int(_))) => {
                self.panic_pc("division by zero", instr)
              }
              _ => self.panic_pc("not an integer in asp", instr),
            }
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        Instruction::Andint => {
          if let Value::Int(i) = self.accu {
            self.accu =
              if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
                Value::Int(i & y)
              } else {
                self.panic_pc("not an integer in asp", instr);
              }
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        Instruction::Orint => {
          if let Value::Int(i) = self.accu {
            self.accu =
              if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
                Value::Int(i | y)
              } else {
                self.panic_pc("not an integer in asp", instr);
              }
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        Instruction::Xorint => {
          if let Value::Int(i) = self.accu {
            self.accu =
              if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
                Value::Int(i ^ y)
              } else {
                self.panic_pc("not an integer in asp", instr);
              }
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        Instruction::Shiftleftint => {
          if let Value::Int(i) = self.accu {
            self.accu =
              if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
                Value::Int(i << y)
              } else {
                self.panic_pc("not an integer in asp", instr);
              }
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        Instruction::Shiftrightintsigned => {
          if let Value::Int(i) = self.accu {
            self.accu =
              if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
                Value::Int(i >> y)
              } else {
                self.panic_pc("not an integer in asp", instr);
              }
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        Instruction::Shiftrightintunsigned => {
          if let Value::Int(i) = self.accu {
            self.accu =
              if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
                Value::Int(((i as u8) >> y) as i8) // Can crash.
              } else {
                self.panic_pc("not an integer in asp", instr);
              }
          } else {
            self.panic_pc("not an integer", instr);
          }
        }
        _ => self.panic_pc("not implemented", instr), // TODO
      };
    }
  }

  fn access(&self, i: u8) -> Value {
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
      pers_env: self.env.clone(),
      vol_env: self.env[0..self.cache_size as usize].to_vec(),
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
