use crate::runtime::opcodes;
use crate::runtime::opcodes::Instruction;
use crate::runtime::prims;

// TODO Public?
#[derive(Debug, Clone)]
pub struct Closure(u32, Vec<Value>);

// TODO Public?
#[derive(Debug, Clone)]
pub enum Value {
  Fn(Closure),
  FnRec(Closure),
  Int(i32),
  Float(f32),
  String(String),
  True,
  False,
  Dummy,
  Atom0,
}

impl Value {
  fn string_from_bytes(bs: &[u8]) -> (Value, usize) {
    let mut buff: Vec<u8> = vec![];
    let mut i: usize = 0;
    loop {
      let b = bs[i] as u8;
      if b == 0 {
        break;
      }
      buff.push(b);
      i += 1;
    }
    if let Ok(s) = std::str::from_utf8(&buff) {
      (Value::String(s.into()), i)
    } else {
      panic!("bytes failed to convert to utf8: {:?}", buff);
    }
  }
}

// Value in the argument stack is either a value or a mark.
#[derive(Debug)]
enum AspValue {
  Val(Value),
  Mark,
}

struct Monitor {
  len_env_max: usize,
  len_asp_max: usize,
  len_rsp_max: usize,
}

impl Monitor {
  fn new() -> Self {
    Monitor {
      len_rsp_max: 0,
      len_asp_max: 0,
      len_env_max: 0,
    }
  }
}

pub struct Machine<'machine> {
  pc: u32,                   // Code pointer.
  mem: &'machine [u8],       // Program memory in bytes.
  env: Vec<Value>,           // Current environment.
  asp: Vec<AspValue>,        // Argument stack.
  rsp: Vec<Closure>,         // Return stack.
  accu: Value,               // Accumulator for intermediate results.
  prims: [prims::PrimFn; 4], // Primitives table
  monitor: Monitor,
}

// I know needless lifetimes are considered a bad practice.
// Allow them because the transparency helps me understand the type-checker.
#[allow(clippy::needless_lifetimes)]
impl<'machine> Machine<'machine> {
  pub fn new(mem: &'machine [u8]) -> Self {
    Machine {
      mem,
      pc: 0,
      accu: Value::Atom0,

      // TODO Consider Vec::with_capacity
      env: vec![],
      asp: vec![],
      rsp: vec![],

      // Feed the primitives table
      prims: [
        prims::print_string,
        prims::less_than,
        prims::int_sub,
        prims::int_add,
      ],

      monitor: Monitor::new(),
    }
  }

  pub fn interpret<'a>(&'a mut self) -> Value {
    loop {
      let instr = self.decode();
      match instr {
        Instruction::Stop => return self.accu.clone(),
        Instruction::Access => {
          self.step(None);
          self.accu =
            self.access_nth(self.mem[self.pc as usize] as usize).clone();
          self.step(None);
        }
        Instruction::Acc0 => {
          self.accu = self.access_nth(0).clone();
          self.step(None);
        }
        Instruction::Acc1 => {
          self.accu = self.access_nth(1).clone();
          self.step(None);
        }
        Instruction::Appterm => {
          // Application in tail-call position.
          // There is no need to push a mark in the argument stack.
          if let Value::Fn(Closure(c1, e1)) = &self.accu {
            self.pc = *c1 as u32;
            let new_env = {
              let mut e1 = e1.clone();
              e1.push(if let AspValue::Val(v) = self.asp.pop().unwrap() {
                v
              } else {
                panic!();
              });
              e1
            };
            self.env_replace(new_env);
          } else {
            panic!();
          }
        }
        Instruction::Apply => {
          // Application using the return stack.
          self.rsp_push(Closure(self.pc + 1, self.env.clone()));

          if let Value::Fn(Closure(c1, e1)) = &self.accu {
            self.pc = *c1 as u32;
            let new_env = {
              let mut e1 = e1.clone();
              e1.push(if let AspValue::Val(v) = self.asp.pop().unwrap() {
                v
              } else {
                panic!();
              });
              e1
            };
            self.env_replace(new_env);
          } else if let Value::FnRec(Closure(c1, _)) = &self.accu {
            self.pc = *c1 as u32;
            let new_env = {
              let mut e1 = vec![self.accu.clone()];
              e1.push(if let AspValue::Val(v) = self.asp.pop().unwrap() {
                v
              } else {
                panic!();
              });
              e1
            };
            self.env_replace(new_env);
          } else {
            panic!("not a closure in the accumulator: {:?}", self.accu);
          }
        }
        Instruction::Push => {
          // Push the accumulator onto the argument stack,
          // leave the accumulator untouched, and
          // take a step.
          self.asp_push(AspValue::Val(self.accu.clone()));
          self.step(None);
        }
        Instruction::Pushmark => {
          // Push a mark onto the argument stack, and
          // take step.
          self.asp_push(AspValue::Mark);
          self.step(None);
        }
        Instruction::Grab => {
          // Abstraction in tail-call position.
          match self.asp.pop() {
            // 1) In tail-call position.
            //    Simply pops one from the argumeht stack and puts it in front
            //    of the environment.
            //    Pc simply takes a step.
            Some(AspValue::Val(v)) => {
              self.env_push(v);
              self.step(None);
            }
            // 2) Got a mark so must build a closure.
            //    All arguments have already been consumed.
            //    Builds a closure of the current code with the current
            //    environment and returns it to the caller while popping
            //    the mark.
            Some(AspValue::Mark) => {
              let Closure(c1, e1) = self.rsp.pop().unwrap();
              // Read current state...
              self.accu = Value::Fn(Closure(self.pc, self.env.clone()));
              // now modify it...
              self.pc = c1;
              self.env_replace(e1);
            }
            _ => self.panic_pc("argument stack is empty", instr),
          }
        }
        Instruction::Cur => {
          // Abstraction using the stack.
          self.step(None);
          self.accu = Value::Fn(Closure(
            self.pc + (self.mem[self.pc as usize] as u32), // TODO how to jump backward and forward?
            self.env.clone(),
          ));
          self.step(None);
        }
        Instruction::Return => {
          // Page 80:
          //
          // If there is a mark on top of the stack, pop it and return to
          // the caller. Otherwise, jump to the closure contained in the
          // accumulator.
          //
          match self.asp.pop() {
            Some(AspValue::Mark) => {
              let Closure(c1, e1) = self.rsp.pop().unwrap();
              self.pc = c1;
              self.env_replace(e1);
            }
            Some(AspValue::Val(v)) => {
              if let Value::Fn(Closure(c1, e1)) = &self.accu {
                self.pc = *c1;
                let new_env = {
                  let mut e1 = e1.clone();
                  e1.push(v);
                  e1
                };
                self.env_replace(new_env);
              } else {
                panic!();
              }
            }
            None => panic!("asp empty"),
          }
        }
        Instruction::Let => {
          // Put the value of the accumulator in front of the environment.
          self.env_push(self.accu.clone());
          self.step(None);
        }
        Instruction::Letrec1 => {
          // Same as [Dummy; Cur ofs; Update], a frequent sequence
          // corresponding to [let rec f = function .. in ..].
          self.step(None);
          let new_env = vec![Value::FnRec(Closure(
            self.pc - (self.mem[self.pc as usize] as u32),
            vec![],
          ))];
          self.env_replace(new_env);
          self.step(None); // TODO Why? There's an extra zero here. Skip over it.
          self.step(None);
        }
        Instruction::Endlet => {
          // Throw away the first n local variables from the environment.
          self.step(None);
          let valofpc = self.mem[self.pc as usize];
          for _ in 0..valofpc {
            let _ = self.env.pop();
          }
          self.step(None);
        }
        Instruction::Endlet1 => {
          // Throw away the head of the environment.
          let _ = self.env.pop();
          self.step(None);
        }
        Instruction::Dummy => {
          // Place a Dummy in the environment.
          self.env_push(Value::Dummy);
          self.step(None);
        }
        Instruction::Update => {
          // Replace the head of the environment with the value in the
          // accumulator.
          if let Value::Dummy = self.env.pop().unwrap() {
            self.env_push(self.accu.clone());
          } else {
            panic!("expected a Dummy in the environment");
          }
          self.step(None);
        }
        Instruction::Succint => {
          if let Value::Int(i) = self.accu {
            self.accu = Value::Int(i + 1);
          } else {
            self.panic_pc("not an integer", instr);
          }
          self.step(None);
        }
        Instruction::Predint => {
          if let Value::Int(i) = self.accu {
            self.accu = Value::Int(i - 1);
          } else {
            self.panic_pc("not an integer", instr);
          }
          self.step(None);
        }
        Instruction::Negint => {
          if let Value::Int(i) = self.accu {
            self.accu = Value::Int(-i);
          } else {
            self.panic_pc("not an integer", instr);
          }
          self.step(None);
        }
        Instruction::Addint => {
          if let Value::Int(i) = self.accu {
            self.accu = match self.asp.pop().unwrap() {
              AspValue::Val(Value::Int(y)) => Value::Int(i + y),
              y => {
                self.panic_pc(&format!("not an integer in asp: {:?}", y), instr)
              }
            }
          } else {
            self.panic_pc("not an integer", instr);
          }
          self.step(None);
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
          self.step(None);
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
          self.step(None);
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
          self.step(None);
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
          self.step(None);
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
          self.step(None);
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
          self.step(None);
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
          self.step(None);
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
          self.step(None);
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
          self.step(None);
        }
        Instruction::Shiftrightintunsigned => {
          if let Value::Int(i) = self.accu {
            self.accu =
              if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
                Value::Int(((i as u8) >> y) as i32) // Can crash.
              } else {
                self.panic_pc("not an integer in asp", instr);
              }
          } else {
            self.panic_pc("not an integer", instr);
          }
          self.step(None);
        }
        Instruction::Floatop => {
          self.step(None);
          self.accu = match self.decode() {
            Instruction::Floatofint => {
              if let Value::Int(v) = self.accu {
                Value::Float(v as f32)
              } else {
                self.panic_pc("not an integer", instr);
              }
            }
            Instruction::Negfloat => {
              if let Value::Float(f) = self.accu {
                Value::Float(-f)
              } else {
                self.panic_pc("not a float", instr);
              }
            }
            Instruction::Addfloat => {
              if let Value::Float(x) = self.accu {
                if let Some(AspValue::Val(Value::Float(y))) = self.asp.pop() {
                  Value::Float(x + y)
                } else {
                  self.panic_pc("not a float in asp", instr);
                }
              } else {
                self.panic_pc("not a float in accu", instr);
              }
            }
            Instruction::Subfloat => {
              if let Value::Float(x) = self.accu {
                if let Some(AspValue::Val(Value::Float(y))) = self.asp.pop() {
                  Value::Float(x - y)
                } else {
                  self.panic_pc("not a float in asp", instr);
                }
              } else {
                self.panic_pc("not a float in accu", instr);
              }
            }
            Instruction::Mulfloat => {
              if let Value::Float(x) = self.accu {
                if let Some(AspValue::Val(Value::Float(y))) = self.asp.pop() {
                  Value::Float(x * y)
                } else {
                  self.panic_pc("not a float in asp", instr);
                }
              } else {
                self.panic_pc("not a float in accu", instr);
              }
            }
            Instruction::Divfloat => {
              if let Value::Float(x) = self.accu {
                match self.asp.pop() {
                  Some(AspValue::Val(Value::Float(y))) if y > 0.0 => {
                    Value::Float(x / y)
                  }
                  Some(AspValue::Val(Value::Float(_))) => {
                    self.panic_pc("division by zero", instr);
                  }
                  _ => self.panic_pc("not a float in asp", instr),
                }
              } else {
                self.panic_pc("not a float in accu", instr);
              }
            }
            _ => {
              self.panic_pc("not an instruction supported with floats", instr)
            }
          };
          self.step(None);
        }
        Instruction::Intoffloat => {
          self.accu = if let Value::Float(f) = self.accu {
            Value::Int(f as i32)
          } else {
            self.panic_pc("not a float", instr);
          };
          self.step(None);
        }
        Instruction::Boolnot => {
          self.accu = match self.accu {
            Value::False => Value::True,
            Value::True => Value::False,
            _ => panic!("not a bool in the accumulator"),
          };
          self.step(None);
        }
        Instruction::Constbyte => {
          self.step(None);
          self.accu = Value::Int(self.mem[self.pc as usize] as i32);
          self.step(None);
        }
        Instruction::Ccall1 => {
          self.step(None);
          let pnum = self.mem[self.pc as usize] as usize;
          if let Some(prim) = self.prims.get(pnum) {
            self.accu = prim(&[&self.accu]);
          } else {
            panic!("primitive number {pnum} undefined");
          };
          self.step(None);
        }
        Instruction::Ccall2 => {
          self.step(None);
          let pnum = self.mem[self.pc as usize] as usize;
          if let Some(prim) = self.prims.get(pnum) {
            let arg1 = if let Some(AspValue::Val(v)) = self.asp.pop() {
              v
            } else {
              panic!()
            };
            self.accu = prim(&[&self.accu, &arg1]);
          } else {
            panic!("primitive number {pnum} undefined");
          };
          self.step(None);
          self.step(None); // TODO Why is the prim id duplicated? (look at linker)
        }
        Instruction::Makestring => {
          self.step(None);
          let (val, len) =
            Value::string_from_bytes(&self.mem[self.pc as usize..]);
          self.accu = val;
          self.step(Some(len.try_into().unwrap()));
          self.step(None);
        }
        Instruction::Branch => {
          self.step(None);
          let jump = self.pc + (self.mem[self.pc as usize] as u32);
          self.pc = jump as u32;
        }
        Instruction::Branchifnot => {
          self.step(None);
          if let Value::False = self.accu {
            let jump = self.pc + (self.mem[self.pc as usize] as u32);
            self.pc = jump as u32;
          } else {
            self.step(None);
            self.step(None);
          }
        }
        _ => self.panic_pc("not implemented", instr), // TODO
      };
    }
  }

  pub fn report(&self) -> String {
    format!(
      "accumulator: {:?}
env max: {}
asp max: {}
rsp max: {}",
      self.accu,
      self.monitor.len_env_max,
      self.monitor.len_asp_max,
      self.monitor.len_rsp_max
    )
  }

  fn rsp_push(&mut self, val: Closure) {
    self.rsp.push(val);
    let len = self.rsp.len();
    if len > self.monitor.len_rsp_max {
      self.monitor.len_rsp_max = len;
    };
  }

  fn asp_push(&mut self, val: AspValue) {
    self.asp.push(val);
    let len = self.asp.len();
    if len > self.monitor.len_asp_max {
      self.monitor.len_asp_max = len;
    };
  }

  fn env_replace(&mut self, val: Vec<Value>) {
    self.env = val;
    let len = self.env.len();
    if len > self.monitor.len_env_max {
      self.monitor.len_env_max = len;
    };
  }

  fn env_push(&mut self, val: Value) {
    self.env.push(val);
    let len = self.env.len();
    if len > self.monitor.len_env_max {
      self.monitor.len_env_max = len;
    };
  }

  fn access_nth(&self, n: usize) -> &Value {
    let len = self.env.len();
    &self.env[(len - (n + 1))]
  }

  fn step(&mut self, n: Option<u8>) {
    match n {
      Some(n) => self.pc += n as u32,
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
  use super::{Machine, Value};
  use crate::runtime::opcodes;
  use crate::runtime::opcodes::{Instruction, Instruction::*};

  enum Code {
    I(Instruction), // Instruction
    D(u8),          // Data
  }

  impl Code {
    fn encode(c: &Code) -> u8 {
      match c {
        Code::I(i) => opcodes::encode(i),
        D(d) => *d,
      }
    }
  }

  use Code::*;

  #[test]
  fn machine_halts() {
    let program: Vec<u8> = vec![I(Constbyte), D(42), I(Stop)]
      .iter()
      .map(Code::encode)
      .collect();
    let mut machine = Machine::new(&program);
    let accu = machine.interpret();
    if let Value::Int(int) = accu {
      assert_eq!(int, 42);
    } else {
      panic!("not an integer");
    }
  }

  #[test]
  fn machine_can_add() {
    let program: Vec<u8> = vec![
      I(Constbyte),
      D(42),
      I(Push),
      I(Constbyte),
      D(1),
      I(Addint),
      I(Stop),
    ]
    .iter()
    .map(Code::encode)
    .collect();
    let mut machine = Machine::new(&program);
    let accu = machine.interpret();
    if let Value::Int(int) = accu {
      assert_eq!(int, 43);
    } else {
      panic!("not an integer");
    }
  }

  #[test]
  fn machine_can_sub() {
    let program: Vec<u8> = vec![
      I(Constbyte),
      D(1),
      I(Push),
      I(Constbyte),
      D(43),
      I(Subint),
      I(Stop),
    ]
    .iter()
    .map(Code::encode)
    .collect();
    let mut machine = Machine::new(&program);
    let accu = machine.interpret();
    if let Value::Int(int) = accu {
      assert_eq!(int, 42);
    } else {
      panic!("not an integer");
    }
  }

  #[test]
  fn machine_can_mul() {
    let program: Vec<u8> = vec![
      I(Constbyte),
      D(2),
      I(Push),
      I(Constbyte),
      D(21),
      I(Mulint),
      I(Stop),
    ]
    .iter()
    .map(Code::encode)
    .collect();
    let mut machine = Machine::new(&program);
    let accu = machine.interpret();
    if let Value::Int(int) = accu {
      assert_eq!(int, 42);
    } else {
      panic!("not an integer");
    }
  }

  #[test]
  fn machine_can_div() {
    let program: Vec<u8> = vec![
      I(Constbyte),
      D(2),
      I(Push),
      I(Constbyte),
      D(84),
      I(Divint),
      I(Stop),
    ]
    .iter()
    .map(Code::encode)
    .collect();
    let mut machine = Machine::new(&program);
    let accu = machine.interpret();
    if let Value::Int(int) = accu {
      assert_eq!(int, 42);
    } else {
      panic!("not an integer");
    }
  }

  // (\lambda x. x) 42
  #[test]
  fn machine_can_apply() {
    let program: Vec<u8> = vec![
      I(Constbyte),
      D(42),
      I(Push),
      I(Grab),
      I(Access),
      D(0),
      I(Stop),
    ]
    .iter()
    .map(Code::encode)
    .collect();
    let mut machine = Machine::new(&program);
    let accu = machine.interpret();
    if let Value::Int(int) = accu {
      assert_eq!(int, 42);
    } else {
      panic!("not an integer");
    }
  }

  // TODO: negative jump.
  // #[test]
  // fn machine_can_cur() {
  //   let program: Vec<u8> = vec![
  //     I(Constbyte),
  //     D(42),
  //     I(Push),
  //     I(Grab),
  //     I(Cur),
  //     D(-5),
  //     I(Stop),
  //   ]
  //   .iter()
  //   .map(Code::encode)
  //   .collect();
  //   let mut machine = Machine::new(&program);
  //   let accu = machine.interpret();
  //   match accu {
  //     Value::Fn(Closure(0, env)) => match &env[..] {
  //       [Value::Int(42)] => assert!(true),
  //       _ => panic!("environment does not match"),
  //     },
  //     Value::Fn(Closure(pc, _)) => panic!("wrong pc: {pc}"),
  //     _ => panic!("not a closure: {:?}", accu),
  //   }
  // }

  #[test]
  fn machine_can_print() {
    let mut program: Vec<Code> = vec![I(Makestring)];
    program.append(&mut ("42\0".as_bytes().iter().map(|b| D(*b)).collect()));
    program.push(I(Ccall1)); // Call primitive
    program.push(D(0)); // Primitive 0
    program.push(I(Stop));
    let program: Vec<u8> = program.iter().map(Code::encode).collect();
    let mut machine = Machine::new(&program);
    let accu = machine.interpret();
    match accu {
      Value::Int(0) => (),
      _ => panic!(),
    }
  }
}
