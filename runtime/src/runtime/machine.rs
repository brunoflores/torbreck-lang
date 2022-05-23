use crate::runtime::opcodes;
use crate::runtime::opcodes::Instruction;

// TODO Public?
#[derive(Debug, Clone)]
pub struct Closure(u8, Vec<Value>);

// TODO Public?
// Value is either a closure representing a function, or a constant.
#[derive(Debug, Clone)]
pub enum Value {
  Closure(Closure),
  // ConcreteTy { tag: u8, constructors: Vec<Value> },
  Int(i8),
  // Float(f32),
  // Bool(bool),
  // Record,         // TODO
  // Bytes(Vec<u8>), // Sequence of bytes.
  Dummy,
  Atom0,
}

// impl Value {
//   fn empty_from_tag(t: u8) -> Value {
//     match t {
//       0 => Value::Closure(Closure(0, vec![])),
//       // 1 => Value::ConcreteTy {
//       //   tag: 0,
//       //   constructors: vec![],
//       // },
//       // 2 => Value::Record,
//       // 3 => Value::Bytes(vec![]),
//       _ => panic!("unknown tag: {}", t),
//     }
//   }
// }

// Value in the argument stack is either a value or a mark.
#[derive(Debug)]
enum AspValue {
  Val(Value),
  Mark,
}

#[derive(Debug)]
pub struct Machine<'a> {
  pc: u8,             // Code pointer.
  mem: &'a [u8],      // Program memory in bytes.
  env: Vec<Value>,    // Current environment.
  asp: Vec<AspValue>, // Argument stack.
  rsp: Vec<Closure>,  // Return stack.
  accu: Value,        // Accumulator for intermediate results.
}

impl<'a> Machine<'a> {
  pub fn new(mem: &'a [u8]) -> Self {
    Machine {
      mem,
      pc: 0,
      accu: Value::Atom0,

      // TODO Consider Vec::with_capacity
      env: vec![],
      asp: vec![],
      rsp: vec![],
    }
  }

  pub fn interpret(&'a mut self) -> Value {
    loop {
      let instr = self.decode();
      match instr {
        Instruction::Stop => return self.accu.clone(),
        Instruction::Access => {
          // Place in the accumulator the local variable of index mem[pc+1].
          // Leave the rest of state untouched.
          self.accu =
            self.env[self.mem[(self.pc + 1) as usize] as usize].clone();
          self.step(Some(2));
        }
        Instruction::Appterm => {
          // Application in tail-call position.
          // There is no need to push a mark in the argument stack.
          if let Value::Closure(Closure(c1, e1)) = &self.accu {
            self.pc = *c1;
            self.env = {
              let mut e1 = e1.clone();
              e1.push(if let AspValue::Val(v) = self.asp.pop().unwrap() {
                v
              } else {
                panic!();
              });
              e1
            };
          } else {
            panic!();
          }
        }
        Instruction::Apply => {
          // Application using the return stack.
          if let Value::Closure(Closure(c1, e1)) = &self.accu {
            // Read current state...
            self.rsp.push(Closure(self.pc, self.env.clone()));
            // now modify it...
            self.pc = *c1;
            self.env = {
              let mut e1 = e1.clone();
              e1.push(if let AspValue::Val(v) = self.asp.pop().unwrap() {
                v
              } else {
                panic!();
              });
              e1
            };
          } else {
            panic!();
          }
        }
        Instruction::Push => {
          // Push the accumulator onto the argument stack,
          // leave the accumulator untouched, and
          // take a step.
          self.asp.push(AspValue::Val(self.accu.clone()));
          self.step(None);
        }
        Instruction::Pushmark => {
          // Push a mark onto the argument stack, and
          // take step.
          self.asp.push(AspValue::Mark);
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
              self.env.push(v);
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
              self.accu = Value::Closure(Closure(self.pc, self.env.clone()));
              // now modify it...
              self.pc = c1;
              self.env = e1;
            }
            _ => self.panic_pc("argument stack is empty", instr),
          }
        }
        Instruction::Cur => {
          // Abstraction using the stack.
          self.accu = Value::Closure(Closure(self.pc, self.env.clone()));
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
              self.env = e1;
            }
            Some(AspValue::Val(v)) => {
              if let Value::Closure(Closure(c1, e1)) = &self.accu {
                self.pc = *c1;
                self.env = {
                  let mut e1 = e1.clone();
                  e1.push(v);
                  e1
                }
              } else {
                panic!();
              }
            }
            None => panic!(),
          }
        }
        Instruction::Let => {
          // Put the value of the accumulator in front of the environment.
          self.env.push(self.accu.clone());
          self.step(None);
        }
        // Instruction::Letrec1 => {
        //   // Same as [Dummy; Cur ofs; Update], a frequent sequence
        //   // corresponding to [let ref f = function .. in ..].
        //   self.step(None);
        //   self.rsp.push(Closure(self.pc, self.env.clone()));
        //   self.step(None);
        //   self.pc = self.pc + self.mem[self.pc as usize];
        //   self.step(None);
        // }
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
          self.env.push(Value::Dummy);
          self.step(None);
        }
        Instruction::Update => {
          // Replace the head of the environment with the value in the
          // accumulator.
          if let Value::Dummy = self.env.pop().unwrap() {
            self.env.push(self.accu.clone());
          } else {
            panic!("expected a Dummy in the environment");
          }
          self.step(None);
        }

        //         Instruction::Pop => {
        //           self.accu = match self.asp.pop() {
        //             Some(AspValue::Val(value)) => value,
        //             Some(AspValue::Mark) => self.panic_pc("popping a mark", instr),
        //             None => self.panic_pc("popping an empty argument stack", instr),
        //           };
        //           self.step(None);
        //         }
        //         Instruction::Makeblock1 => {
        //           self.accu = match Value::empty_from_tag(self.mem[self.pc as usize]) {
        //             Value::Closure { code: _, env: _ } => Rc::new(Value::Closure {
        //               code: if let Value::Int(i) = *self.accu {
        //                 i as u8 // Can crash.
        //               } else {
        //                 self.panic_pc("not an integer value", instr);
        //               },
        //               env: vec![],
        //             }),
        //             Value::ConcreteTy {
        //               tag: _,
        //               constructors: _,
        //             } => Rc::new(Value::ConcreteTy {
        //               // TODO Wild guess.
        //               tag: if let Value::Int(i) = *self.accu {
        //                 i as u8 // Can crash.
        //               } else {
        //                 self.panic_pc("not an integer value", instr);
        //               },
        //               constructors: vec![],
        //             }),
        //             _ => panic!("not implemented"), // TODO
        //           };
        //           self.step(None);
        //         }
        //         Instruction::Makeblock2 => {
        //           self.accu = match Value::empty_from_tag(self.mem[self.pc as usize]) {
        //             Value::Closure { code: _, env: _ } => Rc::new(Value::Closure {
        //               code: if let Value::Int(i) = *self.accu {
        //                 i as u8 // Can crash.
        //               } else {
        //                 self.panic_pc("not an integer value", instr);
        //               },
        //               env: if let Some(AspValue::Val(v)) = self.asp.pop() {
        //                 vec![*v]
        //               } else {
        //                 // Do nothing.
        //                 // TODO Should we crash?
        //                 vec![]
        //               },
        //             }),
        //             Value::ConcreteTy {
        //               tag: _,
        //               constructors: _,
        //             } => Rc::new(Value::ConcreteTy {
        //               // TODO Wild guess.
        //               tag: if let Value::Int(i) = *self.accu {
        //                 i as u8 // Can crash.
        //               } else {
        //                 self.panic_pc("not an integer value", instr);
        //               },
        //               constructors: if let Some(AspValue::Val(v)) = self.asp.pop() {
        //                 vec![*v]
        //               } else {
        //                 // Do nothing.
        //                 // TODO Should we crash?
        //                 vec![]
        //               },
        //             }),
        //             _ => panic!("not implemented"), // TODO
        //           };
        //           self.step(None);
        //         }
        //         Instruction::Makeblock3 => {
        //           self.panic_pc("same sa above", instr);
        //         }
        //         Instruction::Makeblock4 => {
        //           self.panic_pc("same sa above", instr);
        //         }
        //         Instruction::Makeblock => {
        //           self.panic_pc("same sa above", instr); // TODO Maybe?
        //         }
        //         Instruction::Getfield0 => {
        //           self.accu = match self.accu {
        //             Value::Closure { code, env: _ } => Value::Int(code as i8),
        //             Value::ConcreteTy {
        //               tag,
        //               constructors: _,
        //             } => Value::Int(tag as i8),
        //             _ => self.panic_pc("not implemented", instr),
        //           };
        //           self.step(None);
        //         }
        //         Instruction::Getfield1 => {
        //           self.accu = match &self.accu {
        //             Value::Closure { code: _, env } => env[0].clone(), // Can panic.
        //             Value::ConcreteTy {
        //               tag: _,
        //               constructors,
        //             } => constructors[0].clone(), // Can panic.
        //             _ => self.panic_pc("not implemented", instr),
        //           };
        //           self.step(None);
        //         }
        //         Instruction::Getfield2 => {
        //           self.panic_pc("not implemented", instr); // TODO
        //         }
        //         Instruction::Getfield3 => {
        //           self.panic_pc("not implemented", instr); // TODO
        //         }
        //         Instruction::Getfield => {
        //           self.step(None);
        //           self.accu = match &self.accu {
        //             Value::Closure { code: _, env } => {
        //               env[self.mem[self.pc as usize] as usize].clone()
        //             } // Can panic.
        //             Value::ConcreteTy {
        //               tag: _,
        //               constructors,
        //             } => constructors[self.mem[self.pc as usize] as usize].clone(), // Can panic.
        //             _ => self.panic_pc("not implemented", instr),
        //           };
        //           self.step(None);
        //         }
        //         Instruction::Setfield0 => {
        //           self.accu = match &self.accu {
        //             Value::Closure { code: _, env } => {
        //               if let Some(AspValue::Val(Value::Int(code))) = self.asp.pop() {
        //                 Value::Closure {
        //                   code: code as u8, // Can crash.
        //                   env: env.clone(),
        //                 }
        //               } else {
        //                 panic!();
        //               }
        //             }
        //             _ => panic!(),
        //           };
        //           self.step(None);
        //         }
        //         Instruction::Setfield1 => {
        //           self.accu = match self.accu {
        //             Value::Closure { code, env: _ } => {
        //               if let Some(AspValue::Val(v)) = self.asp.pop() {
        //                 Value::Closure { code, env: vec![v] }
        //               } else {
        //                 panic!();
        //               }
        //             }
        //             _ => panic!(),
        //           };
        //           self.step(None);
        //         }
        //         Instruction::Setfield2 => {
        //           self.panic_pc("not implemented", instr); // TODO
        //         }
        //         Instruction::Setfield3 => {
        //           self.panic_pc("not implemented", instr); // TODO
        //         }
        //         Instruction::Setfield => {
        //           self.panic_pc("not implemented", instr); // TODO
        //         }
        //         Instruction::Succint => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu = Value::Int(i + 1);
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Predint => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu = Value::Int(i - 1);
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Negint => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu = Value::Int(-i);
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Addint => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu =
        //               if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
        //                 Value::Int(i + y)
        //               } else {
        //                 self.panic_pc("not an integer in asp", instr);
        //               }
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Subint => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu =
        //               if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
        //                 Value::Int(i - y)
        //               } else {
        //                 self.panic_pc("not an integer in asp", instr);
        //               }
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Mulint => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu =
        //               if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
        //                 Value::Int(i * y)
        //               } else {
        //                 self.panic_pc("not an integer in asp", instr);
        //               }
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Divint => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu = match self.asp.pop() {
        //               Some(AspValue::Val(Value::Int(y))) if y > 0 => Value::Int(i / y),
        //               Some(AspValue::Val(Value::Int(_))) => {
        //                 self.panic_pc("division by zero", instr)
        //               }
        //               _ => self.panic_pc("not an integer in asp", instr),
        //             }
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Modint => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu = match self.asp.pop() {
        //               Some(AspValue::Val(Value::Int(y))) if y > 0 => Value::Int(i % y),
        //               Some(AspValue::Val(Value::Int(_))) => {
        //                 self.panic_pc("division by zero", instr)
        //               }
        //               _ => self.panic_pc("not an integer in asp", instr),
        //             }
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Andint => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu =
        //               if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
        //                 Value::Int(i & y)
        //               } else {
        //                 self.panic_pc("not an integer in asp", instr);
        //               }
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Orint => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu =
        //               if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
        //                 Value::Int(i | y)
        //               } else {
        //                 self.panic_pc("not an integer in asp", instr);
        //               }
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Xorint => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu =
        //               if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
        //                 Value::Int(i ^ y)
        //               } else {
        //                 self.panic_pc("not an integer in asp", instr);
        //               }
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Shiftleftint => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu =
        //               if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
        //                 Value::Int(i << y)
        //               } else {
        //                 self.panic_pc("not an integer in asp", instr);
        //               }
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Shiftrightintsigned => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu =
        //               if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
        //                 Value::Int(i >> y)
        //               } else {
        //                 self.panic_pc("not an integer in asp", instr);
        //               }
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Shiftrightintunsigned => {
        //           if let Value::Int(i) = self.accu {
        //             self.accu =
        //               if let Some(AspValue::Val(Value::Int(y))) = self.asp.pop() {
        //                 Value::Int(((i as u8) >> y) as i8) // Can crash.
        //               } else {
        //                 self.panic_pc("not an integer in asp", instr);
        //               }
        //           } else {
        //             self.panic_pc("not an integer", instr);
        //           }
        //         }
        //         Instruction::Floatop => {
        //           self.step(None);
        //           self.accu = match self.decode() {
        //             Instruction::Floatofint => {
        //               if let Value::Int(v) = self.accu {
        //                 Value::Float(v as f32)
        //               } else {
        //                 self.panic_pc("not an integer", instr);
        //               }
        //             }
        //             Instruction::Negfloat => {
        //               if let Value::Float(f) = self.accu {
        //                 Value::Float(-f)
        //               } else {
        //                 self.panic_pc("not a float", instr);
        //               }
        //             }
        //             Instruction::Addfloat => {
        //               if let Value::Float(x) = self.accu {
        //                 if let Some(AspValue::Val(Value::Float(y))) = self.asp.pop() {
        //                   Value::Float(x + y)
        //                 } else {
        //                   self.panic_pc("not a float in asp", instr);
        //                 }
        //               } else {
        //                 self.panic_pc("not a float in accu", instr);
        //               }
        //             }
        //             Instruction::Subfloat => {
        //               if let Value::Float(x) = self.accu {
        //                 if let Some(AspValue::Val(Value::Float(y))) = self.asp.pop() {
        //                   Value::Float(x - y)
        //                 } else {
        //                   self.panic_pc("not a float in asp", instr);
        //                 }
        //               } else {
        //                 self.panic_pc("not a float in accu", instr);
        //               }
        //             }
        //             Instruction::Mulfloat => {
        //               if let Value::Float(x) = self.accu {
        //                 if let Some(AspValue::Val(Value::Float(y))) = self.asp.pop() {
        //                   Value::Float(x * y)
        //                 } else {
        //                   self.panic_pc("not a float in asp", instr);
        //                 }
        //               } else {
        //                 self.panic_pc("not a float in accu", instr);
        //               }
        //             }
        //             Instruction::Divfloat => {
        //               if let Value::Float(x) = self.accu {
        //                 match self.asp.pop() {
        //                   Some(AspValue::Val(Value::Float(y))) if y > 0.0 => {
        //                     Value::Float(x / y)
        //                   }
        //                   Some(AspValue::Val(Value::Float(_))) => {
        //                     self.panic_pc("division by zero", instr);
        //                   }
        //                   _ => self.panic_pc("not a float in asp", instr),
        //                 }
        //               } else {
        //                 self.panic_pc("not a float in accu", instr);
        //               }
        //             }
        //             _ => {
        //               self.panic_pc("not an instruction supported with floats", instr)
        //             }
        //           };
        //           self.step(None);
        //         }
        //         Instruction::Intoffloat => {
        //           self.accu = if let Value::Float(f) = self.accu {
        //             Value::Int(f as i8)
        //           } else {
        //             self.panic_pc("not a float", instr);
        //           };
        //           self.step(None);
        //         }
        //         Instruction::Boolnot => {
        //           self.accu = match self.accu {
        //             Value::Atom0 => Value::Bool(true),
        //             Value::Atom1 => Value::Bool(false),
        //             _ => panic!(),
        //           }
        //         }
        _ => self.panic_pc("not implemented", instr), // TODO
      };
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
