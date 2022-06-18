use crate::runtime::opcodes;
use crate::runtime::opcodes::Instruction;
use crate::runtime::prims;

use std::mem;

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

pub struct Machine<'machine> {
  instr: Instruction,
  pc: u32,                        // Code pointer.
  mem: &'machine [u8],            // Program memory in bytes.
  env: Vec<Value>,                // Current environment (heap allocated).
  astack: [Option<AspValue>; 60], // Argument stack.
  rstack: [Option<Closure>; 30],  // Return stack.
  asp: usize,                     // Argument stack pointer
  rsp: usize,                     // Return stack pointer
  accu: Value,                    // Accumulator for intermediate results.
  prims: [prims::PrimFn; 4],      // Primitives table
  globals: Vec<Value>,            // Table of global values
                                  // cache size TODO
}

#[allow(clippy::needless_lifetimes)]
impl<'machine> Machine<'machine> {
  pub fn new(mem: &'machine [u8], globals: &'machine [u8]) -> Self {
    // Debug:
    // println!("{:?}", mem);
    // println!("{:?}", globals);

    let mut globals_iter = globals.iter();
    let mut number_of_globals: u32 = 0;
    if !globals.is_empty() {
      for p in 0..4 {
        let n = globals_iter.next().unwrap();
        number_of_globals = (n >> (8 * p)) as u32;
      }
    }
    let mut global_vals: Vec<Value> =
      Vec::with_capacity(number_of_globals as usize);
    let mut pos = 4;
    for _ in 0..number_of_globals {
      // TODO: Do not assume always strings in the globals section.
      let (val, len) = Value::string_from_bytes(&globals[pos..]);
      pos += len;
      pos += 1; // Skip the null byte

      global_vals.push(val);

      // Debug:
      // println!("{} {:?}", len, val);
    }

    // Debug:
    // println!("{:?}", global_vals);

    Machine {
      instr: Instruction::Stop,
      mem,
      pc: 0,
      accu: Value::Atom0,

      env: Vec::with_capacity(100),
      astack: [
        None, None, None, None, None, None, None, None, None, None, None, None,
        None, None, None, None, None, None, None, None, None, None, None, None,
        None, None, None, None, None, None, None, None, None, None, None, None,
        None, None, None, None, None, None, None, None, None, None, None, None,
        None, None, None, None, None, None, None, None, None, None, None, None,
      ],
      rstack: [
        None, None, None, None, None, None, None, None, None, None, None, None,
        None, None, None, None, None, None, None, None, None, None, None, None,
        None, None, None, None, None, None,
      ],
      asp: 0,
      rsp: 0,

      // Primitives table
      prims: [
        prims::print_endline,
        prims::less_than,
        prims::string_of_int,
        prims::greaterequal,
      ],

      globals: global_vals,
    }
  }

  pub fn interpret<'a>(&'a mut self) -> Value {
    loop {
      self.instr = self.decode();
      // Debug:
      // println!("{:?}", self.accu);
      // println!("{}", instr);
      match self.instr {
        Instruction::Stop => return self.accu.clone(),
        Instruction::Access => {
          self.exec_access();
        }
        Instruction::Acc0 => {
          self.exec_access_0();
        }
        Instruction::Acc1 => {
          self.exec_access_1();
        }
        Instruction::Appterm => {
          // Application in tail-call position.
          // There is no need to push a mark in the argument stack.
          self.exec_appterm();
        }
        Instruction::Apply => {
          // Application using the return stack.
          self.exec_apply();
        }
        Instruction::Push => {
          // Push the accumulator onto the argument stack,
          // leave the accumulator untouched, and
          // take a step.
          self.exec_push();
        }
        Instruction::Pushmark => {
          // Push a mark onto the argument stack, and
          // take step.
          self.exec_pushmark();
        }
        Instruction::Grab => {
          // Abstraction in tail-call position.
          self.exec_grab();
        }
        Instruction::Cur => {
          // Abstraction using the stack.
          self.exec_cur();
        }
        Instruction::Return => {
          // Page 80:
          //
          // If there is a mark on top of the stack, pop it and return to
          // the caller. Otherwise, jump to the closure contained in the
          // accumulator.
          //
          self.exec_return();
        }
        Instruction::Let => {
          // Put the value of the accumulator in front of the environment.
          self.exec_let();
        }
        Instruction::Letrec1 => {
          // Same as [Dummy; Cur ofs; Update], a frequent sequence
          // corresponding to [let rec f = function .. in ..].
          self.exec_letrec1();
        }
        Instruction::Endlet => {
          // Throw away the first n local variables from the environment.
          self.exec_endlet();
        }
        Instruction::Endlet1 => {
          // Throw away the head of the environment.
          self.exec_endlet1();
        }
        Instruction::Dummy => {
          // Place a Dummy in the environment.
          self.exec_dummy();
        }
        Instruction::Update => {
          // Replace the head of the environment with the value in the
          // accumulator.
          self.exec_update();
        }
        Instruction::Succint => {
          if let Value::Int(i) = self.accu {
            self.accu = Value::Int(i + 1);
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.step(None);
        }
        Instruction::Predint => {
          if let Value::Int(i) = self.accu {
            self.accu = Value::Int(i - 1);
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.step(None);
        }
        Instruction::Negint => {
          if let Value::Int(i) = self.accu {
            self.accu = Value::Int(-i);
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.step(None);
        }
        Instruction::Addint => {
          if let Value::Int(i) = self.accu {
            self.accu = match self.astack[self.asp].take().unwrap() {
              AspValue::Val(Value::Int(y)) => Value::Int(i + y),
              y => self.panic_pc(
                &format!("not an integer in asp: {:?}", y),
                self.instr,
              ),
            };
            self.asp -= 1;
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.step(None);
        }
        Instruction::Subint => {
          if let Value::Int(i) = self.accu {
            self.accu = if let Some(AspValue::Val(Value::Int(y))) =
              self.astack[self.asp]
            {
              self.asp -= 1;
              Value::Int(i - y)
            } else {
              self.panic_pc("not an integer in asp", self.instr);
            }
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.step(None);
        }
        Instruction::Mulint => {
          if let Value::Int(i) = self.accu {
            self.accu = if let Some(AspValue::Val(Value::Int(y))) =
              self.astack[self.asp]
            {
              self.asp -= 1;
              Value::Int(i * y)
            } else {
              self.panic_pc("not an integer in asp", self.instr);
            }
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.step(None);
        }
        Instruction::Divint => {
          if let Value::Int(i) = self.accu {
            self.accu = match self.astack[self.asp] {
              Some(AspValue::Val(Value::Int(y))) if y > 0 => {
                self.asp -= 1;
                Value::Int(i / y)
              }
              Some(AspValue::Val(Value::Int(_))) => {
                self.panic_pc("division by zero", self.instr)
              }
              _ => self.panic_pc("not an integer in asp", self.instr),
            }
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.step(None);
        }
        Instruction::Modint => {
          if let Value::Int(i) = self.accu {
            self.accu = match self.astack[self.asp] {
              Some(AspValue::Val(Value::Int(y))) if y > 0 => {
                self.asp -= 1;
                Value::Int(i % y)
              }
              Some(AspValue::Val(Value::Int(_))) => {
                self.panic_pc("division by zero", self.instr)
              }
              _ => self.panic_pc("not an integer in asp", self.instr),
            }
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.step(None);
        }
        Instruction::Andint => {
          if let Value::Int(i) = self.accu {
            self.accu = if let Some(AspValue::Val(Value::Int(y))) =
              self.astack[self.asp]
            {
              self.asp -= 1;
              Value::Int(i & y)
            } else {
              self.panic_pc("not an integer in asp", self.instr);
            }
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.step(None);
        }
        Instruction::Orint => {
          if let Value::Int(i) = self.accu {
            self.accu = if let Some(AspValue::Val(Value::Int(y))) =
              self.astack[self.asp]
            {
              self.asp -= 1;
              Value::Int(i | y)
            } else {
              self.panic_pc("not an integer in asp", self.instr);
            }
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.step(None);
        }
        Instruction::Xorint => {
          if let Value::Int(i) = self.accu {
            self.accu = if let Some(AspValue::Val(Value::Int(y))) =
              self.astack[self.asp]
            {
              self.asp -= 1;
              Value::Int(i ^ y)
            } else {
              self.panic_pc("not an integer in asp", self.instr);
            }
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.step(None);
        }
        Instruction::Shiftleftint => {
          if let Value::Int(i) = self.accu {
            self.accu = if let Some(AspValue::Val(Value::Int(y))) =
              self.astack[self.asp]
            {
              self.asp -= 1;
              Value::Int(i << y)
            } else {
              self.panic_pc("not an integer in asp", self.instr);
            }
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.step(None);
        }
        Instruction::Shiftrightintsigned => {
          if let Value::Int(i) = self.accu {
            self.accu = if let Some(AspValue::Val(Value::Int(y))) =
              self.astack[self.asp]
            {
              self.asp -= 1;
              Value::Int(i >> y)
            } else {
              self.panic_pc("not an integer in asp", self.instr);
            }
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.step(None);
        }
        Instruction::Shiftrightintunsigned => {
          if let Value::Int(i) = self.accu {
            self.accu = if let Some(AspValue::Val(Value::Int(y))) =
              self.astack[self.asp]
            {
              self.asp -= 1;
              Value::Int(((i as u8) >> y) as i32) // Can crash.
            } else {
              self.panic_pc("not an integer in asp", self.instr);
            }
          } else {
            self.panic_pc("not an integer", self.instr);
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
                self.panic_pc("not an integer", self.instr);
              }
            }
            Instruction::Negfloat => {
              if let Value::Float(f) = self.accu {
                Value::Float(-f)
              } else {
                self.panic_pc("not a float", self.instr);
              }
            }
            Instruction::Addfloat => {
              if let Value::Float(x) = self.accu {
                if let Some(AspValue::Val(Value::Float(y))) =
                  self.astack[self.asp]
                {
                  self.asp -= 1;
                  Value::Float(x + y)
                } else {
                  self.panic_pc("not a float in asp", self.instr);
                }
              } else {
                self.panic_pc("not a float in accu", self.instr);
              }
            }
            Instruction::Subfloat => {
              if let Value::Float(x) = self.accu {
                if let Some(AspValue::Val(Value::Float(y))) =
                  self.astack[self.asp]
                {
                  self.asp -= 1;
                  Value::Float(x - y)
                } else {
                  self.panic_pc("not a float in asp", self.instr);
                }
              } else {
                self.panic_pc("not a float in accu", self.instr);
              }
            }
            Instruction::Mulfloat => {
              if let Value::Float(x) = self.accu {
                if let Some(AspValue::Val(Value::Float(y))) =
                  self.astack[self.asp]
                {
                  self.asp -= 1;
                  Value::Float(x * y)
                } else {
                  self.panic_pc("not a float in asp", self.instr);
                }
              } else {
                self.panic_pc("not a float in accu", self.instr);
              }
            }
            Instruction::Divfloat => {
              if let Value::Float(x) = self.accu {
                match self.astack[self.asp] {
                  Some(AspValue::Val(Value::Float(y))) if y > 0.0 => {
                    self.asp -= 1;
                    Value::Float(x / y)
                  }
                  Some(AspValue::Val(Value::Float(_))) => {
                    self.panic_pc("division by zero", self.instr);
                  }
                  _ => self.panic_pc("not a float in asp", self.instr),
                }
              } else {
                self.panic_pc("not a float in accu", self.instr);
              }
            }
            _ => self
              .panic_pc("not an instruction supported with floats", self.instr),
          };
          self.step(None);
        }
        Instruction::Intoffloat => {
          self.accu = if let Value::Float(f) = self.accu {
            Value::Int(f as i32)
          } else {
            self.panic_pc("not a float", self.instr);
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
          self.step(None); // TODO Why is the prim id duplicated? (look at linker)
        }
        Instruction::Ccall2 => {
          self.step(None);
          let pnum = self.mem[self.pc as usize] as usize;
          if let Some(prim) = self.prims.get(pnum) {
            let arg1 = if let Some(AspValue::Val(v)) =
              self.astack[self.asp].take()
            {
              v
            } else {
              panic!("Ccall2 expects the second argument in the argument stack")
            };
            self.asp -= 1;
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
        Instruction::Getglobal => {
          self.step(None);
          self.accu = self.globals[self.mem[self.pc as usize] as usize].clone();
          self.step(None); // TODO: global id is duplicated.
          self.step(None);
        }
        _ => self.panic_pc("not implemented", self.instr), // TODO
      };
    }
  }

  #[inline(always)]
  fn u16pc(&self) -> u16 {
    u16::from_be_bytes([
      self.mem[(self.pc + 1) as usize],
      self.mem[self.pc as usize],
    ])
  }

  #[inline(always)]
  fn i16pc(&self) -> i16 {
    i16::from_be_bytes([
      self.mem[(self.pc + 1) as usize],
      self.mem[self.pc as usize],
    ])
  }

  #[inline(always)]
  fn i32pc(&self) -> i32 {
    // Logical arith shift
    self.i16pc() as i32
  }

  #[inline(always)]
  fn exec_access(&mut self) {
    self.step(None);
    self.accu = self.access_nth(self.mem[self.pc as usize] as usize).clone();
    self.step(None);
  }

  #[inline(always)]
  fn exec_access_0(&mut self) {
    self.accu = self.access_nth(0).clone();
    self.step(None);
  }

  #[inline(always)]
  fn exec_access_1(&mut self) {
    self.accu = self.access_nth(1).clone();
    self.step(None);
  }

  #[inline(always)]
  fn exec_appterm(&mut self) {
    if let Value::Fn(Closure(c1, e1)) = &self.accu {
      self.pc = *c1 as u32;
      let mut new_env = {
        let mut e1 = e1.clone();
        e1.push(
          if let AspValue::Val(v) = self.astack[self.asp].take().unwrap() {
            self.asp -= 1;
            v
          } else {
            panic!("not a value in the asp");
          },
        );
        e1
      };
      self.env = mem::take(&mut new_env);
    } else {
      panic!();
    }
  }

  #[inline(always)]
  fn exec_apply(&mut self) {
    let oldpc = self.pc;
    if let Value::Fn(Closure(c1, e1)) = &self.accu {
      self.pc = *c1;
      let new_env = {
        let mut e1 = e1.clone();
        e1.push(
          if let AspValue::Val(v) = self.astack[self.asp].take().unwrap() {
            self.asp -= 1;
            v
          } else {
            panic!("not a value in the asp");
          },
        );
        e1
      };
      self.rsp += 1;
      self.rstack[self.rsp] =
        Some(Closure(oldpc + 1, mem::replace(&mut self.env, new_env)));
    } else if let Value::FnRec(Closure(c1, _)) = &self.accu {
      self.pc = *c1;
      let new_env = {
        let mut e1 = Vec::with_capacity(10); // TODO
        e1.push(self.accu.clone());
        if let Some(AspValue::Val(v)) = self.astack[self.asp].take() {
          e1.push(v);
          self.asp -= 1;
        };
        e1
      };
      self.rsp += 1;
      self.rstack[self.rsp] =
        Some(Closure(oldpc + 1, mem::replace(&mut self.env, new_env)));
    } else {
      panic!("not a closure in the accumulator: {:?}", self.accu);
    }
  }

  #[inline(always)]
  fn exec_push(&mut self) {
    self.asp += 1;
    self.astack[self.asp] = Some(AspValue::Val(self.accu.clone()));
    self.step(None);
  }

  #[inline(always)]
  fn exec_pushmark(&mut self) {
    self.asp += 1;
    self.astack[self.asp] = Some(AspValue::Mark);
    self.step(None);
  }

  #[inline(always)]
  fn exec_grab(&mut self) {
    match self.astack[self.asp].take() {
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
        let Closure(c1, mut e1) = self.rstack[self.rsp].take().unwrap();
        self.rsp -= 1;
        // Read current state...
        self.accu = Value::Fn(Closure(self.pc, self.env.clone()));
        // now modify it...
        self.pc = c1;
        self.env = mem::take(&mut e1);
      }
      _ => self.panic_pc("argument stack is empty", self.instr),
    };
    self.asp -= 1;
  }

  #[inline(always)]
  fn exec_cur(&mut self) {
    self.step(None);
    let displacement = self.i32pc();
    self.accu = Value::Fn(Closure(
      ((self.pc as i32) + displacement) as u32,
      self.env.clone(),
    ));
    self.step(None); // Jump over short
    self.step(None);
  }

  #[inline(always)]
  fn exec_return(&mut self) {
    match self.astack[self.asp].take() {
      Some(AspValue::Mark) => {
        let Closure(c1, mut e1) = self.rstack[self.rsp].take().unwrap();
        self.rsp -= 1;
        self.pc = c1;
        self.env = mem::take(&mut e1);
      }
      Some(AspValue::Val(v)) => {
        if let Value::Fn(Closure(c1, e1)) = &self.accu {
          self.pc = *c1;
          let mut new_env = {
            let mut e1 = e1.clone();
            e1.push(v);
            e1
          };
          self.env = mem::take(&mut new_env);
        } else {
          panic!("not a closure in the accumulator");
        }
      }
      None => panic!("asp empty: {}", self.asp),
    }
    self.asp -= 1;
  }

  #[inline(always)]
  fn exec_let(&mut self) {
    self.env.push(self.accu.clone());
    self.step(None);
  }

  #[inline(always)]
  fn exec_letrec1(&mut self) {
    self.step(None);
    self.env.clear();
    self.env.push(Value::FnRec(Closure(
      self.pc - (self.mem[self.pc as usize] as u32),
      Vec::with_capacity(0), // TODO
    )));
    // TODO Why? There's an extra zero here. Skip over it.
    self.step(None);
    self.step(None);
  }

  #[inline(always)]
  fn exec_endlet(&mut self) {
    self.step(None);
    let valofpc = self.mem[self.pc as usize];
    for _ in 0..valofpc {
      let _ = self.env.pop();
    }
    self.step(None);
  }

  #[inline(always)]
  fn exec_endlet1(&mut self) {
    let _ = self.env.pop();
    self.step(None);
  }

  #[inline(always)]
  fn exec_dummy(&mut self) {
    self.env.push(Value::Dummy);
    self.step(None);
  }

  #[inline(always)]
  fn exec_update(&mut self) {
    if let Value::Dummy = self.env.pop().unwrap() {
      self.env.push(self.accu.clone());
    } else {
      panic!("expected a Dummy in the environment");
    }
    self.step(None);
  }

  // #[inline(always)]
  // fn pop_asp(&mut self) -> &Option<AspValue> {
  //   // TODO check not empty to handle gracefully?
  //   let val = &self.astack[self.asp];
  //   self.asp -= 1;
  //   val
  // }

  // #[inline(always)]
  // fn push_asp(&mut self, val: AspValue) {
  //   self.asp += 1;
  //   self.astack[self.asp] = Some(val);
  // }

  // pub fn report(&self) -> String {
  //   format!("accumulator: {:?}", self.accu)
  // }

  fn access_nth(&self, n: usize) -> &Value {
    let len = self.env.len();
    &self.env[(len - (n + 1))]
  }

  #[inline(always)]
  fn step(&mut self, n: Option<u8>) {
    match n {
      Some(n) => self.pc += n as u32,
      None => self.pc += 1,
    }
  }

  #[inline(always)]
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
