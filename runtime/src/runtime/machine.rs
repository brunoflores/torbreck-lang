use crate::runtime::opcodes;
use crate::runtime::opcodes::Instruction;
use crate::runtime::prims;

use std::mem;

// TODO Public?
#[derive(Debug, Clone)]
pub struct Closure(
  usize,      // Program pointer
  Vec<Value>, // Environment
);

// TODO Public?
#[derive(Debug, Clone)]
pub enum Value {
  // Basic types
  Int(i32),
  Float(f32),
  String(String),
  True,
  False,
  Dummy,
  Atom0,
  // Compound types
  Vec(Vec<Value>),
  Fn(Closure),
  FnRec(Closure),
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
  pc: usize,                      // Code pointer.
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
  pub fn new(
    mem: &'machine [u8],
    globals: &'machine [u8],
    args: &'machine [String],
  ) -> Self {
    // Debug:
    // println!("{:?}", mem);
    // println!("{:?}", globals);

    let number_of_globals: u32 = if !globals.is_empty() {
      // In this case we take four bytes as an unsigned 32-bit
      // integer that would have been created by OCaml's
      // [Stdlib.output_binary_int] in our linker.
      u32::from_be_bytes(globals[0..4].try_into().unwrap())
    } else {
      0
    };
    let mut global_vals: Vec<Value> =
      Vec::with_capacity((number_of_globals + 1) as usize);
    // Position after the 32-bit integer that told us about the number of
    // globals in the bytecode.

    // Command line arguments
    let mut args_as_vals = Vec::with_capacity(args.len());
    for s in args.iter() {
      args_as_vals.push(Value::String(s.clone()));
    }
    global_vals.push(Value::Vec(args_as_vals));

    let mut pos = 4;
    for _ in 0..number_of_globals {
      // TODO: Do not assume always strings in the globals section.
      let (val, len) = Value::string_from_bytes(&globals[pos..]);
      pos += len;
      // Jump over the null byte that terminates the string.
      pos += 1;

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
      // println!("{:?}", self.globals);
      // println!("instr: {}", self.instr);
      // println!("accu: {:?}", self.accu);
      // println!("env: {:?}", self.env);
      // println!("astack: {:?}", self.astack);
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
          self.pc += 1;
        }
        Instruction::Predint => {
          if let Value::Int(i) = self.accu {
            self.accu = Value::Int(i - 1);
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.pc += 1;
        }
        Instruction::Negint => {
          if let Value::Int(i) = self.accu {
            self.accu = Value::Int(-i);
          } else {
            self.panic_pc("not an integer", self.instr);
          }
          self.pc += 1;
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
          self.pc += 1;
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
          self.pc += 1;
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
          self.pc += 1;
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
          self.pc += 1;
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
          self.pc += 1;
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
          self.pc += 1;
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
          self.pc += 1;
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
          self.pc += 1;
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
          self.pc += 1;
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
          self.pc += 1;
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
          self.pc += 1;
        }
        Instruction::Floatop => {
          self.pc += 1;
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
          self.pc += 1;
        }
        Instruction::Intoffloat => {
          self.accu = if let Value::Float(f) = self.accu {
            Value::Int(f as i32)
          } else {
            self.panic_pc("not a float", self.instr);
          };
          self.pc += 1;
        }
        Instruction::Boolnot => {
          self.accu = match self.accu {
            Value::False => Value::True,
            Value::True => Value::False,
            _ => panic!("not a bool in the accumulator"),
          };
          self.pc += 1;
        }
        Instruction::Constbyte => {
          self.pc += 1;
          self.accu = Value::Int(self.mem[self.pc as usize] as i32);
          self.pc += 1;
        }
        Instruction::Ccall1 => {
          self.pc += 1;
          let u16pc = self.u16pc();
          if let Some(prim) = self.prims.get(u16pc as usize) {
            self.accu = prim(&[&self.accu]);
          } else {
            panic!("primitive number undefined: {u16pc}");
          };
          self.pc += 2; // Jump over short
        }
        Instruction::Ccall2 => {
          self.pc += 1;
          let u16pc = self.u16pc();
          if let Some(prim) = self.prims.get(u16pc as usize) {
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
            panic!("primitive number undefined: {u16pc}");
          };
          self.pc += 2; // Jump over short
        }
        Instruction::Makestring => {
          self.pc += 1;
          let (val, len) =
            Value::string_from_bytes(&self.mem[self.pc as usize..]);
          self.accu = val;
          self.pc += len + 1;
        }
        Instruction::Branch => {
          self.pc += 1;
          self.pc = (self.pc as i32 + self.i32pc()) as usize;
        }
        Instruction::Branchifnot => {
          self.pc += 1;
          match self.accu {
            Value::False => self.pc = (self.pc as i32 + self.i32pc()) as usize,
            Value::True => {
              self.pc += 2; // Jump over short
            }
            _ => panic!("not a boolean in the accumulator"),
          }
        }
        Instruction::Branchif => {
          self.pc += 1;
          match self.accu {
            Value::True => self.pc = (self.pc as i32 + self.i32pc()) as usize,
            Value::False => {
              self.pc += 2; // Jump over short
            }
            _ => panic!("not a boolean in the accumulator"),
          }
        }
        Instruction::Setglobal => {
          self.pc += 1;
          let u16pc = self.u16pc();
          if u16pc as usize >= self.globals.len() {
            self.realloc_globals();
          };
          self.globals[u16pc as usize] = self.accu.clone();
          self.pc += 2; // Jump over short
        }
        Instruction::Getglobal => {
          self.pc += 1;
          self.accu = self.globals[self.u16pc() as usize].clone();
          self.pc += 2; // Jump over short
        }
        Instruction::Pushgetglobalapply => {
          self.pc += 1;

          let u16pc = self.u16pc();
          let old_accu =
            mem::replace(&mut self.accu, self.globals[u16pc as usize].clone());
          self.asp += 1;
          self.astack[self.asp] = Some(AspValue::Val(old_accu));

          self.pc += 1; // Jump over short
          self.exec_apply();
        }
        Instruction::Vectlength => {
          self.accu = match &self.accu {
            Value::Vec(v) => Value::Int(v.len().try_into().unwrap()),
            _ => panic!(),
          };
          self.pc += 1;
        }
        Instruction::Getvectitem => {
          self.accu = if let Some(AspValue::Val(Value::Int(i))) =
            self.astack[self.asp].take()
          {
            self.asp -= 1;
            match &self.accu {
              Value::Vec(vec) => vec[i as usize].clone(),
              _ => panic!(),
            }
          } else {
            panic!();
          };
          self.pc += 1;
        }
        _ => self.panic_pc("not implemented", self.instr),
      };
    }
  }

  #[inline(always)]
  fn realloc_globals(&mut self) {
    self.globals.resize(self.globals.len() * 2, Value::Dummy);
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
    self.pc += 1;
    self.accu = self.access_nth(self.mem[self.pc as usize] as usize).clone();
    self.pc += 1;
  }

  #[inline(always)]
  fn exec_access_0(&mut self) {
    self.accu = self.access_nth(0).clone();
    self.pc += 1;
  }

  #[inline(always)]
  fn exec_access_1(&mut self) {
    self.accu = self.access_nth(1).clone();
    self.pc += 1;
  }

  #[inline(always)]
  fn exec_appterm(&mut self) {
    if let Value::Fn(Closure(c1, e1)) = &self.accu {
      self.pc = *c1;
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
    self.pc += 1;
  }

  #[inline(always)]
  fn exec_pushmark(&mut self) {
    self.asp += 1;
    self.astack[self.asp] = Some(AspValue::Mark);
    self.pc += 1;
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
        self.pc += 1;
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
    self.pc += 1;
    let displacement = self.i32pc();
    self.accu = Value::Fn(Closure(
      ((self.pc as i32) + displacement) as usize,
      self.env.clone(),
    ));
    self.pc += 2; // Jump over short
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
    self.pc += 1;
  }

  #[inline(always)]
  fn exec_letrec1(&mut self) {
    self.pc += 1;
    self.env.clear();
    self.env.push(Value::FnRec(Closure(
      ((self.pc as i32) + self.i32pc()) as usize,
      Vec::with_capacity(0), // TODO
    )));
    self.pc += 2; // Jump over short
  }

  #[inline(always)]
  fn exec_endlet(&mut self) {
    self.pc += 1;
    let valofpc = self.mem[self.pc as usize];
    for _ in 0..valofpc {
      let _ = self.env.pop();
    }
    self.pc += 1;
  }

  #[inline(always)]
  fn exec_endlet1(&mut self) {
    let _ = self.env.pop();
    self.pc += 1;
  }

  #[inline(always)]
  fn exec_dummy(&mut self) {
    self.env.push(Value::Dummy);
    self.pc += 1;
  }

  #[inline(always)]
  fn exec_update(&mut self) {
    if let Value::Dummy = self.env.pop().unwrap() {
      self.env.push(self.accu.clone());
    } else {
      panic!("expected a Dummy in the environment");
    }
    self.pc += 1;
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

  #[inline(always)]
  fn access_nth(&self, n: usize) -> &Value {
    let len = self.env.len();
    &self.env[(len - (n + 1))]
  }

  #[inline(always)]
  fn decode(&self) -> Instruction {
    if let Some(i) = self.mem.get(self.pc as usize) {
      opcodes::decode(*i)
    } else {
      panic!("instruction out of bounds: {}", self.pc);
    }
  }

  fn panic_pc(&self, msg: &str, instr: Instruction) -> ! {
    panic!("pc: {}: {}: {}", self.pc, instr, msg);
  }
}

#[cfg(test)]
mod tests {
  use super::{Closure, Machine, Value};
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
    let mut machine = Machine::new(&program, &[], &[]);
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
    let mut machine = Machine::new(&program, &[], &[]);
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
    let mut machine = Machine::new(&program, &[], &[]);
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
    let mut machine = Machine::new(&program, &[], &[]);
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
    let mut machine = Machine::new(&program, &[], &[]);
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
    let mut machine = Machine::new(&program, &[], &[]);
    let accu = machine.interpret();
    if let Value::Int(int) = accu {
      assert_eq!(int, 42);
    } else {
      panic!("not an integer");
    }
  }

  #[test]
  fn machine_can_do_negative_jump() {
    let program: Vec<u8> = vec![
      I(Constbyte),
      D(42),
      I(Push),
      I(Grab),
      I(Cur),
      // -5 in base 10 (this is a 16-bit short).
      D(0b11111011),
      D(0b11111111),
      I(Stop),
    ]
    .iter()
    .map(Code::encode)
    .collect();
    let mut machine = Machine::new(&program, &[], &[]);
    let accu = machine.interpret();
    match accu {
      Value::Fn(Closure(0, env)) => match &env[..] {
        [Value::Int(42)] => assert!(true),
        _ => panic!("environment does not match"),
      },
      Value::Fn(Closure(pc, _)) => panic!("wrong pc: {pc}"),
      _ => panic!("not a closure: {:?}", accu),
    }
  }

  #[test]
  fn machine_can_print() {
    let mut program: Vec<Code> = vec![I(Makestring)];
    program.append(&mut ("42\0".as_bytes().iter().map(|b| D(*b)).collect()));
    program.push(I(Ccall1));
    // Primitive id 0: two bytes to make it a short
    program.push(D(0));
    program.push(D(0));
    program.push(I(Stop));
    let program: Vec<u8> = program.iter().map(Code::encode).collect();
    let mut machine = Machine::new(&program, &[], &[]);
    let accu = machine.interpret();
    match accu {
      Value::Int(0) => (),
      _ => panic!(),
    }
  }

  #[test]
  fn machine_can_get_global() {
    let program: Vec<u8> = vec![
      I(Getglobal),
      // 1 in base 10 (this is a 16-bit short).
      // Skip index 0 which is reserved for command line arguments.
      D(0b00000001),
      D(0),
      I(Stop),
    ]
    .iter()
    .map(Code::encode)
    .collect();

    // A 32-bit integer that would have been created by OCaml's
    // [Stdlib.output_binary_int].
    let mut globals: Vec<Code> =
      vec![D(0b00000000), D(0b00000000), D(0b00000000), D(0b00000001)];
    globals.append(&mut ("hello\0".as_bytes().iter().map(|b| D(*b)).collect()));
    let globals: Vec<u8> = globals.iter().map(Code::encode).collect();

    let mut machine = Machine::new(&program, &globals, &[]);
    let accu = machine.interpret();
    match accu {
      Value::String(val) => assert_eq!(val, "hello"),
      _ => panic!(),
    }
  }
}
