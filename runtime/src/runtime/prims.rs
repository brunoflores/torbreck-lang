use crate::runtime::machine::Value;

pub type PrimFn = fn(&[&Value]) -> Value;

pub fn print_string(vals: &[&Value]) -> Value {
  match vals[0] {
    Value::String(s) => {
      println!("{s}");
      Value::Int(0)
    }
    a => {
      panic!(
        "Prims::print_string: not a string in the accumulator: {:?}",
        a
      )
    }
  }
}

pub fn less_than(vals: &[&Value]) -> Value {
  if let (Value::Int(v1), Value::Int(v2)) = (vals[0], vals[1]) {
    if v1 < v2 {
      Value::True
    } else {
      Value::False
    }
  } else {
    panic!()
  }
}

pub fn int_add(vals: &[&Value]) -> Value {
  if let (Value::Int(v1), Value::Int(v2)) = (vals[0], vals[1]) {
    Value::Int(v1 + v2)
  } else {
    panic!()
  }
}

pub fn int_sub(vals: &[&Value]) -> Value {
  if let (Value::Int(v1), Value::Int(v2)) = (vals[0], vals[1]) {
    Value::Int(v1 - v2)
  } else {
    panic!()
  }
}
