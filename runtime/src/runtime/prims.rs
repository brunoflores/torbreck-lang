use crate::runtime::machine::Value;

pub type PrimFn = fn(&[&Value]) -> Value;

pub fn print_endline(vals: &[&Value]) -> Value {
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

pub fn greaterequal(vals: &[&Value]) -> Value {
  if let (Value::Int(v1), Value::Int(v2)) = (vals[0], vals[1]) {
    if v1 >= v2 {
      Value::True
    } else {
      Value::False
    }
  } else {
    panic!()
  }
}

pub fn string_of_int(vals: &[&Value]) -> Value {
  if let Value::Int(v) = vals[0] {
    Value::String(v.to_string())
  } else {
    panic!()
  }
}
