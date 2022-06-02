use crate::runtime::machine::Value;

pub type PrimFn = fn(&Value) -> i32;

pub fn print_string(val: &Value) -> i32 {
  match val {
    Value::String(s) => {
      println!("{s}");
      0
    }
    a => panic!(
      "Prims::print_string: not a string in the accumulator: {:?}",
      a
    ),
  }
}
