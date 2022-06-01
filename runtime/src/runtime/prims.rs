pub type PrimFn = fn(String) -> i32;

pub fn prim_print_string(s: String) -> i32 {
  println!("{}", s);
  0
}
