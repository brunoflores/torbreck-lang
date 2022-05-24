#[derive(ocaml::IntoValue, ocaml::FromValue, Debug)]
pub struct Atom {}

#[ocaml::func]
pub fn print_string(s: String) -> Atom {
  println!("print from Rust: {}", s);
  Atom {}
}
