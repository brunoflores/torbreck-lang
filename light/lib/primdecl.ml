(* Concrete syntax for primitive declarations *)

open Prim
open Globals

let primitive_names = [ ("print_string", Pprintstring) ]

let find_primitive arity name =
  try ValuePrim (arity, List.assoc name primitive_names)
  with Not_found -> ValuePrim (arity, Pccall (name, arity))