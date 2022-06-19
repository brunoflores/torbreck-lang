(* Concrete syntax for primitive declarations *)

open Prim
open Globals

let primitive_names =
  [
    ("identity", Pidentity);
    ("field0", Pfield 0);
    ("setfield0", Psetfield 0);
    ("vect_length", Pvectlength);
    ("sequor", Psequor);
    ("sequand", Psequand);
    ("raise", Praise);
    ("get_vect_item", Pgetvectitem);
    ("+int", Paddint);
    ("-int", Psubint);
  ]

let find_primitive arity name =
  try ValuePrim (arity, List.assoc name primitive_names)
  with Not_found -> ValuePrim (arity, Pccall (name, arity))
