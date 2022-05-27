(* Constants *)

type qualified_ident = { qual : string; id : string } [@@deriving show]

type constr_tag =
  | ConstrExtensible of
      qualified_ident * int (* name of constructor and stamp *)
  | ConstrRegular of int * int (* tag number and number of constrs *)
[@@deriving show]

type atomic_constant =
  | ACint of int
  | ACfloat of float
  | ACstring of string
  | ACchar of char

and struct_constant =
  | SCatom of atomic_constant
  | SCblock of constr_tag * struct_constant list
[@@deriving show]

let const_unit = SCblock (ConstrRegular (0, 1), [])

let int_of_atom = function
  | ACint i -> i
  | ACchar c -> int_of_char c
  | _ -> failwith "int_of_atom"

let int_of_constr_tag = function
  | ConstrRegular (i, _) -> i
  | ConstrExtensible _ -> failwith "int_of_constr_tag"
