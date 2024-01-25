type qualified_ident = { qual : string; id : string } [@@deriving show]

type constr_tag =
  | ConstrExtensible of qualified_ident * int
  | ConstrRegular of int * int
[@@deriving show]

type atomic_constant =
  | ACint of int
  | ACfloat of float
  | ACstring of string
  | ACchar of char
[@@deriving show]

type struct_constant =
  | SCatom of atomic_constant
  | SCblock of constr_tag * struct_constant list
[@@deriving show]

let const_unit = SCblock (ConstrRegular (0, 1), [])
