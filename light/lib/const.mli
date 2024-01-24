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
[@@deriving show]

type struct_constant =
  | SCatom of atomic_constant
  | SCblock of constr_tag * struct_constant list
[@@deriving show]

val const_unit : struct_constant
val int_of_constr_tag : constr_tag -> int
val int_of_atom : atomic_constant -> int
