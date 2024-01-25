(** Constants. *)

type qualified_ident = {
  qual : string;  (** Qualifier *)
  id : string;  (** Identifier *)
}
[@@deriving show]
(** Type of qualified identifiers. In concrete syntax,
    [ident__ident] is called a qualified name. *)

(** Constructor tags. *)
type constr_tag =
  | ConstrExtensible of qualified_ident * int
      (** Name of constructor and numeric stamp *)
  | ConstrRegular of int * int  (** Tag number and number of constructors *)
[@@deriving show]

(** Atomic constants. *)
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
