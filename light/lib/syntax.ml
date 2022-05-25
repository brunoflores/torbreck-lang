(* The abstract syntax for the language *)

open Location
open Const
open Globals

type pattern

type expression = {
  e_desc : expression_desc;
  e_loc : location;
  mutable e_typ : typ;
}

and expression_desc = Zident of expr_ident ref | Zconstant of struct_constant
(* | Ztuple of expression list *)
(* | Zconstruct0 of constr_desc global *)
(* | Zconstruct1 of constr_desc global * expression *)
(* | Zapply of expression * expression list *)
(* | Zlet of bool * (pattern * expression) list * expression *)
(* | Zfunction of (pattern list * expression) list *)
(* | Ztrywith of expression * (pattern * expression) list *)
(* | Zsequence of expression * expression *)
(* | Zcondition of expression * expression * expression *)
(* | Zwhile of expression * expression *)
(* | Zfor of string * expression * expression * bool * expression *)
(* | Zconstraint of expression * type_expression *)
(* | Zvector of expression list *)
(* | Zassign of string * expression *)
(* | Zrecord of (label_desc global * expression) list *)
(* | Zrecord_access of expression * label_desc global *)
(* | Zrecord_update of expression * label_desc global * expression *)
(* | Zstream of stream_component list *)
(* | Zparser of (stream_pattern list * expression) list *)
(* | Zwhen of expression * expression *)

and expr_ident = Zglobal of value_desc global | Zlocal of string

type impl_phrase = { im_desc : impl_desc; im_loc : location }

and impl_desc =
  | Zexpr of expression
  | Zletdef of bool * (pattern * expression) list
(* | Ztypedef of (string * string list * type_decl) list *)
(* | Zexcdef of constr_decl list *)
(* | Zimpldirective of directiveu *)

(* type intf_phrase = { in_desc : intf_desc; in_loc : location } *)

(* and intf_desc = *)
(*   | Zvaluedecl of (string * type_expression * prim_desc) list *)
(*   | Ztypedecl of (string * string list * type_decl) list *)
(*   | Zexcdecl of constr_decl list *)
(*   | Zintfdirective of directiveu *)