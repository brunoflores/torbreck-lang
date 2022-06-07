(* The abstract syntax for the language *)

open Location
open Const
open Globals

type type_expression = { te_desc : type_expression_desc; te_loc : location }
[@@deriving show]

and type_expression_desc =
  | Ztypevar of string
  | Ztypearrow of type_expression * type_expression
  | Ztypeconstr of global_reference * type_expression list
[@@deriving show]

type pattern = { p_desc : pattern_desc; p_loc : location; mutable p_typ : typ }

and pattern_desc =
  | Zwildpat
  | Zvarpat of string
  | Zconstruct0pat of constr_desc global
  | Zconstruct1pat of constr_desc global * pattern
  | Zconstantpat of atomic_constant
  | Zorpat of pattern * pattern
  | Zaliaspat of pattern * string
  | Zconstraintpat of pattern * type_expression
  | Ztuplepat of pattern list
  | Zrecordpat of (label_desc global * pattern) list
[@@deriving show]

type expression = {
  e_desc : expression_desc;
  e_loc : location;
  mutable e_typ : typ;
}
[@@deriving show]

and expression_desc =
  | Zident of expr_ident ref
  | Zconstant of struct_constant
  | Zconstruct0 of constr_desc global
  | Zconstruct1 of constr_desc global * expression
  | Zapply of expression * expression list
  | Zlet of bool * (pattern * expression) list * expression
  | Zfunction of (pattern list * expression) list
  | Zcondition of expression * expression * expression
  | Zwhen of expression * expression
  | Ztuple of expression list
(* | Zconstraint of expression * type_expression *)
(* | Zrecord of (label_desc global * expression) list *)
(* | Zvector of expression list *)
(* | Zparser of (stream_pattern list * expression) list *)
(* | Ztrywith of expression * (pattern * expression) list *)
(* | Zsequence of expression * expression *)
(* | Zwhile of expression * expression *)
(* | Zfor of string * expression * expression * bool * expression *)
(* | Zassign of string * expression *)
(* | Zrecord_access of expression * label_desc global *)
(* | Zrecord_update of expression * label_desc global * expression *)
(* | Zstream of stream_component list *)
[@@deriving show]

and expr_ident = Zglobal of value_desc global | Zlocal of string
[@@deriving show]

type impl_phrase = { im_desc : impl_desc; im_loc : location }

and impl_desc =
  | Zexpr of expression
  | Zletdef of bool * (pattern * expression) list
(* | Ztypedef of (string * string list * type_decl) list *)
(* | Zexcdef of constr_decl list *)
(* | Zimpldirective of directiveu *)
[@@deriving show]

type intf_phrase = { in_desc : intf_desc; in_loc : location }

and intf_desc = Zvaluedecl of (string * type_expression * prim_desc) list
[@@deriving show]

(* type intf_phrase = { in_desc : intf_desc; in_loc : location } *)

(* and intf_desc = *)
(*   | Zvaluedecl of (string * type_expression * prim_desc) list *)
(*   | Ztypedecl of (string * string list * type_decl) list *)
(*   | Zexcdecl of constr_decl list *)
(*   | Zintfdirective of directiveu *)

let rec expr_is_pure expr =
  match expr.e_desc with
  | Zident _ -> true
  | Zconstant _ -> true
  | Zconstruct0 _ -> true
  | Zfunction _ -> true
  | Zapply _ -> false
  | Zlet _ -> false
  | Zcondition _ -> false
  | Zwhen _ -> false
  | Ztuple el -> List.for_all expr_is_pure el
  | Zconstruct1 (_, arg) -> expr_is_pure arg

let single_constructor cstr =
  match cstr.info.cs_tag with
  | ConstrRegular (_, span) -> span == 1
  | ConstrExtensible (_, _) -> false

let rec pat_irrefutable pat =
  match pat.p_desc with
  | Zwildpat -> true
  | Zvarpat _ -> true
  | Zaliaspat (pat, _) -> pat_irrefutable pat
  | Zconstantpat _ -> false
  | Ztuplepat patl -> List.for_all pat_irrefutable patl
  | Zconstruct0pat cstr -> single_constructor cstr
  | Zconstruct1pat (cstr, pat) -> single_constructor cstr && pat_irrefutable pat
  | Zorpat (pat1, pat2) -> pat_irrefutable pat1 || pat_irrefutable pat2
  | Zconstraintpat (pat, _) -> pat_irrefutable pat
  | Zrecordpat lbl_pat_list ->
      List.for_all (fun (_lbl, pat) -> pat_irrefutable pat) lbl_pat_list
