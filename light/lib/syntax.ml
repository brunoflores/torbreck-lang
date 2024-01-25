(** The abstract syntax output from the parser. *)

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
  | Ztrywith of expression * (pattern * expression) list
  | Zwhile of expression * expression
  | Zsequence of expression * expression
(* | Zconstraint of expression * type_expression *)
(* | Zrecord of (label_desc global * expression) list *)
(* | Zvector of expression list *)
(* | Zparser of (stream_pattern list * expression) list *)
(* | Zfor of string * expression * expression * bool * expression *)
(* | Zassign of string * expression *)
(* | Zrecord_access of expression * label_desc global *)
(* | Zrecord_update of expression * label_desc global * expression *)
(* | Zstream of stream_component list *)
[@@deriving show]

and expr_ident = Zglobal of value_desc global | Zlocal of string
[@@deriving show]

type type_decl = Zabstract_type | Zvariant_type of constr_decl list
[@@deriving show]

and constr_decl =
  | Zconstr0decl of string
  | Zconstr1decl of string * type_expression * mutable_flag
[@@deriving show]

type directive = Zdir of string * string [@@deriving show]

type type_def = { id : string; params : string list; decl : type_decl }
[@@deriving show]

type implementation =
  | Zexpr of expression  (** Expression defined in implementation *)
  | Zletdef of {
      recflag : bool;  (** Whether this is a recursive let *)
      binders : (pattern * expression) list;  (** All simultaneous binders *)
    }  (** Let defined in implementation *)
  | Zimpldirective of directive  (** Implementation directive *)
  | Zexcdef of constr_decl list  (** Exception defined in implementation *)
  | Ztypedef of type_def list
      (** Simultaneous type definitions in implementation *)
[@@deriving show]

type impl_phrase = { im_desc : implementation; im_loc : location }
(** Implementation phrase from concrete syntax *)

type value_decl = { id : string; expr : type_expression; prim : prim_desc }
[@@deriving show]

type interface =
  | Zvaluedecl of value_decl list  (** Value declaration in interface *)
  | Ztypedecl of type_def list  (** Type declaration in interface *)
  | Zintfdirective of directive  (** Interface directive *)
  | Zexcdecl of constr_decl list  (** Exception declared in interface *)
[@@deriving show]

(* and intf_desc = *)
(*   | Zvaluedecl of (string * type_expression * prim_desc) list *)
(*   | Ztypedecl of (string * string list * type_decl) list *)

type intf_phrase = { in_desc : interface; in_loc : location }
(** Interface phrase from concrete syntax *)

let rec free_vars_of_pat pat =
  match pat.p_desc with
  | Zwildpat -> []
  | Zconstantpat _ -> []
  | Zconstruct0pat _ -> []
  | Zvarpat v -> [ v ]
  | Zaliaspat (pat, v) -> v :: free_vars_of_pat pat
  | Ztuplepat patl -> List.concat_map free_vars_of_pat patl
  | Zconstruct1pat (_, pat) -> free_vars_of_pat pat
  | Zorpat (pat1, pat2) -> free_vars_of_pat pat1 @ free_vars_of_pat pat2
  | Zconstraintpat (pat, _) -> free_vars_of_pat pat
  | Zrecordpat lbl_pat_list ->
      List.concat_map (fun (_lbl, pat) -> free_vars_of_pat pat) lbl_pat_list

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
  | Ztrywith _ -> false
  | Zwhile _ -> false
  | Zsequence _ -> false
  | Ztuple el -> List.for_all expr_is_pure el
  | Zconstruct1 (_, arg) -> expr_is_pure arg

let letdef_is_pure pat_expr_list =
  List.for_all (fun (_pat, expr) -> expr_is_pure expr) pat_expr_list

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
