(* Global symbol tables *)

open Const
open Prim

(* A reference to a global, in a source file, is either a qualified
   identifier mod__name, or an unqualified identifier name. *)
type global_reference = GRname of string | GRmodname of qualified_ident
[@@deriving show]

(* Internally, a global is represented by its fully qualified name,
   plus associated information. *)
type 'a global = { qualid : qualified_ident; info : 'a } [@@deriving show]

let little_name_of_global g = g.qualid.id

(* Type constructors *)

type type_constr = {
  mutable ty_stamp : int; (* Stamp *)
  mutable ty_abbr : type_abbrev; (* Abbreviation or not *)
}

and type_abbrev =
  | Tnotabbrev
  | Tabbrev of typ list * typ (* Parameters and body *)

(* Type expressions *)
and typ = { typ_desc : typ_desc; mutable typ_level : int } [@@deriving show]

and typ_desc =
  | Tvar of typ_link (* A type variable *)
  | Tarrow of typ * typ (* A function type *)
  | Tproduct of typ list (* A tuple type *)
  | Tconstr of type_constr global * typ list (* A constructed type *)

and typ_link =
  | Tnolink (* Free variable *)
  | Tlinkto of typ (* Instantiated variable *)

(* Type constructor descriptions *)

type type_desc = {
  ty_constr : type_constr global; (* The constructor *)
  ty_arity : int; (* Its arity *)
  mutable ty_desc : type_components; (* Its description *)
}
[@@deriving show]

and type_components = Abstract_type | Variant_type of constr_desc global list

(* Value constructors *)
and constr_desc = {
  cs_res : typ; (* Result type *)
  cs_arg : typ; (* Argument type *)
  cs_mut : mutable_flag; (* Mutable or not *)
  cs_tag : constr_tag; (* Its run-time tag *)
  cs_kind : constr_kind; (* How it is represented *)
}

and mutable_flag = Mutable | Notmutable

and constr_kind =
  | Constr_constant (* Constant constructor *)
  | Constr_regular (* Usual constructor *)
  | Constr_superfluous of int (* Superfluous constructur with its arity *)

(* Labels *)

let generic = -1
and notgeneric = 0

let no_type = { typ_desc = Tproduct []; typ_level = 0 }

(* Global variables *)

type value_desc = {
  val_typ : typ; (* Type *)
  val_prim : prim_desc; (* Is it a primitive? *)
}
[@@deriving show]

and prim_desc =
  | ValueNotPrim
  | ValuePrim of int * primitive (* arity and implementation *)
