(* Global symbol tables *)

open Const
open Prim

(* A reference to a global, in a source file, is either a qualified
   identifier mod__name, or an unqualified identifier name. *)
type global_reference = GRname of string | GRmodname of qualified_ident

(* Internally, a global is represented by its fully qualified name,
   plus associated information. *)
type 'a global = { qualid : qualified_ident; info : 'a }

let little_name_of_global g = g.qualid.id

(* Type expressions *)

type typ = { typ_desc : typ_desc; mutable typ_level : int }
and typ_desc = Tproduct of typ list

(* Labels *)

let no_type = { typ_desc = Tproduct []; typ_level = 0 }

(* Global variables *)

type value_desc = {
  val_typ : typ; (* Type *)
  val_prim : prim_desc; (* Is it a primitive? *)
}

and prim_desc =
  | ValueNotPrim
  | ValuePrim of int * primitive (* arity and implementation *)
