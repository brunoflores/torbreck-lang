(* The pre-defined global identifiers *)

open Const
open Globals

let builtin n d = { qualid = { qual = "builtin"; id = n }; info = d }

(* Some types that must be known to the type checker *)
let constr_type_int = builtin "int" { ty_stamp = 5; ty_abbr = Tnotabbrev }

let type_int =
  { typ_desc = Tconstr (constr_type_int, []); typ_level = notgeneric }
