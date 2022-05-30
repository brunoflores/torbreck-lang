(* The intermediate language:
   extended lambda-calculus in de Bruijn's notation *)

open Globals
open Const
open Prim

(* Structure of compilation environments *)

type access_path =
  | Path_root
  | Path_son of int * access_path
  | Path_tuple of access_path list
[@@deriving show]

type lambda_variable = {
  var_name : string;
  var_path : access_path;
  var_typ : typ;
}
[@@deriving show]

type transl_env =
  | Tnullenv
  | Treserved of transl_env
  | Tenv of lambda_variable list * transl_env
[@@deriving show]

(* Debugging events *)

type event_kind = Lbefore | Lafter of typ [@@deriving show]

type event = {
  ev_kind : event_kind;
  ev_file : string;
  ev_char : int;
  ev_env : transl_env;
  mutable ev_pos : int;
}
[@@deriving show]

(* The intermediate language *)

type lambda =
  | Lvar of int
  | Lconst of struct_constant
  | Lapply of lambda * lambda list
  | Lprim of primitive * lambda list
  | Levent of event * lambda
[@@deriving show]
