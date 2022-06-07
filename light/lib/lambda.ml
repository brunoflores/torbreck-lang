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
  | Lfunction of lambda
  | Llet of lambda list * lambda
  | Lletrec of (lambda * int) list * lambda
  | Lprim of primitive * lambda list
  | Lcond of lambda * (atomic_constant * lambda) list
  | Lswitch of int * lambda * (constr_tag * lambda) list
  | Lstaticfail of int
  | Lstatichandle of lambda * lambda
  | Lhandle of lambda * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of lambda * lambda * bool * lambda
  | Lshared of lambda * int ref
  | Levent of event * lambda
[@@deriving show]

let share_lambda l = Lshared (l, ref (-1))

let rec has_guard = function
  | Lifthenelse (_, _, Lstaticfail _) -> true
  | Levent (_, l) -> has_guard l
  | Lshared (l, _) -> has_guard l
  | Llet (_, l2) -> has_guard l2
  | Lifthenelse
      ( _,
        _,
        ( Lvar _ | Lconst _
        | Lapply (_, _)
        | Lfunction _
        | Llet (_, _)
        | Lletrec (_, _)
        | Lprim (_, _)
        | Lcond (_, _)
        | Lswitch (_, _, _)
        | Lstatichandle (_, _)
        | Lhandle (_, _)
        | Lifthenelse (_, _, _)
        | Lsequence (_, _)
        | Lwhile (_, _)
        | Lfor (_, _, _, _)
        | Lshared (_, _)
        | Levent (_, _) ) )
  | Lvar _ | Lconst _
  | Lapply (_, _)
  | Lfunction _
  | Lletrec (_, _)
  | Lprim (_, _)
  | Lcond (_, _)
  | Lswitch (_, _, _)
  | Lstaticfail _
  | Lstatichandle (_, _)
  | Lhandle (_, _)
  | Lsequence (_, _)
  | Lwhile (_, _)
  | Lfor (_, _, _, _) ->
      false

let guard_expression l1 l2 npops = Lifthenelse (l1, l2, Lstaticfail npops)
