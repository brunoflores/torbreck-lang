(* Type inference *)

open Syntax
open Const
open Builtins

(* Check if an expression is non-expansive, that is, the result of its
   evaluation cannot contain newly created mutable objects. *)
let is_nonexpansive expr =
  match expr.e_desc with Zident _ -> true | Zconstant _ -> true

let type_of_atomic_constant = function
  | ACint _ -> type_int
  | _ -> failwith "not implemented: Typing.type_of_atomic_constant"

let type_of_structured_constant = function
  | SCatom ac -> type_of_atomic_constant ac
  | _ -> failwith "not implemented: Typing.type_of_structured_constant"

let type_expr _env expr =
  let inferred_ty =
    match expr.e_desc with
    | Zconstant c -> type_of_structured_constant c
    | _ -> failwith "not implemented: Typing.type_expr"
  in
  expr.e_typ <- inferred_ty;
  inferred_ty
