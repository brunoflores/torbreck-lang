(* Typing toplevel phrases *)

open Types
open Typing

let type_expression _loc expr =
  push_type_level ();
  let ty = type_expr [] expr in
  pop_type_level ();
  if is_nonexpansive expr then generalize_type ty;
  ty
