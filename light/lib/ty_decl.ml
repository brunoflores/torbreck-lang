(* Typing toplevel phrases *)

open Types
open Typing
open Modules
open Globals

let type_expression _loc expr =
  push_type_level ();
  let ty = type_expr [] expr in
  pop_type_level ();
  if is_nonexpansive expr then generalize_type ty;
  ty

let type_valuedecl _loc decl =
  let enter_val (name, typexp, prim) =
    push_type_level ();
    reset_type_expression_vars ();
    let ty = type_of_type_expression false typexp in
    pop_type_level ();
    generalize_type ty;
    add_value (defined_global name { val_typ = ty; val_prim = prim })
  in
  List.iter enter_val decl
