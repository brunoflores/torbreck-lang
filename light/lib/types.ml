(* Basic operations over types *)

open Globals

(* To take the canonical representative of a type *)
let type_repr ty = match ty.typ_desc with _ -> ty

(* The current nesting level of lets *)

let current_level = ref 0

let reset_type_var () = current_level := 0
and push_type_level () = incr current_level
and pop_type_level () = decr current_level

(* To generalize a type *)

let rec gen_type ty =
  let ty = type_repr ty in
  (match ty.typ_desc with
  | Tproduct ty_list -> ty.typ_level <- gen_type_list ty_list
  | Tconstr (_, ty_list) -> ty.typ_level <- gen_type_list ty_list);
  ty.typ_level

and gen_type_list = function
  | [] -> notgeneric
  | ty :: rest ->
      let lvl1 = gen_type ty in
      let lvl2 = gen_type_list rest in
      if lvl1 <= lvl2 then lvl1 else lvl2

let generalize_type ty =
  let _ = gen_type ty in
  ()
