(* Basic operations over types *)

open Globals

(* To take the canonical representative of a type *)
let type_repr ty = match ty.typ_desc with _ -> ty

(* The current nesting level of lets *)

let current_level = ref 0

let reset_type_var () = current_level := 0
and push_type_level () = incr current_level
and pop_type_level () = decr current_level

(* To get fresh type variables *)

let new_global_type_var () = { typ_desc = Tvar Tnolink; typ_level = 1 }

(* To generalize a type *)

let rec gen_type ty =
  let ty = type_repr ty in
  begin
    match ty.typ_desc with
    | Tvar _ -> if ty.typ_level > !current_level then ty.typ_level <- generic
    | Tarrow (t1, t2) ->
        let lvl1 = gen_type t1 in
        let lvl2 = gen_type t2 in
        ty.typ_level <- (if lvl1 <= lvl2 then lvl1 else lvl2)
    | Tproduct ty_list -> ty.typ_level <- gen_type_list ty_list
    | Tconstr (_, ty_list) -> ty.typ_level <- gen_type_list ty_list
  end;
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
