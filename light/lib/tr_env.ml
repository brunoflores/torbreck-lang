(* Handling of the translation environment *)

open Lambda
open Prim

let translate_path root =
  let rec transl = function
    | Path_root -> root
    | Path_son (n, p) -> Lprim (Pfield n, [ transl p ])
    | Path_tuple pl ->
        Lprim (Pmakeblock (ConstrRegular (0, 1)), List.map transl pl)
  in
  transl

let rec find_var name = function
  | [] -> raise Not_found
  | v :: vs -> if v.var_name = name then v else find_var name vs

let translate_access s env =
  let rec transl i = function
    | Tnullenv -> failwith "translate_access"
    | Treserved env -> transl (i + 1) env
    | Tenv (lambda_var_list, env) -> (
        try
          let var = find_var s lambda_var_list in
          translate_path (Lvar i) var.var_path
        with Not_found -> transl (i + 1) env)
  in
  transl 0 env
