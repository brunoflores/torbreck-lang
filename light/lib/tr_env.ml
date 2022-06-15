(* Handling of the translation environment *)

open Lambda
open Prim
open Syntax
open Globals
open Error

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

let rec pat_is_named pat =
  match pat.p_desc with
  | Zvarpat _s -> true
  | Zaliaspat (_pat, _s) -> true
  | Zconstraintpat (pat, _) -> pat_is_named pat
  | _ -> false

let tuple_path nfields path =
  let rec list_of_fields i =
    if i >= nfields then [] else Path_son (i, path) :: list_of_fields (succ i)
  in
  Path_tuple (list_of_fields 0)

let rec paths_of_pat path pat =
  match pat.p_desc with
  | Zvarpat s -> [ { var_name = s; var_path = path; var_typ = pat.p_typ } ]
  | Zaliaspat (pat, s) ->
      { var_name = s; var_path = path; var_typ = pat.p_typ }
      :: paths_of_pat path pat
  | Ztuplepat patlist ->
      let rec paths_of_patlist i = function
        | [] -> []
        | p :: pl ->
            paths_of_pat (Path_son (i, path)) p @ paths_of_patlist (i + 1) pl
      in
      paths_of_patlist 0 patlist
  | Zconstruct0pat _cstr -> []
  | Zconstruct1pat (cstr, p) -> begin
      match cstr.info.cs_kind with
      | Constr_superfluous n ->
          paths_of_pat (if pat_is_named p then tuple_path n path else path) p
      | _ -> paths_of_pat (Path_son (0, path)) p
    end
  | Zconstraintpat (pat, _) -> paths_of_pat path pat
  | Zrecordpat lbl_pat_list ->
      let rec paths_of_lbl_pat_list = function
        | [] -> []
        | (lbl, p) :: pl ->
            paths_of_pat (Path_son (lbl.info.lbl_pos, path)) p
            @ paths_of_lbl_pat_list pl
      in
      paths_of_lbl_pat_list lbl_pat_list
  | _ -> []

let rec mutable_vars_of_pat mut pat =
  match pat.p_desc with
  | Zvarpat v ->
      if mut then
        [ { var_name = v; var_typ = pat.p_typ; var_path = Path_root } ]
      else []
  | Zaliaspat (pat, v) ->
      let l = mutable_vars_of_pat mut pat in
      if mut then
        { var_name = v; var_typ = pat.p_typ; var_path = Path_root } :: l
      else l
  | Zconstraintpat (pat, _) -> mutable_vars_of_pat mut pat
  | Ztuplepat patl -> List.concat_map (mutable_vars_of_pat mut) patl
  | Zconstruct1pat (cstr, pat) ->
      let mut' =
        match cstr.info.cs_mut with Mutable -> true | Notmutable -> mut
      in
      mutable_vars_of_pat mut' pat
  | Zrecordpat lbl_pat_list ->
      List.concat_map
        (fun (lbl, pat) ->
          let mut' =
            match lbl.info.lbl_mut with Mutable -> true | Notmutable -> mut
          in
          mutable_vars_of_pat mut' pat)
        lbl_pat_list
  | _ -> [] (* Zwildpat or Zconstpat or Zorpat *)

let rec add_lets_to_env varlist env =
  match varlist with
  | [] -> env
  | var :: rest -> add_lets_to_env rest (Tenv ([ var ], env))

let add_lets_to_expr varlist env expr =
  let rec add env = function
    | [] -> []
    | var :: rest ->
        translate_access var.var_name env :: add (Treserved env) rest
  in
  match add env varlist with [] -> expr | el -> Llet (el, expr)

let add_pat_to_env env pat =
  let env' = Tenv (paths_of_pat Path_root pat, env) in
  let mut_vars = mutable_vars_of_pat false pat in
  ( add_lets_to_env mut_vars env',
    add_lets_to_expr mut_vars env',
    List.length mut_vars )

let add_pat_list_to_env env patl =
  let env' =
    List.fold_left
      (fun env pat -> Tenv (paths_of_pat Path_root pat, env))
      env patl
  in
  let mut_vars = List.concat_map (mutable_vars_of_pat false) patl in
  ( add_lets_to_env mut_vars env',
    add_lets_to_expr mut_vars env',
    List.length mut_vars )

(* For let rec: check that the pattern is a variable *)
let add_let_rec_to_env env pat_expr_list =
  let rec add env (pat, expr) =
    match pat.p_desc with
    | Zvarpat v ->
        Tenv
          ([ { var_name = v; var_path = Path_root; var_typ = pat.p_typ } ], env)
    | Zconstraintpat (p, _ty) -> add env (p, expr)
    | _ -> illegal_letrec_pat pat.p_loc
  in
  List.fold_left add env pat_expr_list

let env_for_toplevel_let patl =
  List.fold_left
    (fun env pat -> Tenv (paths_of_pat Path_root pat, env))
    Tnullenv patl
