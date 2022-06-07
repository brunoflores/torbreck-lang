(* Translation from abstract syntax tree to extended lambda-calculus *)

open Lambda
open Syntax
open Tr_env
open Matching
open Error
open Builtins

(* Compilation of let rec definitions *)

let rec check_letrec_expr expr =
  match expr.e_desc with
  | Zident _ -> ()
  | Zconstant _ -> ()
  | Ztuple el -> List.iter check_letrec_expr el
  | Zconstruct0 _cstr -> ()
  | Zconstruct1 (cstr, expr) ->
      check_letrec_expr expr;
      begin
        match cstr.info.cs_kind with
        | Constr_superfluous _n -> begin
            match expr.e_desc with
            | Ztuple _ -> ()
            | _ -> illegal_letrec_expr expr.e_loc
          end
        | _ -> ()
      end
  | Zfunction _ -> ()
  | _ -> failwith "Front.check_letrec_expr: not implemented"

let size_of_expr expr =
  match expr.e_desc with
  | Ztuple el ->
      List.iter check_letrec_expr el;
      List.length el
  | Zconstruct1 (cstr, expr) ->
      check_letrec_expr expr;
      begin
        match cstr.info.cs_kind with Constr_superfluous n -> n | _ -> 1
      end
  | Zfunction _ -> 2
  | _ -> failwith "Front.size_of_expr: not implemented"

(* Translation of expressions *)

let rec translate_expr env =
  let rec transl expr =
    match expr.e_desc with
    | Zident { contents = Zlocal s } -> translate_access s env
    | Zconstant c -> Lconst c
    | Zapply (({ e_desc = Zident { contents = Zglobal g }; _ } as fn), args) ->
      begin
        match g.info.val_prim with
        | ValueNotPrim ->
            Event.after env expr (Lapply (transl fn, List.map transl args))
        | ValuePrim (arity, p) ->
            if arity == List.length args then
              match (p, args) with
              | Praise, [ arg1 ] ->
                  Lprim (p, [ Event.after env arg1 (transl arg1) ])
              | Pccall (_, _), _ ->
                  Event.after env expr (Lprim (p, List.map transl args))
              | _, _ -> Lprim (p, List.map transl args)
            else Event.after env expr (Lapply (transl fn, List.map transl args))
      end
    | Zapply (funct, args) ->
        Event.after env expr (Lapply (transl funct, List.map transl args))
    | Zlet (false, pat_expr_list, body) ->
        let cas = List.map (fun (pat, _) -> pat) pat_expr_list in
        Llet
          ( translate_bind env pat_expr_list,
            translate_match expr.e_loc env [ (cas, body) ] )
    | Zlet (true, pat_expr_list, body) ->
        let new_env = add_let_rec_to_env env pat_expr_list in
        let translate_rec_bind (_pat, expr) =
          (translate_expr new_env expr, size_of_expr expr)
        in
        Lletrec
          ( List.map translate_rec_bind pat_expr_list,
            Event.before new_env body (translate_expr new_env body) )
    | Zfunction [] -> failwith "Front.translate_expr: empty fun"
    | Zfunction ((patl1, _act1) :: _ as case_list) ->
        let rec transl_fun debug_env = function
          | [] -> translate_match expr.e_loc env case_list
          | pat :: patl ->
              let new_debug_env =
                if pat_irrefutable pat then
                  let newenv, _, _ = add_pat_to_env debug_env pat in
                  newenv
                else Treserved debug_env
              in
              Lfunction
                (Event.after_pat new_debug_env pat
                   (transl_fun new_debug_env patl))
        in
        transl_fun env patl1
    | Zcondition (eif, ethen, eelse) ->
        Lifthenelse
          ( transl eif,
            Event.before env ethen (transl ethen),
            if
              match eelse.e_desc with
              | Zconstruct0 cstr -> cstr == constr_void
              | _ -> false
            then transl eelse
            else Event.before env eelse (transl eelse) )
    | x ->
        Printf.printf "%s\n" (Syntax.show_expression_desc x);
        failwith "not implemented: Front.translate_expr"
  in
  transl

and transl_action env (patlist, expr) =
  let new_env, add_lets, num_pops = add_pat_list_to_env env patlist in
  let action =
    match expr.e_desc with
    | Zwhen (e1, e2) ->
        guard_expression
          (translate_expr new_env e1)
          (translate_expr new_env e2)
          num_pops
    | _ -> translate_expr new_env expr
  in
  (patlist, add_lets (Event.before new_env expr action))

and translate_match loc env casel =
  translate_matching_check_failure loc (List.map (transl_action env) casel)

and translate_bind env = function
  | [] -> []
  | (_pat, expr) :: rest ->
      translate_expr env expr :: translate_bind (Treserved env) rest

(* Translation of toplevel expressions *)

let translate_expression = translate_expr Tnullenv
