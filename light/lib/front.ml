(* Translation from abstract syntax tree to extended lambda-calculus *)

open Lambda
open Syntax
open Tr_env
open Matching
open Error
open Builtins
open Modules
open Globals
open Prim

(* Propagation of constants *)

exception Not_constant

let extract_constant = function Lconst cst -> cst | _ -> raise Not_constant

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
    | Zident { contents = Zglobal g } -> begin
        match g.info.val_prim with
        | ValueNotPrim -> Lprim (Pget_global g.qualid, [])
        | ValuePrim (0, _) -> Lprim (Pget_global g.qualid, [])
        | ValuePrim (arity, p) ->
            let rec make_fct args n =
              if n >= arity then Lprim (p, args)
              else Lfunction (make_fct (Lvar n :: args) (n + 1))
            in
            make_fct [] 0
      end
    | Zconstant c -> Lconst c
    | Zconstruct1 (c, arg) -> begin
        match c.info.cs_kind with
        | Constr_superfluous _ ->
            failwith "Front.translate_expr: Constr_superfluous: not implemented"
        | _ ->
            let tr_arg = transl arg in
            begin
              match c.info.cs_mut with
              | Mutable -> Lprim (Pmakeblock c.info.cs_tag, [ tr_arg ])
              | Notmutable -> begin
                  try
                    Lconst
                      (SCblock (c.info.cs_tag, [ extract_constant tr_arg ]))
                  with Not_constant ->
                    Lprim (Pmakeblock c.info.cs_tag, [ tr_arg ])
                end
            end
      end
    | Zapply
        ( ({ e_desc = Zfunction ((patl, _) :: _ as case_list); _ } as funct),
          args ) ->
        if List.length patl == List.length args then
          Llet (translate_let env args, translate_match expr.e_loc env case_list)
        else Event.after env expr (Lapply (transl funct, List.map transl args))
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

and translate_let env = function
  | [] -> []
  | a :: l -> translate_expr env a :: translate_let (Treserved env) l

and translate_bind env = function
  | [] -> []
  | (_pat, expr) :: rest ->
      translate_expr env expr :: translate_bind (Treserved env) rest

(* Translation of toplevel expressions *)

let translate_expression = translate_expr Tnullenv

(* Translation of toplevel let expressions *)

let rec make_sequence f = function
  | [] -> Lconst (SCatom (ACint 0))
  | [ x ] -> f x
  | x :: rest -> Lsequence (f x, make_sequence f rest)

let translate_letdef loc pat_expr_list =
  let modname = !defined_module.mod_name in
  match pat_expr_list with
  (* Simple case: let id = expr *)
  | [ ({ p_desc = Zvarpat i; _ }, expr) ] ->
      Lprim
        (Pset_global { qual = modname; id = i }, [ translate_expression expr ])
  (* The general case *)
  | _ ->
      let pat_list = List.map (fun (p, _) -> p) pat_expr_list in
      let vars = List.concat_map Syntax.free_vars_of_pat pat_list in
      let env = Tr_env.env_for_toplevel_let pat_list in
      let store_global var =
        Lprim
          ( Pset_global { qual = modname; id = var },
            [ translate_access var env ] )
      in
      Llet
        ( translate_bind Tnullenv pat_expr_list,
          translate_matching_check_failure loc
            [ (pat_list, make_sequence store_global vars) ] )

(* Translation of toplevel let rec expressions *)

let extract_variable pat =
  let rec extract p =
    match p.p_desc with
    | Zvarpat id -> id
    | Zconstraintpat (p, _ty) -> extract p
    | _ -> Error.illegal_letrec_pat pat.p_loc
  in
  extract pat

exception Complicated_definition

let translate_letdef_rec _loc pat_expr_list =
  (* First check that all patterns are variables *)
  let var_expr_list =
    List.map (fun (pat, expr) -> (extract_variable pat, expr)) pat_expr_list
  in
  let modname = !defined_module.mod_name in
  (* Simple case: let rec id = fun *)
  try
    make_sequence
      (function
        | i, e -> (
            match e.e_desc with
            | Zfunction _ ->
                Lprim
                  ( Pset_global { qual = modname; id = i },
                    [ translate_expression e ] )
            | _ -> raise Complicated_definition))
      var_expr_list
  with Complicated_definition ->
    (* The general case *)
    let dummies =
      make_sequence
        (function
          | i, e ->
              Lprim
                ( Pset_global { qual = modname; id = i },
                  [ Lprim (Pdummy (size_of_expr e), []) ] ))
        var_expr_list
    in
    let updates =
      make_sequence
        (function
          | i, e ->
              Lprim
                ( Pupdate,
                  [
                    Lprim (Pget_global { qual = modname; id = i }, []);
                    translate_expression e;
                  ] ))
        var_expr_list
    in
    Lsequence (dummies, updates)
