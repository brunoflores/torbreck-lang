(* Type inference *)

open Syntax
open Const
open Builtins
open Globals
open Error
open Types
open Modules

(* To convert type expressions to types *)

let type_expr_vars = ref ([] : (string * typ) list)
let reset_type_expression_vars () = type_expr_vars := []

let bind_type_expression_vars var_list =
  type_expr_vars := [];
  List.map
    (function
      | v ->
          if List.mem_assoc v !type_expr_vars then
            failwith "bind_type_expression_vars"
          else begin
            let t = new_global_type_var () in
            type_expr_vars := (v, t) :: !type_expr_vars;
            t
          end)
    var_list

let type_of_type_expression strict_flag typexp =
  let rec type_of typexp =
    match typexp.te_desc with
    | Ztypevar v -> begin
        try List.assoc v !type_expr_vars
        with Not_found ->
          if strict_flag then unbound_type_var_err v typexp
          else begin
            let t = new_global_type_var () in
            type_expr_vars := (v, t) :: !type_expr_vars;
            t
          end
      end
    | Ztypearrow (arg1, arg2) -> type_arrow (type_of arg1, type_of arg2)
    | Ztypeconstr (cstr_name, args) ->
        let cstr =
          try find_type_desc cstr_name
          with Desc_not_found ->
            unbound_type_constr_err cstr_name typexp.te_loc
        in
        if List.length args != cstr.info.ty_arity then
          type_arity_err cstr args typexp.te_loc
        else
          {
            typ_desc = Tconstr (cstr.info.ty_constr, List.map type_of args);
            typ_level = notgeneric;
          }
  in
  type_of typexp

(* Typing of patterns *)

let typing_let = ref false

let rec tpat new_env ((pat, ty, mut_flag) : pattern * typ * mutable_flag) =
  pat.p_typ <- ty;
  begin
    match pat.p_desc with
    | Zvarpat v ->
        if List.mem_assoc v new_env then non_linear_pattern_err pat v
        else (v, (ty, mut_flag)) :: new_env
    | _ as p ->
        Printf.printf "%s\n" (Syntax.show_pattern_desc p);
        failwith ""
  end

and tpat_list new_env (pat_list : pattern list) (ty_list : typ list) =
  match (pat_list, ty_list) with
  | [], [] -> new_env
  | pat :: patl, ty :: tyl ->
      tpat_list (tpat new_env (pat, ty, Notmutable)) patl tyl
  | _, _ -> failwith "Typing: arity error"

let type_pattern = tpat []
and type_pattern_list = tpat_list []

(* Check if an expression is non-expansive, that is, the result of its
   evaluation cannot contain newly created mutable objects. *)
let rec is_nonexpansive expr =
  match expr.e_desc with
  | Zident _ -> true
  | Zconstant _ -> true
  | Zconstruct0 _ -> true
  | Zfunction _ -> true
  | Zwhile _ -> false
  | Ztrywith (body, pat_expr_list) ->
      is_nonexpansive body
      && List.for_all (fun (_pat, expr) -> is_nonexpansive expr) pat_expr_list
  | Zsequence (_e1, e2) -> is_nonexpansive e2
  | Zapply _ -> false
  | Ztuple el -> List.for_all is_nonexpansive el
  | Zconstruct1 (cstr, e) -> cstr.info.cs_mut == Notmutable && is_nonexpansive e
  | Zlet (_rec_flag, bindings, body) ->
      List.for_all (fun (_pat, expr) -> is_nonexpansive expr) bindings
      && is_nonexpansive body
  | Zcondition (_e1, e2, e3) -> is_nonexpansive e2 && is_nonexpansive e3
  | Zwhen (_cond, action) -> is_nonexpansive action

let type_of_atomic_constant = function
  | ACint _ -> type_int
  | ACfloat _ -> type_float
  | ACstring _ -> type_string
  | ACchar _ -> type_char

let type_of_structured_constant = function
  | SCatom ac -> type_of_atomic_constant ac
  | x ->
      failwith
      @@ Format.sprintf
           "not implemented: Typing.type_of_structured_constant: %s"
           (Const.show_struct_constant x)

(* Typing of expressions *)

let unify_expr expr expected_ty actual_ty =
  try unify (expected_ty, actual_ty)
  with Unify -> expr_wrong_type_err expr actual_ty expected_ty

let rec type_expr env expr =
  let inferred_ty =
    match expr.e_desc with
    | Zident r -> begin
        match !r with
        | Zglobal glob_desc -> type_instance glob_desc.info.val_typ
        | Zlocal s -> begin
            try
              let ty_schema, _ = List.assoc s env in
              type_instance ty_schema
            with Not_found -> (
              try
                let glob_desc = find_value_desc (GRname s) in
                r := Zglobal glob_desc;
                type_instance glob_desc.info.val_typ
              with Desc_not_found -> unbound_value_err (GRname s) expr.e_loc)
          end
      end
    | Zconstant c -> type_of_structured_constant c
    | Zconstruct0 cstr -> begin
        match cstr.info.cs_kind with
        | Constr_constant -> type_instance cstr.info.cs_res
        | _ ->
            let ty_res, ty_arg =
              type_pair_instance (cstr.info.cs_res, cstr.info.cs_arg)
            in
            type_arrow (ty_arg, ty_res)
      end
    | Zconstruct1 (cstr, arg) -> begin
        match cstr.info.cs_kind with
        | Constr_constant -> Error.constant_constr_err cstr expr.e_loc
        | _ ->
            let ty_res, ty_arg =
              type_pair_instance (cstr.info.cs_res, cstr.info.cs_arg)
            in
            type_expect env arg ty_arg;
            ty_res
      end
    | Zapply (fn, args) ->
        let ty_fn = type_expr env fn in
        let rec type_args ty_res = function
          | [] -> ty_res
          | arg1 :: arg_rest ->
              let ty1, ty2 =
                try filter_arrow ty_res
                with Unify -> application_of_non_function_err fn ty_fn
              in
              type_expect env arg1 ty1;
              type_args ty2 arg_rest
        in
        type_args ty_fn args
    | Zlet (rec_flag, pat_expr_list, body) ->
        type_expr (type_let_decl env rec_flag pat_expr_list) body
    | Zfunction [] -> failwith "Typing.type_expr: empty matching"
    | Zfunction ((patl1, _expr1) :: _ as matching) ->
        let ty_args = List.map (fun _pat -> new_type_var ()) patl1 in
        let ty_res = new_type_var () in
        let tcase (patl, action) =
          if List.length patl != List.length ty_args then
            ill_shaped_match_err expr;
          type_expect (type_pattern_list patl ty_args @ env) action ty_res
        in
        List.iter tcase matching;
        List.fold_right
          (fun ty_arg ty_res -> type_arrow (ty_arg, ty_res))
          ty_args ty_res
    | Ztrywith (body, matching) ->
        let ty = type_expr env body in
        List.iter
          (fun (pat, expr) ->
            type_expect (type_pattern (pat, type_exn, Notmutable) @ env) expr ty)
          matching;
        ty
    | Zsequence (e1, e2) ->
        type_statement env e1;
        type_expr env e2
    | Zcondition (e1, e2, e3) ->
        type_expect env e1 type_bool;
        if
          match e3.e_desc with
          | Zconstruct0 cstr -> cstr == constr_void
          | _ -> false
        then begin
          type_expect env e2 type_unit;
          type_unit
        end
        else begin
          let ty = type_expr env e2 in
          type_expect env e3 ty;
          ty
        end
    | Zwhile (cond, body) ->
        type_expect env cond type_bool;
        type_statement env body;
        type_unit
    | _ as d ->
        Printf.printf "%s\n" (Syntax.show_expression_desc d);
        failwith "Typing.type_expr"
  in
  expr.e_typ <- inferred_ty;
  inferred_ty

and type_expect env exp expected_ty =
  match exp.e_desc with
  | Zconstant (SCatom (ACstring _)) ->
      let actual_ty =
        begin
          match (type_repr expected_ty).typ_desc with
          (* Hack for format strings *)
          (* https://github.com/brunoflores/camllight/blob/master/sources/src/compiler/typing.ml#L457 *)
          (* | Tconstr (cstr, _) ->
           *     if cstr = constr_type_format then type_format exp.e_loc s
           *     else type_string *)
          | _ -> type_string
        end
      in
      unify_expr exp expected_ty actual_ty
  | Zlet (rec_flag, pat_expr_list, body) ->
      type_expect (type_let_decl env rec_flag pat_expr_list) body expected_ty
  | Zsequence (e1, e2) ->
      type_statement env e1;
      type_expect env e2 expected_ty
  | Zcondition (cond, ifso, ifnot) ->
      type_expect env cond type_bool;
      type_expect env ifso expected_ty;
      type_expect env ifnot expected_ty
  | Ztuple el -> begin
      try
        List.iter2 (type_expect env) el
          (filter_product (List.length el) expected_ty)
      with Unify -> unify_expr exp expected_ty (type_expr env exp)
    end
  (* To do: try...with, match...with ? *)
  (* https://github.com/brunoflores/camllight/blob/master/sources/src/compiler/typing.ml#L480 *)
  | _ -> unify_expr exp expected_ty (type_expr env exp)

and type_let_decl env rec_flag pat_expr_list =
  push_type_level ();
  let ty_list = List.map (fun (_pat, _expr) -> new_type_var ()) pat_expr_list in
  typing_let := true;
  let add_env =
    type_pattern_list (List.map (fun (pat, _expr) -> pat) pat_expr_list) ty_list
  in
  typing_let := false;
  let new_env = add_env @ env in
  List.iter2
    (fun (_pat, exp) ty ->
      type_expect (if rec_flag then new_env else env) exp ty)
    pat_expr_list ty_list;
  pop_type_level ();
  let gen_type =
    List.map2
      (fun (_pat, expr) ty -> (is_nonexpansive expr, ty))
      pat_expr_list ty_list
  in
  List.iter (fun (gen, ty) -> if not gen then nongen_type ty) gen_type;
  List.iter (fun (gen, ty) -> if gen then generalize_type ty) gen_type;
  new_env

and type_statement env expr =
  let ty = type_expr env expr in
  match (type_repr ty).typ_desc with
  | Tarrow _ -> Error.partial_apply_warning expr.e_loc
  | Tvar _ -> ()
  | _ ->
      if not (Types.same_base_type ty type_unit) then
        Error.not_unit_type_warning expr ty
