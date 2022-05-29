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

(* Check if an expression is non-expansive, that is, the result of its
   evaluation cannot contain newly created mutable objects. *)
let rec is_nonexpansive expr =
  match expr.e_desc with
  | Zident _ -> true
  | Zconstant _ -> true
  | Zconstruct0 _ -> true
  | Zapply _ -> false
  | Zconstruct1 (cstr, e) -> cstr.info.cs_mut == Notmutable && is_nonexpansive e

let type_of_atomic_constant = function
  | ACint _ -> type_int
  | _ -> failwith "not implemented: Typing.type_of_atomic_constant"

let type_of_structured_constant = function
  | SCatom ac -> type_of_atomic_constant ac
  | _ -> failwith "not implemented: Typing.type_of_structured_constant"

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
          (* https://github.com/brunoflores/camllight/blob/master/sources/src/compiler/typing.ml#L457 *)
          (* | Tconstr (cstr, _) ->
           *     if cstr = constr_type_format then type_format exp.e_loc s
           *     else type_string *)
          | _ -> type_string
        end
      in
      unify_expr exp expected_ty actual_ty
  (* To do: try...with, match...with ? *)
  (* https://github.com/brunoflores/camllight/blob/master/sources/src/compiler/typing.ml#L480 *)
  | _ -> unify_expr exp expected_ty (type_expr env exp)
