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
let is_nonexpansive expr =
  match expr.e_desc with
  | Zident _ -> true
  | Zconstant _ -> true
  | Zconstruct0 _ -> true

let type_of_atomic_constant = function
  | ACint _ -> type_int
  | _ -> failwith "not implemented: Typing.type_of_atomic_constant"

let type_of_structured_constant = function
  | SCatom ac -> type_of_atomic_constant ac
  | _ -> failwith "not implemented: Typing.type_of_structured_constant"

let type_expr _env expr =
  let inferred_ty =
    match expr.e_desc with
    | Zconstant c -> type_of_structured_constant c
    | _ -> failwith "not implemented: Typing.type_expr"
  in
  expr.e_typ <- inferred_ty;
  inferred_ty
