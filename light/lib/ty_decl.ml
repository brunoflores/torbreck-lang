(* Typing toplevel phrases *)

open Types
open Typing
open Modules
open Globals
open Syntax
open Const

let enter_new_variant is_extensible _loc (_ty_constr, ty_res, constrs) =
  let nbr_constrs = List.length constrs in
  let rec make_constrs constr_idx = function
    | [] -> []
    | Zconstr0decl _ :: _ -> failwith ""
    | Zconstr1decl (constr_name, arg, mut_flag) :: rest ->
        let ty_arg = type_of_type_expression true arg in
        let constr_tag =
          if is_extensible then
            ConstrExtensible
              ( { qual = compiled_module_name (); id = constr_name },
                Modules.new_exc_stamp () )
          else ConstrRegular (constr_idx, nbr_constrs)
        in
        let kind =
          match type_repr ty_arg with
          | { typ_desc = Tproduct tylist; _ } -> begin
              match mut_flag with
              | Notmutable -> Constr_superfluous (List.length tylist)
              | Mutable -> Constr_regular
            end
          | _ -> Constr_regular
        in
        let constr_glob =
          defined_global constr_name
            {
              cs_res = ty_res;
              cs_arg = ty_arg;
              cs_mut = mut_flag;
              cs_tag = constr_tag;
              cs_kind = kind;
            }
        in
        add_constr constr_glob;
        constr_glob :: make_constrs (succ constr_idx) rest
  in
  let constr_descs = make_constrs 0 constrs in
  pop_type_level ();
  generalize_type ty_res;
  List.iter (fun cstr -> generalize_type cstr.info.cs_arg) constr_descs;
  Variant_type constr_descs

type external_type = {
  et_descr : type_desc global;
  et_manifest : bool;
  mutable et_defined : bool;
}

let external_types = ref ([] : (string * external_type) list)

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

let enter_new_type (ty_name, params, def) =
  let ty_constr =
    defined_global ty_name
      { ty_stamp = new_type_stamp (); ty_abbr = Tnotabbrev }
  in
  let ty_desc =
    defined_global ty_name
      { ty_constr; ty_arity = List.length params; ty_desc = Abstract_type }
  in
  add_type ty_desc;
  (ty_desc, params, def)

let define_new_type loc (ty_desc, params, def) =
  push_type_level ();
  let ty_params =
    try bind_type_expression_vars params
    with Failure _ -> Error.duplicate_param_in_type_decl_err loc
  in
  let ty_res =
    {
      typ_desc = Tconstr (ty_desc.info.ty_constr, ty_params);
      typ_level = notgeneric;
    }
  in
  let type_comp =
    match def with
    | Zabstract_type -> begin
        pop_type_level ();
        Abstract_type
      end
  in
  ty_desc.info.ty_desc <- type_comp;
  begin
    try
      let extdef = List.assoc ty_desc.qualid.id !external_types in
      if extdef.et_manifest || extdef.et_defined then
        Error.illegal_type_redefinition loc extdef.et_descr;
      if extdef.et_descr.info.ty_arity <> ty_desc.info.ty_arity then
        Error.type_decl_arity_err loc extdef.et_descr ty_desc;
      extdef.et_defined <- true;
      let extconstr = extdef.et_descr.info.ty_constr in
      let intconstr = ty_desc.info.ty_constr in
      intconstr.info.ty_stamp <- extconstr.info.ty_stamp;
      extconstr.info.ty_abbr <- intconstr.info.ty_abbr
    with Not_found -> ()
  end;
  (ty_res, type_comp)

let check_recursive_abbrev loc (ty, _params, _def) =
  try check_recursive_abbrev ty.info.ty_constr
  with Recursive_abbrev -> Error.recursive_abbrev_err loc ty

let type_typedecl loc decl =
  let newdecl = List.map enter_new_type decl in
  let res = List.map (define_new_type loc) newdecl in
  List.iter (check_recursive_abbrev loc) newdecl;
  res

let type_excdecl loc decl =
  push_type_level ();
  reset_type_expression_vars ();
  enter_new_variant true loc (Builtins.constr_type_exn, Builtins.type_exn, decl)

let type_letdef _loc rec_flag pat_expr_list =
  push_type_level ();
  let ty_list = List.map (fun (_pat, _expr) -> new_type_var ()) pat_expr_list in
  typing_let := true;
  let env =
    type_pattern_list (List.map (fun (pat, _expr) -> pat) pat_expr_list) ty_list
  in
  typing_let := false;
  let enter_val =
    List.iter (fun (name, (ty, _mut_flag)) ->
        add_value
          (defined_global name { val_typ = ty; val_prim = ValueNotPrim }))
  in
  if rec_flag then enter_val env;
  List.iter2 (fun (_pat, exp) ty -> type_expect [] exp ty) pat_expr_list ty_list;
  pop_type_level ();
  let gen_type =
    List.map2
      (fun (_pat, expr) ty -> (is_nonexpansive expr, ty))
      pat_expr_list ty_list
  in
  List.iter (fun (gen, ty) -> if not gen then nongen_type ty) gen_type;
  List.iter (fun (gen, ty) -> if gen then generalize_type ty) gen_type;
  if not rec_flag then enter_val env;
  env
