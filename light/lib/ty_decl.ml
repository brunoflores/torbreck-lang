(* Typing toplevel phrases *)

open Types
open Typing
open Modules
open Globals
open Syntax

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
