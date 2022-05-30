(* Consistency check between an interface and an implementation *)

open Const
open Globals
open Modules
open Ty_decl
open Error
open Types

(* Create the initial environment for compiling an implementation
   when an explicit interface exists. *)

let enter_interface_definitions intf =
  Printf.printf "enter_interface_defitions\n";
  external_types := [];
  Hashtbl.iter
    (fun _name ty_desc ->
      let manifest =
        match ty_desc.info.ty_desc with
        | Abstract_type -> false
        | _ ->
            add_type ty_desc;
            true
      in
      external_types :=
        ( ty_desc.qualid.id,
          { et_descr = ty_desc; et_manifest = manifest; et_defined = false } )
        :: !external_types)
    (types_of_module intf);
  Hashtbl.iter
    (fun _name val_desc ->
      match val_desc.info.val_prim with
      | ValuePrim _ -> add_value val_desc
      | _ -> ())
    (values_of_module intf);
  Hashtbl.iter
    (fun _name constr_desc -> add_constr constr_desc)
    (constrs_of_module intf)

(* Check that an implementation matches an explicit interface *)

let check_value_match val_decl =
  let val_impl =
    try
      Hashtbl.find
        (values_of_module !defined_module)
        (little_name_of_global val_decl)
    with Not_found -> undefined_value_err val_decl.info
  in
  let nongen_vars = free_type_vars notgeneric val_impl.info.val_typ in
  begin
    try filter (type_instance val_impl.info.val_typ, val_decl.info.val_typ)
    with Unify -> type_mismatch_err val_decl val_impl
  end;
  if List.exists (fun ty -> free_type_vars generic ty != []) nongen_vars then
    cannot_generalize_err val_impl

let check_interface intf =
  Hashtbl.iter
    (fun _name val_desc ->
      match val_desc.info.val_prim with
      | ValueNotPrim -> check_value_match val_desc
      | _ -> ())
    (values_of_module intf)
