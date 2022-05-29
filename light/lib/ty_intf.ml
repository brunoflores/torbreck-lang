(* Consistency check between an interface and an implementation *)

open Const
open Globals
open Modules
open Ty_decl

(* Create the initial environment for compiling an implementation
   when an explicit interface exists. *)

let enter_interface_definitions intf =
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
