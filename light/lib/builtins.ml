(* The pre-defined global identifiers *)

open Const
open Globals
open Modules

let builtin n d = { qualid = { qual = "builtin"; id = n }; info = d }

(* Some types that must be known to the type checker *)

let constr_type_unit = builtin "unit" { ty_stamp = 2; ty_abbr = Tnotabbrev }
let constr_type_int = builtin "int" { ty_stamp = 5; ty_abbr = Tnotabbrev }
let constr_type_string = builtin "string" { ty_stamp = 7; ty_abbr = Tnotabbrev }
let type_arrow (t1, t2) = { typ_desc = Tarrow (t1, t2); typ_level = notgeneric }

let type_unit =
  { typ_desc = Tconstr (constr_type_unit, []); typ_level = notgeneric }

let type_int =
  { typ_desc = Tconstr (constr_type_int, []); typ_level = notgeneric }

let type_string =
  { typ_desc = Tconstr (constr_type_string, []); typ_level = notgeneric }

(* Some constructors that must be known to the parser *)

let constr_void =
  builtin "()"
    {
      cs_res =
        { typ_desc = Tconstr (constr_type_unit, []); typ_level = notgeneric };
      cs_arg = type_unit;
      cs_tag = ConstrRegular (0, 1);
      cs_mut = Notmutable;
      cs_kind = Constr_constant;
    }

(* Construction of the "builtin" module *)

let () =
  let module_builtin = new_module "builtin" in
  List.iter
    (fun (name, desc) ->
      Hashtbl.add module_builtin.mod_types name (builtin name desc))
    [
      ( "unit",
        {
          ty_constr = constr_type_unit;
          ty_arity = 0;
          ty_desc = Variant_type [ constr_void ];
        } );
      ( "int",
        { ty_constr = constr_type_int; ty_arity = 0; ty_desc = Abstract_type }
      );
      ( "string",
        {
          ty_constr = constr_type_string;
          ty_arity = 0;
          ty_desc = Abstract_type;
        } );
    ];
  List.iter
    (fun desc -> Hashtbl.add module_builtin.mod_constrs desc.qualid.id desc)
    [ constr_void ];
  Hashtbl.add module_table "builtin" module_builtin
