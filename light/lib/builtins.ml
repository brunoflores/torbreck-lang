(* The pre-defined global identifiers *)

open Const
open Globals
open Modules

let builtin n d = { qualid = { qual = "builtin"; id = n }; info = d }

(* Some types that must be known to the type checker *)

let constr_type_unit = builtin "unit" { ty_stamp = 2; ty_abbr = Tnotabbrev }
let constr_type_exn = builtin "exn" { ty_stamp = 3; ty_abbr = Tnotabbrev }
let constr_type_bool = builtin "bool" { ty_stamp = 4; ty_abbr = Tnotabbrev }
let constr_type_int = builtin "int" { ty_stamp = 5; ty_abbr = Tnotabbrev }
let constr_type_string = builtin "string" { ty_stamp = 7; ty_abbr = Tnotabbrev }
let constr_type_vect = builtin "vect" { ty_stamp = 10; ty_abbr = Tnotabbrev }
let type_arrow (t1, t2) = { typ_desc = Tarrow (t1, t2); typ_level = notgeneric }
let type_product tlist = { typ_desc = Tproduct tlist; typ_level = notgeneric }

let type_unit =
  { typ_desc = Tconstr (constr_type_unit, []); typ_level = notgeneric }

let type_int =
  { typ_desc = Tconstr (constr_type_int, []); typ_level = notgeneric }

let type_string =
  { typ_desc = Tconstr (constr_type_string, []); typ_level = notgeneric }

let type_bool =
  { typ_desc = Tconstr (constr_type_bool, []); typ_level = notgeneric }

let type_vect t =
  { typ_desc = Tconstr (constr_type_vect, [ t ]); typ_level = notgeneric }

let type_exn =
  { typ_desc = Tconstr (constr_type_exn, []); typ_level = notgeneric }

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

let constr_false =
  builtin "false"
    {
      cs_res =
        { typ_desc = Tconstr (constr_type_bool, []); typ_level = notgeneric };
      cs_arg = type_unit;
      cs_tag = ConstrRegular (0, 2);
      cs_mut = Notmutable;
      cs_kind = Constr_constant;
    }

and constr_true =
  builtin "true"
    {
      cs_res =
        { typ_desc = Tconstr (constr_type_bool, []); typ_level = notgeneric };
      cs_arg = type_unit;
      cs_tag = ConstrRegular (1, 2);
      cs_mut = Notmutable;
      cs_kind = Constr_constant;
    }

(* Some exceptions that must be known to the compiler *)

let match_failure_tag =
  ConstrExtensible ({ qual = "builtin"; id = "Match_failure" }, 1)

let constr_match_failure =
  builtin "Match_failure"
    {
      cs_res =
        { typ_desc = Tconstr (constr_type_exn, []); typ_level = notgeneric };
      cs_arg = type_product [ type_string; type_int; type_int ];
      cs_tag = match_failure_tag;
      cs_mut = Notmutable;
      cs_kind = Constr_superfluous 3;
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
      ( "bool",
        {
          ty_constr = constr_type_bool;
          ty_arity = 0;
          ty_desc = Variant_type [ constr_false; constr_true ];
        } );
      ( "vect",
        { ty_constr = constr_type_vect; ty_arity = 1; ty_desc = Abstract_type }
      );
      ( "exn",
        { ty_constr = constr_type_exn; ty_arity = 0; ty_desc = Variant_type [] }
      );
    ];
  List.iter
    (fun desc -> Hashtbl.add module_builtin.mod_constrs desc.qualid.id desc)
    [ constr_void; constr_false; constr_match_failure; constr_true ];
  Hashtbl.add module_table "builtin" module_builtin
