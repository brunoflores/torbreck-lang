(* Printing of error messages and warnings *)

open Location
open Syntax
open Globals

let output_globalref oc = function
  | GRname s -> output_string oc s
  | GRmodname q ->
      output_string oc q.qual;
      output_string oc "__";
      output_string oc q.id

let unbound_value_err name loc =
  Printf.eprintf "%aThe value identifier %a is unbound.\n" output_location loc
    output_globalref name;
  raise @@ Failure "unbound_value_err"

let unbound_type_var_err v ty =
  Printf.eprintf "%aThe type variable %s is unbound.\n" output_location
    ty.te_loc v;
  raise @@ Failure "unbound_type_var_err"

and unbound_type_constr_err name loc =
  Printf.eprintf "%aThe type constructor %a is unbound.\n" output_location loc
    output_globalref name;
  raise @@ Failure "unbound_type_constr"

let type_arity_err cstr args loc =
  Printf.eprintf
    "%sThe type constructor %s expects %d argument(s), but is here given %d.\n"
    (Location.show_location loc)
    (Globals.show_type_desc cstr.info)
    cstr.info.ty_arity (List.length args);
  raise @@ Failure "type_arity_err"

let displacement_overflow () =
  Printf.eprintf "%tPhrase too large, a relative displacement overflowed.\n"
    output_input_name;
  raise @@ Failure "displacement_overflow"

let undefined_value_err val_desc =
  Printf.eprintf
    "The value %s is declared in the interface but not implemented.\n"
    (Globals.show_value_desc val_desc);
  raise @@ Failure "undefined_value_err"

let type_mismatch_err val_desc val_desc' =
  Printf.eprintf
    "The value %s is declared with type %s, but defined with type %s.\n"
    (Globals.show_value_desc val_desc.info)
    (Globals.show_typ val_desc.info.val_typ)
    (Globals.show_typ val_desc'.info.val_typ);
  raise @@ Failure "type_mismatch_err"

let cannot_generalize_err val_desc =
  Printf.eprintf
    "The type inferred for the value %s, that is %s, contains type variables \
     that cannot be generalized.\n"
    (Globals.show_value_desc val_desc.info)
    (Globals.show_typ val_desc.info.val_typ);
  raise @@ Failure "cannot_generalize_err"
