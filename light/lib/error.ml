(* Printing of error messages and warnings *)

open Location
open Syntax
open Globals
open Types

let output_globalref oc = function
  | GRname s -> output_string oc s
  | GRmodname q ->
      output_string oc q.qual;
      output_string oc "__";
      output_string oc q.id

let unbound_value_err name loc =
  Printf.eprintf "%aThe value identifier %a is unbound.\n" output_location loc
    output_globalref name;
  failwith "unbound_value_err"

let unbound_type_var_err v ty =
  Printf.eprintf "%aThe type variable %s is unbound.\n" output_location
    ty.te_loc v;
  failwith "unbound_type_var_err"

and unbound_type_constr_err name loc =
  Printf.eprintf "%aThe type constructor %a is unbound.\n" output_location loc
    output_globalref name;
  failwith "unbound_type_constr"

let type_arity_err cstr args loc =
  Printf.eprintf
    "%sThe type constructor %s expects %d argument(s), but is here given %d.\n"
    (Location.show_location loc)
    (Globals.show_type_desc cstr.info)
    cstr.info.ty_arity (List.length args);
  failwith "type_arity_err"

let displacement_overflow () =
  Printf.eprintf "%tPhrase too large, a relative displacement overflowed.\n"
    output_input_name;
  failwith "displacement_overflow"

let undefined_value_err val_desc =
  Printf.eprintf
    "The value %s is declared in the interface but not implemented.\n"
    (Globals.show_value_desc val_desc);
  failwith "undefined_value_err"

let type_mismatch_err val_desc val_desc' =
  Printf.eprintf
    "The value %s is declared with type %s, but defined with type %s.\n"
    (Globals.show_value_desc val_desc.info)
    (Globals.show_typ val_desc.info.val_typ)
    (Globals.show_typ val_desc'.info.val_typ);
  failwith "type_mismatch_err"

let cannot_generalize_err val_desc =
  Printf.eprintf
    "The type inferred for the value %s, that is %s, contains type variables \
     that cannot be generalized.\n"
    (Globals.show_value_desc val_desc.info)
    (Globals.show_typ val_desc.info.val_typ);
  failwith "cannot_generalize_err"

let application_of_non_function_err exp ty =
  begin
    try
      let _ = filter_arrow ty in
      Printf.eprintf "This function is applied to too many arguments.\n"
    with Unify ->
      Printf.eprintf
        "This expression is not a function, it cannot be applied: %s.\n"
        (Syntax.show_expression exp)
  end;
  failwith "application_of_non_function_err"

let expr_wrong_type_err _exp actual_ty expected_ty =
  Printf.eprintf "This expression has type %s but is used with type %s.\n"
    (Globals.show_typ actual_ty)
    (Globals.show_typ expected_ty);
  failwith "expr_wrong_type_err"

let non_linear_pattern_err _pat name =
  Printf.eprintf "The variable %s is bound several times in this pattern.\n"
    name;
  failwith "non_linear_pattern_err"

let ill_shaped_match_err _exp =
  Printf.eprintf "This curried matching contains cases of different lengths.\n";
  failwith "ill_shaped_match_err"

let unused_cases_warning _loc : unit =
  Printf.eprintf "Warning: this matching case is unused.\n";
  failwith "unused_cases_warning"

let not_exhaustive_warning _loc : unit =
  Printf.eprintf "Warning: this matching is not exhaustive.\n";
  failwith "not_exhaustive_warning"

let illegal_letrec_pat _loc =
  Printf.eprintf
    "Only variables are allowed as left-hand sides of \"let rec\".\n";
  failwith "illegal_letrec_pat"

let illegal_letrec_expr _loc =
  Printf.eprintf
    "This kind of expression is not allowed in right-hand sides of \"let rec\".\n";
  failwith "illegal_letrec_expr"

let duplicate_param_in_type_decl_err _loc =
  Printf.eprintf "Repeated type parameter in type declaration.\n";
  failwith "duplicate_param_in_type_decl_err"

let illegal_type_redefinition _loc _ty_desc =
  Printf.eprintf
    "The type is exported as an abstract type by this module and defined \
     several times in the implementation. Please define it only once.";
  failwith "illegal_type_redefinition"

let type_decl_arity_err _loc ty_desc1 ty_desc2 =
  Printf.eprintf
    "The type has been declared with %d parameter(s) but is here defined with \
     %d parameter(s).\n"
    ty_desc1.info.ty_arity ty_desc2.info.ty_arity;
  failwith "type_decl_arity_err"

let recursive_abbrev_err _loc _ty_cstr =
  Printf.eprintf "The type abbreviation is a cyclic (infinite) type.\n";
  failwith "recursive_abbrev_err"
