(* Auxiliary functions for parsing *)

open Error
open Syntax
open Location
open Globals
open Modules

let make_expr desc =
  { e_desc = desc; e_loc = get_current_location (); e_typ = no_type }

and make_typ desc = { te_desc = desc; te_loc = get_current_location () }
and make_impl desc = { im_desc = desc; im_loc = get_current_location () }
and make_intf desc = { in_desc = desc; in_loc = get_current_location () }

and make_pat desc =
  { p_desc = desc; p_loc = get_current_location (); p_typ = no_type }

let make_apply = function
  | { e_desc = Zconstruct0 cstr1; _ }, [ e2 ] ->
      make_expr (Zconstruct1 (cstr1, e2))
  | e1, el -> make_expr (Zapply (e1, el))

let make_binop op ({ e_loc = Loc (_l1, m1); _ } as e1)
    ({ e_loc = Loc (l2, _m2); _ } as e2) =
  make_expr
    (Zapply
       ( {
           e_desc = Zident (ref (Zlocal op));
           e_loc = Loc (m1, l2);
           e_typ = no_type;
         },
         [ e1; e2 ] ))

let expr_constr_or_ident = function
  | GRname s as gr -> begin
      try make_expr (Zconstruct0 (find_constr_desc gr))
      with Desc_not_found -> make_expr (Zident (ref (Zlocal s)))
    end
  | GRmodname _ as gr -> (
      try make_expr (Zconstruct0 (find_constr_desc gr))
      with Desc_not_found -> (
        try make_expr (Zident (ref (Zglobal (find_value_desc gr))))
        with Desc_not_found -> unbound_value_err gr (get_current_location ())))

let pat_constr_or_var s =
  try make_pat (Zconstruct0pat (find_constr_desc (GRname s)))
  with Desc_not_found -> make_pat (Zvarpat s)
