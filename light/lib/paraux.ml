(* Auxiliary functions for parsing *)

open Syntax
open Location
open Globals

let make_expr desc =
  { e_desc = desc; e_loc = get_current_location (); e_typ = no_type }

and make_typ desc = { te_desc = desc; te_loc = get_current_location () }
and make_impl desc = { im_desc = desc; im_loc = get_current_location () }
and make_intf desc = { in_desc = desc; in_loc = get_current_location () }
