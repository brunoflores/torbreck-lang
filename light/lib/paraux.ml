open Syntax
open Location
open Globals

let make_expr desc =
  { e_desc = desc; e_loc = get_current_location (); e_typ = no_type }

and make_impl desc = { im_desc = desc; im_loc = get_current_location () }
