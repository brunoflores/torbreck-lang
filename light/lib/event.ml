(* Handling of debugging events *)

open Lambda
open Location
open Syntax
open Modules

let record_events = ref false

let before env { e_loc = Loc (p1, _p2); _ } l =
  if !record_events then
    Levent
      ( {
          ev_kind = Lbefore;
          ev_file = !defined_module.mod_name;
          ev_char = p1;
          ev_env = env;
          ev_pos = 0;
        },
        l )
  else l

let after_pat env { p_loc = Loc (_p1, p2); _ } l =
  if !record_events then
    Levent
      ( {
          ev_kind = Lbefore;
          ev_file = !defined_module.mod_name;
          ev_char = p2;
          ev_env = env;
          ev_pos = 0;
        },
        l )
  else l

let after env { e_loc = Loc (_p1, p2); e_typ = ty; _ } lambda =
  if !record_events then
    Levent
      ( {
          ev_kind = Lafter ty;
          ev_file = !defined_module.mod_name;
          ev_char = p2;
          ev_env = env;
          ev_pos = 0;
        },
        lambda )
  else lambda

let events = ref ([] : event list)
let reset () = events := []
let enter e = events := e :: !events

let get_events () =
  let res = !events in
  events := [];
  res
