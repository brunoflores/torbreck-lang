(* Printing a location in the source program *)

open Parsing

type location =
  | Loc of
      int (* Position of the first character *)
      * int (* Position of the next character following the last one *)

let get_current_location () = Loc (symbol_start (), symbol_end ())
