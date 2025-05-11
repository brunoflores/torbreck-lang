(* Source code locations (ranges of positions), used in parsetree. *)

open Utils

type t = Warnings.loc
type 'a loc = { txt : 'a; loc : t }
