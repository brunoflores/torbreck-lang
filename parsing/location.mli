(* Source code locations (ranges of positions), used in parsetree. *)

open Utils

type t = Warnings.loc = {
  loc_start : Lexing.position;
  loc_end : Lexing.position;
}
(** [t] represents a range of characters in the source code. *)

val none : t
(** An arbitrary value of type [t]. *)

type 'a loc = { txt : 'a; loc : t }
