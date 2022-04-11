open Syntax

val eval : context -> term -> term
(** Multi-step evaluation function. *)

val evalbinding : context -> binding -> binding
