open Syntax

val eval : context -> term -> term
(** Multi-step evaluation function. *)

val typeof : context -> term -> ty
val tyeqv : context -> ty -> ty -> bool
val evalbinding : context -> binding -> binding
val simplifyty : context -> ty -> ty
