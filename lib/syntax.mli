open Support.Error

type term =
  | TmString of info * string
  | TmVar of info * int * int
      (** Variable is its de Bruijn index and the total length of the context
          in which it occurs. *)
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmLet of info * string * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmAbs of info * string * term
      (** Abstractions are annotated with a string to
          serve as a hint for the name of the bound variable.
          This is used when converting them back from nameless form. *)
  | TmApp of info * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmFloat of info * float
  | TmTimesFloat of info * term * term

type binding = NameBind | TmAbbBind of term

type command =
  | Import of string
  | Eval of info * term
  | Bind of info * string * binding

(* Naming contexts. *)
type context

val emptycontext : context

val ctxlength : context -> int

val addbinding : context -> string -> binding -> context

val addname : context -> string -> context

val index_to_name : info -> context -> int -> string

val getbinding : info -> context -> int -> binding

val name_to_index : info -> context -> string -> int

val isnamebound : context -> string -> bool

(* Shifting and substitution. *)
val termshift : int -> term -> term

val termsubsttop : term -> term -> term

(* Printing. *)
val printtm : context -> term -> unit

val printtm_aterm : bool -> context -> term -> unit

val prbinding : context -> binding -> unit

(* Misc. *)
val tmInfo : term -> info
