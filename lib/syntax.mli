open Support.Error

(* Data type definitions *)
type ty =
  | TyBot
  | TyTop
  | TyRef of ty
  | TySource of ty
  | TySink of ty
  | TyId of string
  | TyVar of int * int
  | TyUnit
  | TyFloat
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyString
  | TyBool
  | TyArr of ty * ty
  | TyNat

type term =
  | TmLoc of info * int
  | TmRef of info * term
  | TmDeref of info * term
  | TmAssign of info * term * term
  | TmAscribe of info * term * ty
  | TmString of info * string
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmCase of info * term * (string * (string * term)) list
  | TmTag of info * string * term * ty
  | TmUnit of info
  | TmVar of info * int * int
      (** Variable is its de Bruijn index and the total length of the context
          in which it occurs. *)
  | TmFloat of info * float
  | TmTimesFloat of info * term * term
  | TmLet of info * string * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmAbs of info * string * ty * term
      (** Abstractions are annotated with a string to
          serve as a hint for the name of the bound variable.
          This is used when converting them back from nameless form. *)
  | TmApp of info * term * term
  | TmFix of info * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmInert of info * ty
  | TmError of info
  | TmTry of info * term * term

(* Note on bindings and contexts:
   We use the same context for parsing, printing and type-checking.
   A different implementation strategy would be to define two
   completely different context types - one for parsing and printing
   and another for type-checking. *)

type binding =
  | NameBind
  | TmAbbBind of term * ty option
  | VarBind of ty
  | TyVarBind
  | TyAbbBind of ty

type command =
  | Import of string
  | Eval of info * term
  | Bind of info * string * binding
[@@deriving show]

(* Naming contexts. *)
type context

val emptycontext : context
val ctxlength : context -> int

val addbinding : context -> string -> binding -> context
(** [addbinding ctx str bind] extends context [ctx] with [str] and [bind]. *)

val addname : context -> string -> context
val index_to_name : info -> context -> int -> string

val getbinding : info -> context -> int -> binding
(** [getbinding fi ctx i] look up the ith binding in the context [ctx]. *)

val name_to_index : info -> context -> string -> int
val isnamebound : context -> string -> bool

val gettypefromcontext : info -> context -> int -> ty
(** [gettypefromcontext fi ctx i] extract the typing assumption associated
    with a particular variable [i] in a context [ctx].
    The file information [fi] is used for printing an error message if [i]
    is out of range. *)

(* Shifting and substitution. *)
val termshift : int -> term -> term
val termsubsttop : term -> term -> term
val typeshift : int -> ty -> ty
val typesubsttop : ty -> ty -> ty
val tytermsubsttop : ty -> term -> term

(* Printing. *)
val printtm : context -> term -> unit
val printtm_aterm : bool -> context -> term -> unit
val prbinding : context -> binding -> unit
val printty : context -> ty -> unit

(* Misc. *)
val tmInfo : term -> info
