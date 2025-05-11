exception Exit of int

(* An element of the type info represents a "file position":
   file name, line number, and character position within the
   line. Used for priting error messages. *)
type info [@@deriving show]

val dummyinfo : info

(* Create file position info: file name, line number and column.  *)
val createInfo : string -> int -> int -> int -> info
val printInfo : ?context:info -> info -> unit

(* A "value with file info". Used in the lexer and parser. *)
type 'a withinfo = { i : info; v : 'a }

(* Print an error message and fail. The printing function is called
     in a context where the formatter is processing an hvbox.
     Insert calls to Format.print_space to print a space or,
     if necessary, break the line at that point. *)
val errf : (unit -> unit) -> 'a
val errfAt : ?context:info -> info -> (unit -> unit) -> 'a

(* Convenient wrappers for the above, for the common case where the
     action to be performed is just to print a given string. *)
val err : string -> 'a
val error : ?context:info -> info -> string -> 'a

(* Variants that print a message but do not fail afterwards. *)
val warning : string -> unit
val warningAt : info -> string -> unit
