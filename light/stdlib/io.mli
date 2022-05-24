(** Buffered input and output *)

(*

type in_channel
type out_channel
(* The abstract types of input channels and output channels. *)

(** Output functions on standard output *)

val print_char : char -> unit
(* Print the character on standard output. *)

val print_string : string -> unit
(* Print the string on standard output. *)

val print_int : int -> unit
(* Print the integer, in decimal, on standard output. *)

val print_float : float -> unit
(* Print the floating-point number, in decimal, on standard output. *)

val print_endline : string -> unit
(* Print the string, followed by a newline character, on
   standard output. *)

val print_newline : unit -> unit
(* Print a newline character on standard output, and flush
   standard output. This can be used to simulate line
   buffering of standard output. *)

external output_char : out_channel -> char -> unit = "output_char"
(* Write the character on the given output channel. *)

*)

val print_string : string -> unit
