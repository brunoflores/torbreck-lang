(* Buffered input and output *)

(* The abstract types for input channels and output channels *)
type in_channel
;;

(* The standard input, standard output, and standard error output
   for the process. [std_in], [std_out] and [std_err] are respectively
   synonymous with [stdin], [stdout] and [stderr]. *)
value std_in : in_channel
;;

value exit : int -> 'a
;;

(* Output functions on standard output *)
value print_endline : string -> unit = 1 "print_endline"
and print_int : int -> unit
and print_string : string -> unit = 1 "print_string"
;;

(* General input functions *)
value input_char : in_channel -> char = 1 "input_char"
and open_descriptor_in : int -> in_channel = 1 "open_descriptor"
;;
