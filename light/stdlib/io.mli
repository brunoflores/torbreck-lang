(* Buffered input and output *)

(* The abstract types for input channels and output channels *)
type in_channel
;;

(* The standard input, standard output, and standard error output
   for the process. [std_in], [std_out] and [std_err] are respectively
   synonymous with [stdin], [stdout] and [stderr]. *)
value std_in : in_channel
;;

(* Output functions on standard output *)
value print_endline : string -> unit = 1 "print_endline"
;;

(* General input functions *)
value input_char : in_channel -> char = 1 "input_char"
;;
