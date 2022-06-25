(* Buffered input and output *)

(* The abstract types for input channels and output channels *)
type in_channel
;;

(* The standard input, standard output, and standard error output
   for the process. *)
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
 (* Read one character from the given input channel.
    Raise [End_of_file] if there are no more characters to read. *)

and open_descriptor_in : int -> in_channel = 1 "open_descriptor"
 (* [open_descriptor_in fd] returns a buffered input channel
    reading from the file descriptor [fd]. The file descriptor [fd]
    must have been previously opened for reading, else the behavior is
    undefined. *)
;;
