(*
let print_char = output_char stdout
let print_string = output_string stdout
let print_int i = print_string (string_of_int i)
let print_float f = print_string (string_of_float f)

let print_endline s =
  print_string s;
  print_char '\n'

let print_newline () =
  print_char '\n';
  flush stdout
*)

external print_string : string -> unit = "print_string"
