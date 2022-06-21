(* System interface *)

value command_line : string vect;;
(* The command line arguments given to the process.
   The first element is the command name used to invoke the program. *)

value exit : int -> 'a = 1 "sys_exit"
;;
