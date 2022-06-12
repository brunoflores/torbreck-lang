(* Exceptions *)

value raise: exn -> 'a = 1 "raise";;

(* General-purpose predefined exceptions *)

exception Invalid_argument of string;;
exception Failure of string;;

value failwith : string -> 'a;;
value invalid_arg : string -> 'a;;
