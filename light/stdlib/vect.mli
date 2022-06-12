(* Operations on vectors *)

value vect_length : 'a vect -> int = 1 "vect_length"
;;

value vect_item : 'a vect -> int -> 'a
and vect_assign : 'a vect -> int -> 'a -> unit
;;
