(* Operations on vectors, without sanity checks *)

value vect_length : 'a vect -> int = 1 "vect_length"
;;

value vect_item : 'a vect -> int -> 'a = 2 "get_vect_item"
and vect_assign : 'a vect -> int -> 'a -> unit = 3 "set_vect_item"
;;
