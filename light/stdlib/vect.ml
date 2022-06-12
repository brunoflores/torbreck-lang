(* Operations on vectors, with sanity checks *)

let vect_item v i =
  if i < 0 || i >= vect_length v
  then invalid_arg "vect_item"
  else fvect__vect_item v i
;;

let vect_assign v i e =
  if i < 0 || i > vect_length v
  then invalid_arg "vect_assign"
  else fvect__vect_assign v i e
;;
