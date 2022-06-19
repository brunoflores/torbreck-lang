(* Operations on references *)

type 'a ref = ref of mutable 'a;;
  (* The type of references (mutable indirection cells) containing a value
     of type ['a]. *)

value prefix ! : 'a ref -> 'a = 1 "field0"
and prefix := : 'a ref -> 'a -> unit = 2 "setfield0"
and incr : int ref -> unit = 1 "incr"
and decr : int ref -> unit = 1 "decr"
;;
