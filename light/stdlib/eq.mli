value prefix = : 'a -> 'a -> bool = 2 "equal"
  (* [e1 = e2] tests for structural equality of [e1] and [e2].
     Mutable structures (e.g. references and arrays) are equal
     if and only if their current contents are structurally equal,
     even if the two mutable objects are not the same physical object.
     Equality between functional values raises [Invalid_argument].
     Equality between cyclic data structures may not terminate. *)
;;

value prefix < : 'a -> 'a -> bool = 2 "lessthan"
and prefix >= : 'a -> 'a -> bool = 2 "greaterequal"
;;
