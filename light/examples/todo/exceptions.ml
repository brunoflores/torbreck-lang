exception Found of int;;

let find_index p l =
  let rec find n =
    function [] -> raise (Failure "not found")
           | hd :: tail -> if p hd then raise (Found n)
                           else find (n+1) l
  in
    try find 1 l with Found n -> n
;;
