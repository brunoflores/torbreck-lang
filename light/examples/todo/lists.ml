1::2::[];;

[3;4;5];;

let x=2 in [1; 2; x+1; x+2];;

let adds =
  let add x y = x+y
  in [add 1; add 2; add 3]
;;

let is_null = function [] -> true | _ -> false;;

let head = function x :: _ -> x
                  | _      -> raise (Failure "head");;

let tail = function _ :: l -> l
                  | _      -> raise (Failure "tail");;

let rec length = function []     -> 0
                        | _ :: l -> 1 + (length l);;

[1;2] @ [3;4];;

let rec rev = function []     -> []
                     | x :: l -> (rev l) @ [x];;

let rec it_list f a =
  function []   -> a
         | x :: l -> it_list f (f a x) l;;
let sigma = it_list prefix + 0;;
