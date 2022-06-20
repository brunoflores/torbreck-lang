[< >];;
[< '0; '1; '2 >];;
[< '0; [< '1; '2 >]; '3 >];;

let s = [< '"abc" >] in [< s; '"def" >];;

let stream_concat s t = [< s; t >];;

stream_of_channel;;
stream_of_string;;

(* Destructive *)
let next = function [< 'x >] -> s;;
let s = [< '0; '1; '2 >];;
next s;;
next s;;
next s;;
next s;;

let next function [< 'x >] -> x
                | [<  >]   -> raise (Failure "empty")

(* A parser recognizing a non-empty sequence of
   characters 'a'. *)
let seq_a = 
  let rec seq = function
      [< ''a'; seq l >] -> 'a' :: l
    | [< >] -> []
  in 
  (* A non-empty sequence of 'a' starts with an 'a',
     and is followedd by a possibly empty sequence
     of 'a'. *)
  function [< ''a'; seq l >] -> 'a' :: l
;;

(* A non-empty sequence of 'a' followed by a 'b',
   or a 'b' alone. *)
let seq_a_b = function
    [< seq_a l; ''b' >] -> l @ ['b']
  | [< ''b' >] -> ['b']
;;

(* Bindings in stream patterns occur sequentially.
   Stream matching is guaranteed to be performed from
   left to right. *)
let rec stream_sum accumulator = function
    [< '0; (stream_sum accumulator) p >] -> p
  | [< 'x; (stream_sum (accumulator+x)) p >] -> p
  | [< >] -> n
;;

(* Example: a parser for arithmetic expressions *)

type token = 
  PLUS | MINUS | TIMES | DIV | LPAR | RPAR | INT of int
;;

let rec spaces = function
    [< '' '|'\t'|'\n'; spaces _ >] -> ()
  | [< >] -> ()

let int_of_digit = function
    '0'..'9' as c -> (int_of_char c) - (int_of_char '0')
  | _ -> raise (Failure "not a digit")
;;

let rec integer n = function
    [< ' '0'..'9' as c; (integer (10*n + int_of_digit c)) r >] -> r
  | [< >] -> n
;;

integer 0 (stream_of_string "12345");;

(* The lexical analyser *)

let rec lexer s = match s with
    [< ''('; spaces _ >] -> [< 'LPAR; lexer s >]
  | [< '')'; spaces _ >] -> [< 'RPAR; lexer s >]
  | [< ''+'; spaces _ >] -> [< 'PLUS; lexer s >]
  | [< ''-'; spaces _ >] -> [< 'MINUS; lexer s >]
  | [< ''*'; spaces _ >] -> [< 'TIMES; lexer s >]
  | [< ''/'; spaces _ >] -> [< 'DIV; lexer s >]
  | [< ''0'..'9' as c; (integer (int_of_digit c)) n; spaces _ >] -> [< 'INT n; lexer s >]
;;

type atree =
    Int of int
  | Plus of atree * atree
  | Minus of atree * atree
  | Mult of atree * atree
  | Div of atree * atree
;;

let addop = function
    [< 'PLUS >] -> (function (x, y) -> Plus (x, y))
  | [< 'MINUS >] -> (function (x, y) -> Minus (x, y))

and multop = function
    [< 'TIMES >] -> (function (x, y) -> Mult (x, y))
  | [< 'DIV >] -> (function (x, y) -> Div (x, y))
;;

let rec left_assoc op term =
  let rec rest e1 = function
      [< op f; term e2; (rest (f (e1, e2))) e >] -> e
    | [< >] -> e1
  in function [< term e1; (rest e1) e2 >] -> e2
;;

let rec expr str = left_assoc addop mult str
and mult str = left_assoc multop atom str
and atom = function
    [< 'INT n >] -> Int n
  | [< 'LPAR; expr e; 'RPAR >] -> e
;;

expr (lexer (stream_of_string "(1+2+3*4)-567"));;

