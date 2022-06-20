type arith_expr = Const of int
                | Var of string
                | Plus of args
                | Mult of args
                | Minus of args
                | Div of args
and args = {Arg1:arith_expr; Arg2:arith_expr};;

(* AST for x+(3*y) *)
Plus {Arg1=Var "x"; Arg2=Mult {Arg1=Const 3; Arg2=Var "y"}};;
