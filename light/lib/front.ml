(* Translation from abstract syntax tree to extended lambda-calculus *)

open Lambda
open Syntax
open Tr_env

(* Translation of expressions *)

let translate_expr env =
  let transl expr =
    match expr.e_desc with
    | Zident { contents = Zlocal s } -> translate_access s env
    | Zconstant c -> Lconst c
    | x ->
        failwith
        @@ Printf.sprintf "not implemented: Front.translate_expr: %s"
             (Syntax.show_expression_desc x)
  in
  transl

(* Translation of toplevel expressions *)

let translate_expression = translate_expr Tnullenv
