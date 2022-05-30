(* Translation from abstract syntax tree to extended lambda-calculus *)

open Lambda
open Syntax
open Tr_env

(* Translation of expressions *)

let translate_expr env =
  let rec transl expr =
    match expr.e_desc with
    | Zident { contents = Zlocal s } -> translate_access s env
    | Zconstant c -> Lconst c
    | Zapply (({ e_desc = Zident { contents = Zglobal g }; _ } as fn), args) ->
      begin
        match g.info.val_prim with
        | ValueNotPrim ->
            Event.after env expr (Lapply (transl fn, List.map transl args))
        | ValuePrim (arity, p) ->
            if arity == List.length args then
              match (p, args) with
              | Praise, [ arg1 ] ->
                  Lprim (p, [ Event.after env arg1 (transl arg1) ])
              | Pccall (_, _), _ ->
                  Event.after env expr (Lprim (p, List.map transl args))
              | _, _ -> Lprim (p, List.map transl args)
            else Event.after env expr (Lapply (transl fn, List.map transl args))
      end
    | x ->
        Printf.printf "%s\n" (Syntax.show_expression_desc x);
        failwith "not implemented: Front.translate_expr"
  in
  transl

(* Translation of toplevel expressions *)

let translate_expression = translate_expr Tnullenv
