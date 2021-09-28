open Core
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module A = FulluntypedLib
module Syntax = A.Syntax
module Parser = A.Parser
module Lexer = A.Lexer
module Fulluntyped = A.Fulluntyped
module I = Parser.MenhirInterpreter

let print_position outx (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  let fname = pos.pos_fname in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  fprintf outx "%s:%d:%d" fname line col

let show text positions =
  E.extract text positions |> E.sanitize |> E.compress |> E.shorten 20

let succeed (cmds : Syntax.context -> Syntax.command list * Syntax.context) =
  let cmds, ctx = cmds Syntax.emptycontext in
  let one ctx cmd =
    match cmd with
    | Syntax.Eval (_, t) ->
        let t' = Fulluntyped.eval ctx t in
        Syntax.printtm_aterm true ctx t';
        ctx
    | _ -> failwith "not implemented"
  in
  let rec all ctx cmds =
    match cmds with
    | [] -> ()
    | x :: xs ->
        let ctx = one ctx x in
        all ctx xs
  in
  all ctx cmds

let fail text buffer _ =
  let location = L.range (E.last buffer) in
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  eprintf "%s%s%!" location indication;
  exit 1

let parse lexbuf text : unit =
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.topLevel lexbuf.lex_curr_p in
  I.loop_handle succeed (fail text buffer) supplier checkpoint

let get_contents s =
  let filename, content =
    match s with
    | None | Some "-" -> ("-", In_channel.input_all In_channel.stdin)
    | Some filename -> (filename, In_channel.read_all filename)
  in
  (L.init filename (content |> Lexing.from_string), content)

let loop filename =
  let lexbuf, content = get_contents filename in
  parse lexbuf content

let command =
  Command.basic ~summary:"Type-check a program"
    Command.Let_syntax.(
      let%map_open filename = anon (maybe ("filename" %: Filename.arg_type)) in
      fun () -> loop filename)

let () = Command.run command
