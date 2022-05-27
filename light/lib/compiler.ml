(* The compiler entry points *)

module Errors = MenhirLib.ErrorReports
module LexerUtil = MenhirLib.LexerUtil
module Interpreter = Parser.MenhirInterpreter

let succeed (_p : Syntax.impl_phrase) = ()

let show text positions =
  Errors.extract text positions
  |> Errors.sanitize |> Errors.compress |> Errors.shorten 20

let fail text buffer _ =
  let location = LexerUtil.range (Errors.last buffer) in
  let indication =
    Format.sprintf "Syntax error %s.\n" (Errors.show (show text) buffer)
  in
  Format.eprintf "%s%s%!" location indication;
  exit 1

let parse lexbuf text : unit =
  let supplier = Interpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = Errors.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.implementation lexbuf.lex_curr_p in
  Interpreter.loop_handle succeed (fail text buffer) supplier checkpoint

let get_contents filename =
  let filename, content = (filename, Stdio.In_channel.read_all filename) in
  (LexerUtil.init filename (content |> Lexing.from_string), content)

let loop filename =
  let lexbuf, content = get_contents filename in
  parse lexbuf content

let compile_impl filename suffix =
  let source_name = filename ^ suffix and _obj_name = filename ^ ".zo" in
  (* start_emit_phrase oc; *)
  try loop source_name
  with Sys_error s | Failure s ->
    print_endline s;
    exit 1
(* end_emit_phrase oc *)

let compile_implementation filename suffix = compile_impl filename suffix