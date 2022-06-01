module Lexer = LightLib.Lexer
module Errors = MenhirLib.ErrorReports
module LexerUtil = MenhirLib.LexerUtil
module Interpreter = LightLib.Parser.MenhirInterpreter

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

let parse start succeed lexbuf text =
  let supplier = Interpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = Errors.wrap_supplier supplier in
  let checkpoint = start lexbuf.lex_curr_p in
  Interpreter.loop_handle succeed (fail text buffer) supplier checkpoint
