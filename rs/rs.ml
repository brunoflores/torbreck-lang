module I = Parser.MenhirInterpreter
module LexerUtil = MenhirLib.LexerUtil
module Errors = MenhirLib.ErrorReports
module Interpreter = Parser.MenhirInterpreter

let usage = ""
let source : string option ref = ref None
let anon_fn x = source := Some x
let spec_list = []

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
  let supplier = Interpreter.lexer_lexbuf_to_supplier Lexer.main lexbuf in
  let buffer, supplier = Errors.wrap_supplier supplier in
  let checkpoint = start lexbuf.lex_curr_p in
  Interpreter.loop_handle succeed (fail text buffer) supplier checkpoint

let get_contents filename =
  let filename, content = (filename, Stdio.In_channel.read_all filename) in
  (LexerUtil.init filename (content |> Lexing.from_string), content)

let compile _x = ()

let () =
  Arg.parse spec_list anon_fn usage;
  match !source with
  | None -> print_endline usage
  | Some fname -> (
      let lexbuf, content = get_contents fname in
      try
        while true do
          parse Parser.Incremental.main compile lexbuf content
        done
      with End_of_file -> ())
