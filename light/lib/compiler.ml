(* The compiler entry points *)

open Syntax
open Ty_decl
open Emit_phr
open Front
open Back
module Errors = MenhirLib.ErrorReports
module LexerUtil = MenhirLib.LexerUtil
module Interpreter = Parser.MenhirInterpreter

let succeed (p : impl_phrase) = p

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

let parse lexbuf text =
  let supplier = Interpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = Errors.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.implementation lexbuf.lex_curr_p in
  Interpreter.loop_handle succeed (fail text buffer) supplier checkpoint

let get_contents filename =
  let filename, content = (filename, Stdio.In_channel.read_all filename) in
  (LexerUtil.init filename (content |> Lexing.from_string), content)

let compile_impl_phrase oc (phr : impl_phrase) =
  (* reset_type_expression_vars(); *)
  match phr.im_desc with
  | Zexpr expr ->
      let _ty = type_expression phr.im_loc expr in
      emit_phrase oc (expr_is_pure expr)
        (compile_lambda false (translate_expression expr))
  | x ->
      failwith
      @@ Printf.sprintf "not implemented: Compiler.parse: %s"
           (Syntax.show_impl_desc x)

let compile_impl filename suffix =
  let source_name = filename ^ suffix and obj_name = filename ^ ".zo" in
  let oc = open_out_bin obj_name in
  let lexbuf, content = get_contents source_name in
  (* start_emit_phrase oc; *)
  try compile_impl_phrase oc @@ parse lexbuf content
  with Sys_error s | Failure s ->
    print_endline s;
    exit 1
(* end_emit_phrase oc *)

let compile_implementation filename suffix = compile_impl filename suffix
