(* The compiler entry points *)

open Syntax
open Ty_decl
open Ty_intf
open Emit_phr
open Front
open Back
open Modules
module Errors = MenhirLib.ErrorReports
module LexerUtil = MenhirLib.LexerUtil
module Interpreter = Parser.MenhirInterpreter

let succeed_impl (p : impl_phrase) = p

let succeed_intf intf_name (p : intf_phrase) =
  (* Printf.printf "%s\n" @@ Syntax.show_intf_phrase p; *)
  let oc = open_out_bin intf_name in
  write_compiled_interface oc;
  close_out oc;
  p

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

let get_contents filename =
  let filename, content = (filename, Stdio.In_channel.read_all filename) in
  (LexerUtil.init filename (content |> Lexing.from_string), content)

(* Compiling an interface *)

let compile_intf_phrase phr =
  match phr.in_desc with
  | Zvaluedecl decl ->
      type_valuedecl phr.in_loc decl;
      ()

let compile_interface modname filename =
  let source_name = filename ^ ".mli" in
  let intf_name = filename ^ ".zi" in
  let lexbuf, content = get_contents source_name in
  try
    start_compiling_interface modname;
    compile_intf_phrase
    @@ parse Parser.Incremental.interface (succeed_intf intf_name) lexbuf
         content
  with Sys_error s | Failure s -> failwith s

(* Compiling an implementation *)

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
  let source_name = filename ^ suffix in
  let obj_name = filename ^ ".zo" in
  let oc = open_out_bin obj_name in
  let lexbuf, content = get_contents source_name in
  (* start_emit_phrase oc; *)
  try
    compile_impl_phrase oc
    @@ parse Parser.Incremental.implementation succeed_impl lexbuf content
  with Sys_error s | Failure s -> failwith s
(* end_emit_phrase oc *)

let compile_implementation modname filename suffix =
  if Sys.file_exists (filename ^ ".mli") then begin
    try
      let intfname = filename ^ ".zi" in
      let _ =
        if Sys.file_exists intfname = false then
          failwith
          @@ Printf.sprintf
               "Cannot find file %s.zi. Please compile %s.mli first." filename
               filename
      in
      let intf = read_module modname intfname in
      start_compiling_implementation modname intf;
      enter_interface_definitions intf;
      compile_impl filename suffix;
      check_interface intf;
      ()
    with x -> (* Sys.remove (filename ^ ".zo"); *)
              raise x
  end
  else begin
    compile_impl filename suffix
  end
