(* Invoke the compiler and do IO *)

module Errors = MenhirLib.ErrorReports
module LexerUtil = MenhirLib.LexerUtil
module Interpreter = Parser.MenhirInterpreter

let succeed_impl (p : Syntax.impl_phrase) = p

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

let write_compiled_interface intf_name =
  let oc = open_out_bin intf_name in
  Modules.write_compiled_interface oc;
  close_out oc

let succeed_intf (p : Syntax.intf_phrase) =
  Printf.printf "%s\n" @@ Syntax.show_intf_phrase p;
  Compiler.compile_intf_phrase p

let compile_interface modname filename =
  let source_name = filename ^ ".mli" in
  let intf_name = filename ^ ".zi" in
  let lexbuf, content = get_contents source_name in
  try
    Modules.start_compiling_interface modname;
    while true do
      parse Parser.Incremental.interface succeed_intf lexbuf content
    done
  with
  | End_of_file -> write_compiled_interface intf_name
  | Sys_error s | Failure s -> failwith s

let compile_impl filename suffix =
  let source_name = filename ^ suffix in
  let obj_name = filename ^ ".zo" in
  let oc = open_out_bin obj_name in
  let lexbuf, content = get_contents source_name in
  Emit_phr.start_emit_phrase oc;
  begin
    try
      let zam_phr, is_pure =
        Compiler.compile_impl_phrase
        @@ parse Parser.Incremental.implementation succeed_impl lexbuf content
      in
      Emit_phr.emit_phrase oc is_pure zam_phr
    with Sys_error s | Failure s -> failwith s
  end;
  Emit_phr.end_emit_phrase oc;
  close_out oc

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
      let intf = Modules.read_module modname intfname in
      Modules.start_compiling_implementation modname intf;
      Ty_intf.enter_interface_definitions intf;
      compile_impl filename suffix;
      Ty_intf.check_interface intf
    with x -> (* Sys.remove (filename ^ ".zo"); *)
              raise x
  end
  else begin
    let intf_name = filename ^ ".zi" in
    try
      Modules.start_compiling_interface modname;
      compile_impl filename suffix;
      write_compiled_interface intf_name
    with x -> raise x
  end
