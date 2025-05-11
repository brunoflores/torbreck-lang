(* Invoke the compiler and do IO *)

module Errors = MenhirLib.ErrorReports
module LexerUtil = MenhirLib.LexerUtil
module Interpreter = Parser.MenhirInterpreter
open Syntax

let do_directive _loc = function
  | Zdir ("open", name) -> Modules.State.open_module name
  | Zdir (d, _) -> failwith @@ Format.sprintf "unknown directive: %s\n" d

let compile_intf_phrase (phr : Syntax.intf_phrase) =
  match phr.in_desc with
  | Zvaluedecl decl -> Ty_decl.type_valuedecl phr.in_loc decl
  | Ztypedecl decl ->
      let _ = Ty_decl.type_typedecl phr.in_loc decl in
      ()
  | Zintfdirective dir -> do_directive phr.in_loc dir
  | Zexcdecl decl ->
      let _ = Ty_decl.type_excdecl phr.in_loc decl in
      ()

let compile_impl_phrase oc (phr : Syntax.impl_phrase) =
  let _ = Typing.reset_type_expression_vars () in
  match phr.im_desc with
  | Zexpr expr ->
      let _ty = Ty_decl.type_expression phr.im_loc expr in
      Emit_phr.emit_phrase oc (Syntax.expr_is_pure expr)
        (Back.compile_lambda false (Front.translate_expression expr))
  | Zletdef { recflag; binders } ->
      let _env = Ty_decl.type_letdef phr.im_loc recflag binders in
      Emit_phr.emit_phrase oc
        (Syntax.letdef_is_pure binders)
        (if recflag then
           Back.compile_lambda true
             (Front.translate_letdef_rec phr.im_loc binders)
         else
           Back.compile_lambda false (Front.translate_letdef phr.im_loc binders))
  | Ztypedef decl ->
      let _ = Ty_decl.type_typedecl phr.im_loc decl in
      ()
  | Zimpldirective dir -> do_directive phr.im_loc dir
  | Zexcdef decl ->
      let _ = Ty_decl.type_excdecl phr.im_loc decl in
      ()

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
  let content = Stdio.In_channel.read_all filename in
  (LexerUtil.init filename (content |> Lexing.from_string), content)

let write_compiled_interface intf_name =
  let oc = open_out_bin intf_name in
  Modules.write_compiled_interface oc;
  close_out oc

let compile_impl filename suffix =
  let source_name = filename ^ suffix in
  let obj_name = filename ^ ".zo" in
  let oc = open_out_bin obj_name in
  let lexbuf, content = get_contents source_name in
  let _ = Emit_phr.start_emit_phrase oc in
  try
    while true do
      parse Parser.Incremental.implementation (compile_impl_phrase oc) lexbuf
        content
    done
  with End_of_file ->
    Emit_phr.end_emit_phrase oc;
    close_out oc

let compile_interface (_modname : string) (filename : string) : unit =
  let source_name = filename ^ ".mli" in
  let intf_name = filename ^ ".zi" in
  let lexbuf, content = get_contents source_name in
  let _ = Modules.State.reset () in
  try
    while true do
      parse Parser.Incremental.interface compile_intf_phrase lexbuf content
    done
  with
  | End_of_file -> write_compiled_interface intf_name
  | Sys_error s | Failure s -> failwith @@ "compile_interface: " ^ s

let compile_implementation (_modname : string) (filename : string) : unit =
  let intf_name = filename ^ ".zi" in
  let suffix = ".ml" in
  let _ = Modules.State.reset () in
  if Sys.file_exists (filename ^ ".mli") then begin
    (* Module interface provided by the user as .mli *)
    try
      let _ =
        if Sys.file_exists intf_name = false then
          failwith
          @@ Printf.sprintf
               "Cannot find file %s.zi. Please compile %s.mli first." filename
               filename
      in
      let intf_mod = Modules.Mem.read intf_name in
      Modules.State.start_compiling_implementation intf_mod;
      Ty_intf.enter_interface_definitions intf_mod;
      compile_impl filename suffix;
      Ty_intf.check_interface intf_mod
    with x ->
      Sys.remove (filename ^ ".zo");
      raise x
  end
  else begin
    (* No module interface provided: export all symbols *)
    try
      compile_impl filename suffix;
      write_compiled_interface intf_name
    with x -> raise x
  end
