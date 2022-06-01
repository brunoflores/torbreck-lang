module Compiler = LightLib.Compiler
module Modules = LightLib.Modules
module Lexer = LightLib.Lexer
module Parser = LightLib.Parser
module Interpreter = Parser.MenhirInterpreter
module Errors = MenhirLib.ErrorReports
module LexerUtil = MenhirLib.LexerUtil

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

let () =
  let modname = "one" in
  let mli = "value print_string : string -> unit = 1 \"print_string \";;" in
  let _ml = "print_string \"42\";;" in
  let content = mli in
  let lexbuf = content |> Lexing.from_string in
  begin
    try
      Modules.default_used_modules := [ "builtin" ];
      Modules.start_compiling_interface modname;
      parse Parser.Incremental.interface
        (fun phr -> Compiler.compile_intf_phrase phr)
        lexbuf content
    with Sys_error s | Failure s -> failwith s
  end;
  let md = !Modules.defined_module in
  match md with
  | { mod_name = "one"; mod_values; mod_types; mod_constrs; _ } -> begin
      begin
        try
          let md = Hashtbl.find mod_values "print_string" in
          match md.info.val_typ.typ_desc with
          | Tarrow
              ( { typ_desc = Tconstr ({ info = { ty_stamp = 7; _ }; _ }, []); _ },
                {
                  typ_desc = Tconstr ({ info = { ty_stamp = 2; _ }; _ }, []);
                  _;
                } ) ->
              ()
          | _ -> failwith "not the expected arrow type"
        with Not_found -> failwith "symbol not found"
      end;
      if Hashtbl.length mod_types > 0 then failwith "expected no types";
      if Hashtbl.length mod_constrs > 0 then failwith "expected no constructors"
    end
  | _ -> failwith "test failed: Modules.defined_module: module not found"
