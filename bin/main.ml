open Core
module Syntax = CoreLib.Syntax
module Parser = CoreLib.Parser
module Lexer = CoreLib.Lexer
module Core = CoreLib.Core
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Parser.MenhirInterpreter

let cli_state =
  object
    val mutable d = false
    method init ~debug = d <- debug
    method debug = d
  end

let printbindingty ctx b =
  match b with
  | Syntax.NameBind -> ()
  | Syntax.VarBind tyT ->
      print_string ": ";
      Syntax.printty ctx tyT
  | Syntax.TmAbbBind (t, tyT_opt) -> (
      match tyT_opt with
      | None -> Syntax.printty ctx (Core.typeof ctx t)
      | Some tyT -> Syntax.printty ctx tyT)
  | Syntax.TyVarBind -> ()
  | Syntax.TyAbbBind t -> Syntax.printty ctx t

let checkbinding fi ctx b =
  match b with
  | Syntax.NameBind as b -> b
  | Syntax.VarBind _ as b -> b
  | Syntax.TmAbbBind (t, None) -> TmAbbBind (t, Some (Core.typeof ctx t))
  | Syntax.TmAbbBind (t, Some tyT) as b ->
      let tyT' = Core.typeof ctx t in
      if Core.tyeqv ctx tyT' tyT then b
      else
        CoreLib.Support.Error.error fi
          "type of binding does not match declared type"
  | Syntax.TyVarBind as b -> b
  | Syntax.TyAbbBind _ as b -> b

let show text positions =
  E.extract text positions |> E.sanitize |> E.compress |> E.shorten 20

let succeed (cmds : Syntax.context -> Syntax.command list * Syntax.context) =
  let one ctx store cmd =
    match cmd with
    | Syntax.Eval (_, t) ->
        let open Format in
        let tyT = Core.typeof ctx t in
        let t', store' = Core.eval ctx store t in
        Syntax.printtm_aterm true ctx t';
        print_string ": ";
        Syntax.printty ctx tyT;
        force_newline ();
        (ctx, store')
    | Syntax.Bind (fi, x, bind) ->
        let open Format in
        let bind = checkbinding fi ctx bind in
        let bind', store' = Core.evalbinding ctx store bind in
        print_string x;
        print_string ": ";
        printbindingty ctx bind';
        force_newline ();
        (Syntax.addbinding ctx x bind', Core.shiftstore 1 store')
    | Syntax.Import _ -> failwith "not implemented"
  in
  let rec all ctx store cmds =
    match cmds with
    | [] -> ()
    | x :: xs ->
        if cli_state#debug then Format.printf "%a\n" Syntax.pp_command x else ();
        let ctx', store' = one ctx store x in
        all ctx' store' xs
  in
  let cmds, ctx = cmds Syntax.emptycontext in
  let store = Core.emptystore in
  all ctx store cmds

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

let () =
  let usage = "fulluntyped [-d] <file1> [<file2>] ..." in
  let debug = ref false in
  let filename = ref None in
  let spec = [ ("-d", Arg.Set debug, "Print debug information") ] in
  let readfname fname =
    filename := if String.length fname > 0 then Some fname else None
  in
  Arg.parse spec readfname usage;
  cli_state#init ~debug:!debug;
  try loop !filename with CoreLib.Support.Error.Exit code -> exit code
