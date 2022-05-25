(* module Lexer = LightLib.Lexer *)
(* let () = LightStdLib.Io.print_string "Light" *)

open LightLib.Compiler

let () =
  let usage = "light [-d] <file1> [<file2>] ..." in
  let debug = ref false in
  let filename = ref None in
  let spec = [ ("-d", Arg.Set debug, "Print debug information") ] in
  let readfname fname =
    filename := if String.length fname > 0 then Some fname else None
  in
  Arg.parse spec readfname usage;
  match !filename with
  | Some f ->
      compile_implementation f;
      exit 0
  | None ->
      print_endline usage;
      exit 0
