(* Command-line parsing *)

open LightLib.Compiler
open LightLib.Modules

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
      default_used_modules := [ "builtin" ];
      if Filename.check_suffix f ".ml" then
        let filename = Filename.chop_suffix f ".ml" in
        compile_implementation (Filename.basename filename) filename ".ml"
      else if Filename.check_suffix f ".mli" then
        let filename = Filename.chop_suffix f ".mli" in
        compile_interface (Filename.basename filename) filename
      else failwith @@ Printf.sprintf "Don't know what to do with file %s." f
  | None ->
      print_endline usage;
      exit 0
