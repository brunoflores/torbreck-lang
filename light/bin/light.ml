(* Command-line parsing *)

open LightLib.Compiler_driver

let () =
  let usage = "light [-d] <file1> [<file2>] ..." in
  let debug = ref false in
  let no_stdlib = ref false in
  let spec =
    [
      ("-d", Arg.Set debug, "Print debug information");
      ("-no-stdlib", Arg.Set no_stdlib, "Do not include the standard library");
    ]
  in
  let anonymous fname =
    (* Prepare environment *)
    LightLib.Modules.default_used_modules := [ "builtin" ];
    if not !no_stdlib then
      LightLib.Modules.default_used_modules :=
        !LightLib.Modules.default_used_modules @ [ "eq"; "int"; "string"; "io" ];

    (* The business *)
    if Filename.check_suffix fname ".ml" then
      let filename = Filename.chop_suffix fname ".ml" in
      compile_implementation (Filename.basename filename) filename ".ml"
    else if Filename.check_suffix fname ".mli" then
      let filename = Filename.chop_suffix fname ".mli" in
      compile_interface (Filename.basename filename) filename
    else failwith @@ Printf.sprintf "Don't know what to do with file %s." fname
  in
  Arg.parse spec anonymous usage
