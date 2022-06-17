(* Command-line parsing *)

open LightLib.Compiler_driver
module Modules = LightLib.Modules
module Misc = LightLib.Misc

let () =
  let usage = "light [-d] [-no-stdlib] <file1> [<file2>] ..." in
  let debug = ref false in
  let no_stdlib = ref false in
  let add_include d = Misc.load_path := d :: !Misc.load_path in
  let spec =
    [
      ("-d", Arg.Set debug, "Print debug information");
      ("-no-stdlib", Arg.Set no_stdlib, "Do not include the standard library");
      ( "-I",
        Arg.String add_include,
        "Add directory to list of included libraries" );
    ]
  in
  let anonymous fname =
    (* Prepare environment *)
    Modules.default_used_modules := [ "builtin" ];
    if not !no_stdlib then
      Modules.default_used_modules :=
        !Modules.default_used_modules
        @ [ "eq"; "int"; "string"; "io"; "exc"; "vect"; "bool"; "sys" ];

    (* The business *)
    if Filename.check_suffix fname ".ml" then
      let filename = Filename.chop_suffix fname ".ml" in
      compile_implementation (Filename.basename filename) filename
    else if Filename.check_suffix fname ".mli" then
      let filename = Filename.chop_suffix fname ".mli" in
      compile_interface (Filename.basename filename) filename
    else failwith @@ Printf.sprintf "Don't know what to do with file %s." fname
  in
  Arg.parse spec anonymous usage
