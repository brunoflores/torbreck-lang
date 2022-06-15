(* Linker: Command-line parsing *)

open LightLib.Link
module Misc = LightLib.Misc

let () =
  let usage = "linker [-d] <file1> [<file2>] ..." in
  let debug = ref false in
  let add_include d = Misc.load_path := d :: !Misc.load_path in
  let object_files = ref ([] : string list) in
  let spec =
    [
      ("-d", Arg.Set debug, "Print debug information");
      ( "-I",
        Arg.String add_include,
        "Add directory to list of included libraries" );
    ]
  in
  let anonymous fname =
    let name =
      if Filename.check_suffix fname ".ml" then
        Filename.chop_suffix fname ".ml" ^ ".zo"
      else fname
    in
    object_files := name :: !object_files
  in
  LightLib.Symtable.reset_linker_tables ();
  Arg.parse spec anonymous usage;
  link (List.rev !object_files)
    (Filename.chop_extension (List.hd !object_files) ^ ".out");
  exit 0
