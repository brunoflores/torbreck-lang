(* Linker: Command-line parsing *)

open LightLib.Link

let () =
  let usage = "linker [-d] <file1> [<file2>] ..." in
  let debug = ref false in
  let object_files = ref ([] : string list) in
  let spec = [ ("-d", Arg.Set debug, "Print debug information") ] in
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
  link (List.rev !object_files) "a.out";
  exit 0
