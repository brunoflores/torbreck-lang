(* Command-line parsing *)

let () =
  let usage = "linker [-d] <file1> [<file2>] ..." in
  let debug = ref false in
  let files = ref ([] : string list) in
  let spec = [ ("-d", Arg.Set debug, "Print debug information") ] in
  let anonymous fname = files := fname :: !files in
  Arg.parse spec anonymous usage;
  List.iter (fun s -> print_endline s) !files;
  exit 0
