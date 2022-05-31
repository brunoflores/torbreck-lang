(* Production of a bytecode executable file *)

open Emit_phr
open Reloc

(* First pass: determine which phrases are required *)

module Missing_globals = Set.Make (struct
  open Const

  type t = qualified_ident

  let compare q1 q2 =
    let c = String.compare q1.id q2.id in
    if c != 0 then c else String.compare q1.qual q2.qual
end)

let missing_globals = ref Missing_globals.empty

let is_required = function
  | Reloc_setglobal id, _ -> Missing_globals.mem id !missing_globals
  | _ -> false

let remove_required = function
  | Reloc_setglobal id, _ ->
      missing_globals := Missing_globals.remove id !missing_globals
  | _ -> ()

let add_required = function
  | Reloc_getglobal id, _ ->
      missing_globals := Missing_globals.add id !missing_globals
  | _ -> ()

let scan_phrase tolink phr =
  if (not phr.cph_pure) || List.exists is_required phr.cph_reloc then begin
    List.iter remove_required phr.cph_reloc;
    List.iter add_required phr.cph_reloc;
    phr :: tolink
  end
  else tolink

let scan_file tolink name =
  let ic = open_in_bin name in
  let n = input_binary_int ic in
  seek_in ic n;
  let phrase_index = (input_value ic : compiled_phrase list) in
  let required = List.fold_left scan_phrase [] phrase_index in
  close_in ic;
  (name, required) :: tolink

(* Build a bytecode executable file *)

let link module_list exec_name =
  let tolink = List.fold_left scan_file [] (List.rev module_list) in
  let oc =
    open_out_gen
      [ Open_wronly; Open_trunc; Open_creat; Open_binary ]
      0o777 exec_name
  in
  ()
