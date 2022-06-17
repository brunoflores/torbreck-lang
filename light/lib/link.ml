(* Production of a bytecode executable file *)

open Emit_phr
open Reloc
open Lambda

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

let scan_file tolink object_filename =
  try
    let actual_filename = Misc.find_in_path object_filename in
    let ic = open_in_bin actual_filename in
    let abs_out_position = input_binary_int ic in
    seek_in ic abs_out_position;
    let compiled_phrase_index = (input_value ic : compiled_phrase list) in
    let required = List.fold_left scan_phrase [] compiled_phrase_index in
    close_in ic;
    (actual_filename, required) :: tolink
  with Misc.Cannot_find_file name ->
    Printf.eprintf "Cannot find file %s.\n" name;
    failwith "Link.scan_file"

(* Second pass: link in the required phrases *)

let events = ref ([] : event list)
let abs_pos = ref 0

let link_object oc ((name, required) : string * compiled_phrase list) : unit =
  let ic = open_in_bin name in
  try
    List.iter
      (fun phr ->
        seek_in ic phr.cph_pos;
        let buff = Bytes.create phr.cph_len in
        really_input ic buff 0 phr.cph_len;
        Patch.patch_object buff 0 phr.cph_reloc;
        (* Printf.printf "output bytes: %s\n"
         * @@ Bytes.fold_left
         *      (fun acc c -> acc ^ string_of_int (int_of_char c) ^ ", ")
         *      "" buff; *)
        output oc buff 0 phr.cph_len;
        abs_pos := !abs_pos + phr.cph_len)
      required;
    close_in ic
  with x ->
    Printf.eprintf "error while liinking file %s.\n" name;
    close_in ic;
    raise x

(* Build the initial table of globals *)
let emit_data oc =
  (* TODO: review *)
  (* output_binary_int oc (Symtable.number_of_globals ()); *)
  output_binary_int oc (List.length !Symtable.literal_table);
  List.iteri
    begin
      fun n sc ->
        Printf.printf "%d %s\n" n (Const.show_struct_constant sc);
        match sc with
        | Const.SCatom (Const.ACstring s) -> begin
            output_string oc s;
            (* Terminate string with a null byte *)
            output_byte oc 0
          end
        | _ ->
            failwith
            @@ Format.sprintf "Link.emit_data: not implemented %s"
                 (Const.show_struct_constant sc)
    end
    (List.rev !Symtable.literal_table)

(* Build a bytecode executable file *)
let link object_files exec_name =
  let tolink = List.fold_left scan_file [] (List.rev object_files) in
  List.iter (fun (name, _) -> Printf.printf "to link: %s\n" name) tolink;
  let oc =
    open_out_gen
      [ Open_wronly; Open_trunc; Open_creat; Open_binary ]
      0o777 exec_name
  in
  try
    (* TODO
       This could be:
       output_string oc "#!/usr/bin/env breckrun\n"; *)
    output_string oc
      "#!/home/bruno/devel/torbreck-lang/runtime/target/release/runtime\n";

    (* The bytecode *)
    let pos1 = pos_out oc in
    abs_pos := 0;
    List.iter (link_object oc) tolink;
    output_byte oc Opcodes.stop;

    (* The table of global data *)
    let pos2 = pos_out oc in
    emit_data oc;

    (* The trailer *)
    let pos3 = pos_out oc in
    let size_of_bytecode = pos2 - pos1 in
    let size_of_globals = pos3 - pos2 in
    output_binary_int oc size_of_bytecode;
    output_binary_int oc size_of_globals;
    (* output_binary_int oc (pos2 - pos1);
     * output_binary_int oc (pos3 - pos2);
     * output_binary_int oc (pos4 - pos3);
     * output_binary_int oc (pos5 - pos4);
     * output_string oc "CL07"; *)
    close_out oc
  with x ->
    close_out oc;
    Sys.remove exec_name;
    raise x
