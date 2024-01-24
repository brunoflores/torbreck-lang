(* Production of a bytecode executable file *)

open Emit_phr
open Reloc
open Const

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
    try
      let compiled_phrase_index = (input_value ic : compiled_phrase list) in
      let required : compiled_phrase list =
        List.fold_left scan_phrase [] compiled_phrase_index
      in
      close_in ic;
      (actual_filename, required) :: tolink
    with End_of_file ->
      failwith @@ Format.sprintf "End_of_file: %s" actual_filename
  with Misc.Cannot_find_file name ->
    Printf.eprintf "Cannot find file %s.\n" name;
    failwith "Link.scan_file"

(* Second pass: link in the required phrases *)

let link_object oc ((object_filename, phrases) : string * compiled_phrase list)
    : unit =
  let ic = open_in_bin object_filename in
  try
    let link phr =
      let buff = Bytes.create phr.cph_len in
      seek_in ic phr.cph_pos;
      really_input ic buff 0 phr.cph_len;
      Patch.patch_object buff 0 phr.cph_reloc;
      output oc buff 0 phr.cph_len
    in
    List.iter link phrases;
    close_in ic
  with x ->
    Printf.eprintf "Link.link_object: error while linking file %s.\n"
      object_filename;
    close_in ic;
    raise x

(* Translate a structured constant into an object *)
let rec transl_structured_const = function
  | SCatom (ACint i) ->
      print_endline @@ Format.sprintf "Translate structured const: int: %d" i;
      failwith "Link.transl_structured_const: ACint: not implemented"
  | SCatom (ACfloat f) ->
      print_endline @@ Format.sprintf "Translate structured const: float: %f" f;
      failwith "Link.transl_structured_const: ACfloat: not implemented"
  | SCatom (ACstring s) ->
      print_endline @@ Format.sprintf "Translate structured const: string: %s" s;
      let res = Bytes.create 4 in
      (* String tag *)
      Bytes.set res 3 (Char.chr 252);
      (* Length + 1 because of the null byte. *)
      let bs = Bytes.make (String.length s + 1) (Char.chr 0) in
      Bytes.blit (Bytes.of_string s) 0 bs 0 (String.length s);
      Bytes.cat res bs
  | SCatom (ACchar c) ->
      print_endline @@ Format.sprintf "Translate structured const: char: %c" c;
      let res = Bytes.create 4 in
      Bytes.set res 3 c;
      (* Mark least significant bit *)
      Bytes.set res 0 (Char.chr 2);
      res
  | SCblock (tag, comps) as sc ->
      print_endline
      @@ Format.sprintf "Translate structured const: block: %s"
           (Const.show_struct_constant sc);
      let res = Bytes.create 4 in
      let num_of_tag = Symtable.get_num_of_tag tag in
      let b =
        try Char.chr num_of_tag
        with Invalid_argument _ ->
          failwith
          @@ Format.sprintf "Tag %d is outside the range 0..255\n" num_of_tag
      in
      Bytes.set res 3 b;
      fill_structured_const 0 res comps;
      res

and fill_structured_const _n _obj = function
  | [] -> ()
  | _cst :: _rest -> failwith "Link.fill_structured_const: not implemented"

(* Build the initial table of globals *)
let emit_data oc =
  output_binary_int oc (Symtable.number_of_globals ());
  output_binary_int oc (List.length !Symtable.literal_table);
  List.iter
    (function
      | n, sc ->
          output_binary_int oc n;
          output_bytes oc (transl_structured_const sc))
    !Symtable.literal_table

(* Build a bytecode executable file *)
let link object_files exec_name =
  let tolink : (string * compiled_phrase list) list =
    List.fold_left scan_file [] (List.rev object_files)
  in
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
      "#!/home/bruno/devel/torbreck-lang/runtime/target/debug/runtime\n";

    (* The bytecode *)
    let pos1 = pos_out oc in
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
