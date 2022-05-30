(* Emitting phrases *)

open Instruct
open Emitcode

type compiled_phrase = {
  cph_pos : int; (* Position of start of code *)
  cph_len : int; (* Length of code *)
  cph_reloc : (Reloc.info * int) list; (* What to patch *)
  cph_pure : bool; (* Can be omitted or not *)
  cph_events : Lambda.event list;
}

let abs_out_position = ref 0
let compiled_phrase_index = ref ([] : compiled_phrase list)

let start_emit_phrase oc =
  output_binary_int oc 0;
  abs_out_position := 4;
  compiled_phrase_index := []

let end_emit_phrase oc =
  output_value oc !compiled_phrase_index;
  compiled_phrase_index := [];
  seek_out oc 0;
  output_binary_int oc !abs_out_position

let emit_phrase oc is_pure phr =
  Reloc.reset ();
  Event.reset ();
  Buffcode.init_out_code ();
  (* Labels.reset_label_table (); *)
  begin
    match phr with
    | { kph_fcts = []; kph_init = init; _ } ->
        Printf.printf "Instructions list:\n%s"
          (List.fold_left
             (fun acc x ->
               Printf.sprintf "%s\t%s\n" acc (show_zam_instruction x))
             "" init);
        emit init
    | _ -> failwith "not implemented: Emit_phr.emit_phrase"
  end;
  output oc !Buffcode.out_buffer 0 !Buffcode.out_position;
  compiled_phrase_index :=
    {
      cph_pos = !abs_out_position;
      cph_len = !Buffcode.out_position;
      cph_reloc = Reloc.get_info ();
      cph_events = Event.get_events ();
      cph_pure = is_pure;
    }
    :: !compiled_phrase_index;
  abs_out_position := !abs_out_position + !Buffcode.out_position
