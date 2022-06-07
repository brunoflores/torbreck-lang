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
  Printf.printf "Emit_phr.end_emit_phrase: abs_out_position: %d\n"
    !abs_out_position;
  seek_out oc 0;
  output_binary_int oc !abs_out_position

let emit_phrase oc is_pure phr =
  Reloc.reset ();
  Event.reset ();
  Buffcode.init_out_code ();
  Labels.reset_label_table ();
  Printf.printf "\nZam phrase:\n%s\n" (Instruct.show_zam_phrase phr);
  begin
    match phr with
    | { kph_fcts = []; _ } -> emit phr.kph_init
    | { kph_rec = false; _ } ->
        emit [ Kbranch 0 ];
        emit phr.kph_fcts;
        emit [ Klabel 0 ];
        emit phr.kph_init
    | { kph_rec = true; _ } ->
        emit phr.kph_init;
        emit [ Kbranch 0 ];
        emit phr.kph_fcts;
        emit [ Klabel 0 ]
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
