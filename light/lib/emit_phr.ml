(* Emitting phrases *)

open Instruct
open Buffcode
open Emitcode

let emit_phrase oc _is_pure phr =
  Reloc.reset ();
  Event.reset ();
  init_out_code ();
  (* Labels.reset_label_table (); *)
  begin
    match phr with
    | { kph_fcts = []; _ } -> emit phr.kph_init
    | _ -> failwith "not implemented: Emit_phr.emit_phrase"
  end;
  output oc !out_buffer 0 !out_position
