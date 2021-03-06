(* Handling of local labels and backpatching *)

open Buffcode

type label_definition =
  | Label_defined of int
  | Label_undefined of (int * int) list

let new_label_table () = Array.make 16 (Label_undefined [])
let label_table = ref (new_label_table ())
let reset_label_table () = label_table := new_label_table ()

let extend_label_table needed =
  let old = Array.length !label_table in
  let new_table =
    Array.make (((needed / old) + 1) * old) (Label_undefined [])
  in
  for i = 0 to pred old do
    new_table.(i) <- !label_table.(i)
  done;
  label_table := new_table

let define_label lbl =
  if lbl >= Array.length !label_table then extend_label_table lbl;
  match !label_table.(lbl) with
  | Label_defined _ -> failwith "Labels.define_label: already defined"
  | Label_undefined l ->
      (* Save current position to reset it back later *)
      let currpos = !out_position in
      !label_table.(lbl) <- Label_defined currpos;
      begin
        match l with
        | [] -> ()
        | _ ->
            List.iter
              (fun (pos, orig) ->
                out_position := pos;
                out_short (currpos - orig))
              l;
            (* Reset position back to its original *)
            out_position := currpos
      end

let out_label_with_orig orig lbl =
  if lbl == Instruct.nolabel then failwith "Labels.out_label: undefined label";
  if lbl >= Array.length !label_table then extend_label_table lbl;
  match !label_table.(lbl) with
  | Label_defined def -> out_short (def - orig)
  | Label_undefined l ->
      !label_table.(lbl) <- Label_undefined ((!out_position, orig) :: l);
      (* Reserve an offset position (a placeholder) *)
      out_short 0

let out_label lbl = out_label_with_orig !out_position lbl
