(* Printing a location in the source program *)

open Parsing
open Config

let input_name = ref "" (* Input file name *)
let input_chan = ref stdin (* The channel opened on the input *)

type location =
  | Loc of
      int (* Position of the first character *)
      * int (* Position of the next character following the last one *)
[@@deriving show]

let get_current_location () = Loc (symbol_start (), symbol_end ())

let output_lines oc char1 char2 charline1 line1 line2 =
  let n1 = char1 - charline1 in
  let n2 = char2 - charline1 in
  if line2 > line1 then
    Printf.fprintf oc ", line %d-%d, characters %d-%d:\n" line1 line2 n1 n2
  else Printf.fprintf oc ", line %d, characters %d-%d:\n" line1 n1 n2;
  ()

let output_loc oc input seek line_flag (Loc (pos1, pos2)) =
  let pr_chars n c =
    for _ = 1 to n do
      output_char oc c
    done
  in
  let skip_line () =
    try
      while input () != '\n' do
        ()
      done
    with End_of_file -> ()
  in
  let copy_line () =
    let c = ref ' ' in
    (try
       while
         c := input ();
         !c != '\n'
       do
         output_char oc !c
       done
     with End_of_file -> output_string oc "<EOF>");
    output_char oc '\n'
  in
  let pr_line first len ch =
    let c = ref ' ' in
    let f = ref first in
    let l = ref len in
    try
      while
        c := input ();
        !c != '\n'
      do
        if !f > 0 then (
          f := !f - 1;
          output_char oc (if !c == '\t' then !c else ' '))
        else if !l > 0 then (
          l := !l - 1;
          output_char oc (if !c == '\t' then !c else ch))
        else ()
      done
    with End_of_file -> if !f = 0 && !l > 0 then pr_chars 5 ch
  in
  let pos = ref 0 in
  let line1 = ref 1 in
  let line1_pos = ref 0 in
  let line2 = ref 1 in
  let line2_pos = ref 0 in
  seek 0;
  (try
     while !pos < pos1 do
       incr pos;
       if input () == '\n' then (
         incr line1;
         line1_pos := !pos;
         ())
     done
   with End_of_file -> ());
  line2 := !line1;
  line2_pos := !line1_pos;
  (try
     while !pos < pos2 do
       incr pos;
       if input () == '\n' then (
         incr line2;
         line2_pos := !pos;
         ())
     done
   with End_of_file -> ());
  if line_flag then output_lines oc pos1 pos2 !line1_pos !line1 !line2;
  if !line1 == !line2 then (
    seek !line1_pos;
    output_string oc error_prompt;
    copy_line ();
    seek !line1_pos;
    output_string oc error_prompt;
    pr_line (pos1 - !line1_pos) (pos2 - pos1) '^';
    output_char oc '\n')
  else (
    seek !line1_pos;
    output_string oc error_prompt;
    pr_line 0 (pos1 - !line1_pos) '.';
    seek pos1;
    copy_line ();
    if !line2 - !line1 <= 8 then
      for _ = !line1 + 1 to !line2 - 1 do
        output_string oc error_prompt;
        copy_line ()
      done
    else (
      for _ = !line1 + 1 to !line1 + 3 do
        output_string oc error_prompt;
        copy_line ()
      done;
      output_string oc error_prompt;
      output_string oc "..........\n";
      for _ = !line1 + 4 to !line2 - 4 do
        skip_line ()
      done;
      for _ = !line2 - 3 to !line2 - 1 do
        output_string oc error_prompt;
        copy_line ()
      done);
    (try
       output_string oc error_prompt;
       for _ = !line2_pos to pos2 - 1 do
         output_char oc (input ())
       done;
       pr_line 0 100 '.'
     with End_of_file -> output_string oc "<EOF>");
    output_char oc '\n')

let output_location oc loc =
  if String.length !input_name > 0 then begin
    let p = pos_in !input_chan in
    Printf.fprintf oc "File \"%s\"" !input_name;
    output_loc oc
      (fun () -> input_char !input_chan)
      (seek_in !input_chan) true loc;
    seek_in !input_chan p
  end
  else begin
    (* Toplevel input... *)
    ()
  end

let output_input_name oc =
  Printf.fprintf oc "Flie \"%s\", line 1:\n" !input_name
