let ( ~! ) =
  let memo = ref [] in
  fun key ->
    try List.assq key !memo
    with Not_found ->
      let data = Str.regexp key in
      memo := (key, data) :: !memo;
      data

module Toplevel = struct
  let buffer_fmt () =
    let b = Buffer.create 30 in
    (b, Format.formatter_of_buffer b)

  let out_fmt = buffer_fmt ()
  let exec (_, ppf) p = ignore @@ Toploop.execute_phrase true ppf p

  let parse s =
    let lex = Lexing.from_string s in
    let ast = Parse.toplevel_phrase lex in
    ast

  let flush_fmt (b, fmt) =
    Format.pp_print_flush fmt ();
    let r = Buffer.contents b in
    Buffer.reset b;
    r

  let read_output () =
    let values =
      Str.replace_first ~!{|^#\( *\*\)* *|} "" @@ flush_fmt out_fmt
    in
    values

  let init () =
    let _ = Toploop.initialize_toplevel_env () in
    ()
end

let outfile = ref ""
let files = ref []

let () =
  Arg.parse
    [ ("-o", Arg.String (fun s -> outfile := s), "output") ]
    (fun s -> files := s :: !files)
    "ocamltex: ";
  Toplevel.init ()

let process_file file =
  let ic = try open_in file with _ -> failwith "Cannot read input file" in
  let oc =
    try
      if !outfile = "-" then stdout
      else if !outfile = "" then
        open_out (Str.replace_first ~!"\\.tex$" "" file ^ ".ml.tex")
      else
        open_out_gen
          [ Open_wronly; Open_creat; Open_append; Open_text ]
          0x666 !outfile
    with _ -> failwith "Cannot open output file"
  in
  let tex_fmt = Format.formatter_of_out_channel oc in
  let re_spaces = "[ \t]*" in
  let re_start =
    ~!({|\\begin{caml_example\(\*?\)}|} ^ re_spaces
     ^ {|\({toplevel}\|{verbatim}\|{signature}\)?|} ^ re_spaces
     ^ {|\(\[\(.*\)\]\)?|} ^ re_spaces ^ "$")
  in
  try
    while true do
      let input = input_line ic in
      if Str.string_match re_start input 0 then begin
        let read_phrase () =
          let phrase = Buffer.create 256 in
          let rec read () =
            let input = input_line ic in
            if Str.string_match ~!"\\\\end{caml_example\\*?}[ \t]*$" input 0
            then Buffer.contents phrase
            else begin
              Buffer.add_string phrase input;
              read ()
            end
          in
          read ()
        in
        let phrase = read_phrase () in
        let ast = Toplevel.parse phrase in
        let _ = Toplevel.(exec out_fmt) ast in
        let out = Toplevel.read_output () in
        Format.fprintf tex_fmt "%s\n" out;
        Format.pp_print_flush tex_fmt ()
      end
      else begin
        Format.fprintf tex_fmt "%s\n" input;
        Format.pp_print_flush tex_fmt ()
      end
    done
  with End_of_file ->
    close_in ic;
    close_out oc

let _ =
  if !outfile <> "-" && !outfile <> "" then begin
    try close_out (open_out !outfile)
    with _ -> failwith "Cannot open output file"
  end;
  List.iter process_file (List.rev !files)
