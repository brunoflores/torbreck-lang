let ( ~! ) =
  let memo = ref [] in
  fun key ->
    try List.assq key !memo
    with Not_found ->
      let data = Str.regexp key in
      memo := (key, data) :: !memo;
      data

let latex_escape s = String.concat "" [ "$"; s; "$" ]
let toplevel_prompt = latex_escape {|\?|} ^ " "

type env = Env of string

(* let main = Env "example" *)
let input_env = Env "input"
let ok_output = Env "output"

(* let error = Env "error" *)
(* let warning = Env "warn" *)
let phrase_env = Env ""
let camlprefix = "caml"

let start out (Env s) args =
  Format.fprintf out "\\begin{%s%s}" camlprefix s;
  List.iter (Format.fprintf out "{%s}") args;
  Format.fprintf out "\n"

let stop out (Env s) =
  Format.fprintf out "\\end{%s%s}" camlprefix s;
  Format.fprintf out "\n"

let code_env env out s =
  let sep = if s.[String.length s - 1] = '\n' then "" else "\n" in
  Format.fprintf out "%a%s%s%a"
    (fun ppf env -> start ppf env [])
    env s sep stop env

module Toplevel = struct
  type output = {
    values : string;  (** toplevel output *)
    stdout : string;  (** output printed on the toplevel stdout *)
  }

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

  (** Redirect the stdout *)
  (* let stdout_out, stdout_in = Unix.pipe ~cloexec:true () *)
  (* let () = Unix.dup2 stdout_in Unix.stdout *)

  (* let _read_stdout = *)
  (*   let size = 50 in *)
  (*   let b = Bytes.create size in *)
  (*   let buffer = Buffer.create 100 in *)
  (*   let rec read_toplevel_stdout () = *)
  (*     match Unix.select [ stdout_out ] [] [] 0. with *)
  (*     | [ _a ], _, _ -> *)
  (*         let n = Unix.read stdout_out b 0 size in *)
  (*         Buffer.add_subbytes buffer b 0 n; *)
  (*         if n = size then read_toplevel_stdout () *)
  (*     | _ -> () *)
  (*   in *)
  (*   fun () -> *)
  (*     let () = *)
  (*       flush stdout; *)
  (*       read_toplevel_stdout () *)
  (*     in *)
  (*     let r = Buffer.contents buffer in *)
  (*     Buffer.reset buffer; *)
  (*     r *)

  let read_output () =
    let values =
      Str.replace_first ~!{|^#\( *\*\)* *|} "" @@ flush_fmt out_fmt
    in
    (* let stdout_str = read_stdout () in *)
    let stdout_str = "" in
    { values; stdout = stdout_str }

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

let format_input s =
  (* match mode with *)
  (* | Verbatim | Signature -> s *)
  (* | Toplevel -> ( *)
  (*     match String.split_on_char '\n' s with *)
  (*     | [] -> assert false *)
  (*     | a :: q -> String.concat ~sep:"\n  " ((toplevel_prompt ^ a) :: q)) *)
  match String.split_on_char '\n' s with
  | [] -> assert false
  | a :: q -> String.concat "\n  " ((toplevel_prompt ^ a) :: q)

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
        let formatted_phrase = format_input phrase in
        let ast = Toplevel.parse phrase in
        let _ = Toplevel.(exec out_fmt) ast in
        let out = Toplevel.read_output () in
        start tex_fmt phrase_env [];
        let _ = out.values in
        let _ = out.stdout in
        code_env input_env tex_fmt formatted_phrase;
        code_env ok_output tex_fmt out.values;
        stop tex_fmt phrase_env
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
