(* let process_file file = () *)

module Toplevel = struct
  let ( ~! ) =
    let memo = ref [] in
    fun key ->
      try List.assq key !memo
      with Not_found ->
        let data = Str.regexp key in
        memo := (key, data) :: !memo;
        data

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
end

let () =
  let _ = Toploop.initialize_toplevel_env () in
  let ast = Toplevel.parse "1 + 2;;" in
  let _ = Toplevel.(exec out_fmt) ast in
  let out = Toplevel.read_output () in
  print_endline out
