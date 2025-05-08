let () =
  let flush_fmt (b, fmt) =
    Format.pp_print_flush fmt ();
    let r = Buffer.contents b in
    Buffer.reset b;
    r
  in
  let buffer_fmt () =
    let b = Buffer.create 30 in
    (b, Format.formatter_of_buffer b)
  in
  let b, out_fmt = buffer_fmt () in
  let ast = Parse.toplevel_phrase (Lexing.from_string "true;;") in
  let _ = Toploop.initialize_toplevel_env () in
  let _ = Toploop.execute_phrase true out_fmt ast in
  print_endline @@ flush_fmt (b, out_fmt)
