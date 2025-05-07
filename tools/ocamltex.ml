let () =
  let buffer_fmt () =
    let b = Buffer.create 30 in
    (b, Format.formatter_of_buffer b)
  in
  let _, out_fmt = buffer_fmt () in
  let ast = Parse.toplevel_phrase (Lexing.from_string "true;;") in
  print_endline "hello";
  Toploop.initialize_toplevel_env ();
  Printf.printf "%b" @@ Toploop.execute_phrase true out_fmt ast
