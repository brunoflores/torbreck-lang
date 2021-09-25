open Core

let command =
  Command.basic ~summary:"Type-check a program"
    Command.Let_syntax.(
      let%map_open _ = anon (maybe ("filename" %: Filename.arg_type)) in
      fun () -> print_endline "Hi.")

let () = Command.run command
