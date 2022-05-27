open Location

let displacement_overflow () =
  Printf.eprintf "%tPhrase too large, a relative displacement overflowed.\n"
    output_input_name;
  raise @@ Failure "displacement_overflow"
