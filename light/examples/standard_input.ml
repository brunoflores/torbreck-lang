try
  while true do
    let c = input_char std_in in
    print_endline (string_of_char c)
  done

with End_of_file ->
  (* Flush the output channels *)
  exit 0
;;
