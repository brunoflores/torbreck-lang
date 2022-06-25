let chars = ref 0;;
let lines = ref 0;;

try
  while true do
    let c = input_char std_in in
      chars := !chars + 1;
      if c = '\n' then lines := !lines + 1 else ()
  done

with End_of_file ->
  print_int !chars; print_string " characters, ";
  print_int !lines; print_string " lines.\n";
  (* Flush the output channels *)
  exit 0
;;
