type in_channel
;;

let std_in = open_descriptor_in 0
;;

let print_int i = print_string (int__string_of_int i)
;;

let exit n =
  (* TODO: flush stdout and stderr *)
  sys__exit n
;;
