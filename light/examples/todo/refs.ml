let r = ref (1+2);;
print_endline (string_of_int !r);;
r := !r+1;;
print_endline (string_of_int !r);;
