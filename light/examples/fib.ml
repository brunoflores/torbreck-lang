let rec fib n =
  if n < 2 then n
  else fib (n-1) + fib (n-2)
in print_endline (string_of_int (fib 1));;
