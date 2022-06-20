let x = ref 1 in
  begin
    x := !x + 1;
    x := !x * !x
  end;;
