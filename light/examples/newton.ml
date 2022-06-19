let newton f epsilon =
  let rec until p change x =
    if p(x) then x
    else until p change (change(x)) in
  let satisfied y = abs(f y) <. epsilon in
  let improve y = y -. (f(y) /. (deriv f y epsilon))
in until satisfied improve
;;
