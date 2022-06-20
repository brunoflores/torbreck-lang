type person =
  {Name: string; mutable Age: int; mutable Job: string; mutable City: string};;

let jean = {Name="Jean"; Age=23; Job="Student"; City="Paris"};;

jean.Age <- jean.Age + 1;;

let get_older ({Age=n; _} as p) = p.Age <- n + 1;;
