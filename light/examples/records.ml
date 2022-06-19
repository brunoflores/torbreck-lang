type person = {Name:string; Age:int; Job:string; City:string};;

let jean = {Job="Student"; City="Paris"; Name="Jean"; Age=23};;

let age_of = function {Age=n; Name=_; Job=_; City=_} -> n;;

jean.Age;;

type ('a,'b) pair = {Fst:'a; Snd:'b};;
let first x = x.Fst and second x = x.Snd;;
let p = {Snd=true; Fst=1+2};;
first p;;
second p;;
