type identification = Name of string
                    | SS of int * int;;

let id1 = Name "Jean";;
let id2 = SS (1670728,280305);;

[id1; id2];;

type suit = Heart
          | Diamond
          | Club
          | Spade;;

Club;;

type card = Ace of suit
          | King of suit
          | Queen of suit
          | Jack of suit
          | Plain of suit * int;;
