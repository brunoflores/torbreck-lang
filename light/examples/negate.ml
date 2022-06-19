let negate = function true -> false
                    | false -> true
;;

let imply = function (true,false) -> false
                   | _ -> true
;;
