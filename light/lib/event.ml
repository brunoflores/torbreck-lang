(* Handling of debugging events *)

open Lambda

let events = ref ([] : event list)
let reset () = events := []
