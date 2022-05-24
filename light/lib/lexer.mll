{

open Parser

let reservedWords = [
  ("and", AND);
]

let (symbolTable : (string, token) Hashtbl.t) = Hashtbl.create 149
let _ = List.iter (fun (key, data) -> Hashtbl.add symbolTable key data) reservedWords

}

let white = [' ' '\t']+
let line_break = [' ' '\009' '\012']*"\n"

rule read = parse
  | white
    { read lexbuf }
