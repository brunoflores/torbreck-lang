{

open Parser

let reservedWords = [
  ("let", LET);
  ("in", IN);
]

let (symbolTable : (string, token) Hashtbl.t) = Hashtbl.create 149
let _ = List.iter (fun (key, data) -> Hashtbl.add symbolTable key data) reservedWords

}

let white = [' ' '\t']+
let line_break = [' ' '\009' '\012']*"\n"

rule read = parse
  | white
    { read lexbuf }
  | line_break
    { read lexbuf }
  | ['A'-'Z' 'a'-'z']
    ( '_' ? ['A'-'Z' 'a'-'z' '\'' '0'-'9' ] )
      { let s = Lexing.lexeme lexbuf in
          try
            Hashtbl.find symbolTable s
          with Not_found ->
            IDENT s }
