(* -*- prog -*- *)

{
open Parser
module H = Hashtbl

let keyword_table = H.create 100
let _ =
   List.iter (fun (kwd, tok) -> H.add keyword_table kwd tok)
             [
               ("fn", FN);
             ]
}

let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let ws = [ ' ' '\t' '\r' ]

rule main = parse
| ws+ { main lexbuf }

(* | '\n' { newline lexbuf } *)

| '{' { LBRACE }
| '}' { RBRACE }
| '(' { LPAREN }
| ')' { RPAREN }
| id as id
  {
    try (H.find keyword_table id) with
    | Not_found -> IDENT (id)
  }
| eof { EOF }
