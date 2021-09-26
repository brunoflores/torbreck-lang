{
  open Parser

  exception Error of string
}

let zeroes = '0'+
let white = [' ' '\t']+

rule read = parse
  | white
    { read lexbuf }
  | '"'
    { read_string (Buffer.create 17) lexbuf }
  | ';'
    { SEMICOLON }
  | eof
    { EOF }
  | _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | [^ '"']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf }
