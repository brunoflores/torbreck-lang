{

open Parser

let reservedWords = [
  ("let", LET);
  ("in", IN);
  ("value", VALUE);
]

let (symbolTable : (string, token) Hashtbl.t) = Hashtbl.create 149
let _ = List.iter (fun (key, data) -> Hashtbl.add symbolTable key data) reservedWords

let string_buff = ref (Bytes.create 2048)
let string_end = ref 0

let resetstr () = string_end := 0

let addstr ch =
  let x = !string_end in
  let buffer = !string_buff in
  if x = Bytes.length buffer then begin
      let buff = Bytes.create (x * 2) in
      Bytes.blit buffer 0 buff 0 x;
      Bytes.set buff x ch;
      string_buff := buff;
      string_end := x + 1
  end else begin
      Bytes.set buffer x ch;
      string_end := x + 1
  end

let getstr () = Bytes.sub_string (!string_buff) 0 (!string_end)

}

let white = [' ' '\t']+
let line_break = [' ' '\009' '\012']*"\n"

rule read = parse
  | white
    { read lexbuf }

  | line_break
    { read lexbuf }

  | ['0'-'9']+
    { INT (int_of_string (Lexing.lexeme lexbuf)) }

  | ['A'-'Z' 'a'-'z' '_']
  | ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*
    { let s = Lexing.lexeme lexbuf in
        try
          Hashtbl.find symbolTable s
        with Not_found ->
          IDENT s }

  | "=" { EQUAL }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ";" { SEMI }
  | ";;" { SEMISEMI }
  | ":" { COLON }
  | "->" { MINUSGREATER }

  | "\""
    { resetstr(); string lexbuf }

  | eof
    { EOF }
  | _
    { raise (Failure "Lexer: Illegal character") }

and string = parse
  | '"'
    { Parser.STRING (getstr ()) }
  | '\\'
    { addstr (escaped lexbuf); string lexbuf }
  | '\n'
    { addstr '\n'; string lexbuf }
  | eof
    { raise (Failure "String not terminated") }
  | _
    { addstr (Lexing.lexeme_char lexbuf 0); string lexbuf }

and escaped = parse
  | 'n'
    { '\n' }
  | 't'
    { '\t' }
  | '\\'
    { '\\' }
  | '"'
    { '\034'  }
  | '\''
    { '\'' }
  | ['0'-'9']['0'-'9']['0'-'9']
    { let x = int_of_string(Lexing.lexeme lexbuf) in
      if x > 255 then
         raise (Failure "Illegal character constant")
      else
        Char.chr x }
  | [^ '"' '\\' 't' 'n' '\'']
    { raise (Failure "Illegal character constant") }
