{
  open Error

  let reservedWords = [
    (* Keywords *)
    ("as", fun i -> Parser.AS i);
    ("String", fun i -> Parser.USTRING i);
    ("if", fun i -> Parser.IF i);
    ("then", fun i -> Parser.THEN i);
    ("else", fun i -> Parser.ELSE i);
    ("true", fun i -> Parser.TRUE i);
    ("false", fun i -> Parser.FALSE i);
    ("Bool", fun i -> Parser.BOOL i);
    ("case", fun i -> Parser.CASE i);
    ("of", fun i -> Parser.OF i);
    ("ref", fun i -> Parser.REF i);
    ("Ref", fun i -> Parser.RREF i);
    ("unit", fun i -> Parser.UNIT i);
    ("Unit", fun i -> Parser.UUNIT i);
    ("timesfloat", fun i -> Parser.TIMESFLOAT i);
    ("Float", fun i -> Parser.UFLOAT i);
    ("let", fun i -> Parser.LET i);
    ("in", fun i -> Parser.IN i);
    ("inert", fun i -> Parser.INERT i);
    ("lambda", fun i -> Parser.LAMBDA i);
    ("fix", fun i -> Parser.FIX i);
    ("Rec", fun i -> Parser.REC i);
    ("letrec", fun i -> Parser.LETREC i);
    ("succ", fun i -> Parser.SUCC i);
    ("pred", fun i -> Parser.PRED i);
    ("iszero", fun i -> Parser.ISZERO i);
    ("Nat", fun i -> Parser.NAT i);
    ("error", fun i -> Parser.ERROR i);
    ("with", fun i -> Parser.OTHERWISE i);

    (* Symbols *)
    ("_", fun i -> Parser.USCORE i);
    ("|", fun i -> Parser.VBAR i);
    (".", fun i -> Parser.DOT i);
    (";", fun i -> Parser.SEMI i);
    (",", fun i -> Parser.COMMA i);
    (":", fun i -> Parser.COLON i);
    ("=", fun i -> Parser.EQ i);
    ("[", fun i -> Parser.LSQUARE i);
    ("<", fun i -> Parser.LT i);
    ("{", fun i -> Parser.LCURLY i);
    ("(", fun i -> Parser.LPAREN i);
    ("}", fun i -> Parser.RCURLY i);
    (")", fun i -> Parser.RPAREN i);
    ("]", fun i -> Parser.RSQUARE i);
    (">", fun i -> Parser.GT i);
    ("!", fun i -> Parser.BANG i);

    (* Special compound symbols *)
    ("->", fun i -> Parser.ARROW i);
    ("==>", fun i -> Parser.DDARROW i);
    (":=", fun i -> Parser.COLONEQ i);
  ]

  (* Support functions *)

  type buildfun = info -> Parser.token
  let (symbolTable : (string,buildfun) Hashtbl.t) = Hashtbl.create 1024
  let _ = List.iter (fun (str,f) -> Hashtbl.add symbolTable str f) reservedWords

  let createID i str =
    try (Hashtbl.find symbolTable str) i
    with _ ->
      if (String.get str 0) >= 'A' && (String.get str 0) <= 'Z' then
         Parser.UCID {i = i; v = str}
      else
         Parser.LCID {i = i; v = str}

  let lineno = ref 1
  and depth = ref 0
  and startLex = ref dummyinfo
  and start = ref 0

  let incr_line lexbuf = incr lineno; start := (Lexing.lexeme_start lexbuf)

  let info (buf : Lexing.lexbuf) : info =
    let p = buf.lex_start_p in
    createInfo p.pos_fname
               (!lineno)
               (Lexing.lexeme_start buf - !start)
               p.pos_cnum



  let text = Lexing.lexeme

  let stringBuffer = ref (Bytes.create 2048)
  let stringEnd = ref 0

  let resetStr () = stringEnd := 0

  let addStr ch =
    let x = !stringEnd in
    let buffer = !stringBuffer in
    if x = Bytes.length buffer then begin
        let newBuffer = Bytes.create (x * 2) in
        Bytes.blit buffer 0 newBuffer 0 x;
        Bytes.set newBuffer x ch;
        stringBuffer := newBuffer;
        stringEnd := x + 1
    end else begin
        Bytes.set buffer x ch;
        stringEnd := x + 1
    end

  let getStr () = Bytes.sub_string (!stringBuffer) 0 (!stringEnd)
}

let white = [' ' '\t']+
let line_break = [' ' '\009' '\012']*"\n"

rule read = parse
  | white
    { read lexbuf }
  | line_break
    { incr_line lexbuf; read lexbuf }

  | "*/"
    { error (info lexbuf) "Unmatched end of comment" }
  | "/*"
    { depth := 1; startLex := info lexbuf; eat_multi_line_comment lexbuf; read lexbuf }
  | "--" { eat_single_line_comment lexbuf }

  | ['0'-'9']+
    { Parser.INTV { i= info lexbuf; v = int_of_string (text lexbuf)} }
  | ['0'-'9']+ '.' ['0'-'9']+
    { Parser.FLOATV { i = info lexbuf; v = float_of_string (text lexbuf)} }

  | ['A'-'Z' 'a'-'z' '_']
  | ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*
    { createID (info lexbuf) (text lexbuf) }

  | ":=" | "<:" | "<-" | "->" | "=>" | "==>"
  | "{|" | "|}" | "<|" | "|>" | "[|" | "|]" | "=="
    { createID (info lexbuf) (text lexbuf) }

  | ['~' '%' '\\' '+' '-' '&' '|' ':' '@' '`' '$']+
    { createID (info lexbuf) (text lexbuf) }

  | ['*' '#' '/' '!' '?' '^' '(' ')' '{' '}' '[' ']' '<' '>' '.' ';' '_' ','
     '=' '\'']
    { createID (info lexbuf) (text lexbuf) }

  | "\""
    { resetStr(); startLex := info lexbuf; string lexbuf }
  | eof
    { Parser.EOF (info lexbuf) }
  | _
    { error (info lexbuf) "Illegal character" }

and eat_single_line_comment = parse
  | line_break
    { incr_line lexbuf; read lexbuf }
  | _
    { eat_single_line_comment lexbuf }

and eat_multi_line_comment = parse
  | "/*"
    { depth := succ !depth; eat_multi_line_comment lexbuf }
  | "*/"
    { depth := pred !depth; if !depth > 0 then eat_multi_line_comment lexbuf }
  | eof
    { error (!startLex) "Comment not terminated" }
  | [^ '\n']
    { eat_multi_line_comment lexbuf }
  | "\n"
    { incr_line lexbuf; eat_multi_line_comment lexbuf }

and string = parse
  | '"'
    { Parser.STRINGV {i = !startLex; v = getStr()} }
  | '\\'
    { addStr (escaped lexbuf); string lexbuf }
  | '\n'
    { addStr '\n'; incr_line lexbuf; string lexbuf }
  | eof
    { error (!startLex) "String not terminated" }
  | _
    { addStr (Lexing.lexeme_char lexbuf 0); string lexbuf }

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
    { let x = int_of_string(text lexbuf) in
      if x > 255 then
         error (info lexbuf) "Illegal character constant"
      else
        Char.chr x }
  | [^ '"' '\\' 't' 'n' '\'']
    { error (info lexbuf) "Illegal character constant" }
