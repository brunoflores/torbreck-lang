(* The lexer definition *)

{

open Lexing
open Parser

type error =
  | Illegal_character of char

exception Error of error * Location.t

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal

rule token = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        EOL }
  | blank +
      { token lexbuf }
  | int_literal as lit { INT lit }
  | eof { EOF }
