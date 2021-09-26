%{
    open Support.Error
    open Syntax

    let pos_of_lexing_position (pos : Lexing.position) : info =
      INFO { pos_fname = pos.pos_fname;
             pos_lnum = pos.pos_lnum;
             pos_bol = pos.pos_bol;
             pos_cnum = pos.pos_cnum }
%}

%token <string> STRING
%token SEMICOLON
%token EOF

%start <Syntax.term option> prog

%%

prog:
  | e = expr; SEMICOLON
    { Some e }

expr:
  | x = constant { x }

constant:
  | x = STRING { TmString ((pos_of_lexing_position $startpos), x) }
