%{
open Ast
%}

%token EOF
%token FN
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token <string> IDENT

%start <stmt list> main

%%

main:
| ss = stmts { ss }
| EOF { [] }

stmts:
| s = stmt { [s] }
| s = stmt ss = stmts { s :: ss }

stmt:
| FN id = IDENT fs = formals LBRACE RBRACE
  { STMT (id, fs) }

formals:
| LPAREN RPAREN { [] }
