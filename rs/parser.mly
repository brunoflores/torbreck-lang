%{

%}

%token EOF
%token FN
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token <string> IDENT

%start <unit> main

%%

main:
| EOF { () }
