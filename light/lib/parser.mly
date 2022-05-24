%token LET
%token IN
%token EQUAL
%token LPAREN
%token RPAREN
%token <string> IDENT
%token EOF

%start <unit> start

%%

start:
  | EOF
    { () }
