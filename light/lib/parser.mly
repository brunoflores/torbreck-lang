%token LET
%token IN
%token <string> IDENT
%token EOF

%start <unit> start

%%

start:
  | EOF
    { () }
