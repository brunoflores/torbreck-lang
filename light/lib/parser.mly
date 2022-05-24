%token AND
%token EOF

%start <unit> start

%%

start:
  | EOF
    { () }
