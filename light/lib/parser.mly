%{
open Const
open Paraux
open Syntax
%}

/* Identifiers, prefixes and infixes */
%token <string> IDENT

/* Literals */
%token <int> INT

/* Keywords */
%token LET
%token IN

/* Special symbols */
%token EQUAL
%token LPAREN
%token RPAREN
%token SEMI
%token SEMISEMI

/* The end-of-file marker */
%token EOF

%start <Syntax.impl_phrase> implementation

%%

implementation:
  | e = expr SEMISEMI
    { make_impl (Zexpr e) }
  | EOF
    { raise End_of_file }

expr:
  | s = simple_expr
    { s }

simple_expr:
  | s = struct_constant
    { make_expr (Zconstant s) }

struct_constant:
  | a = atomic_constant
    { SCatom a }

atomic_constant:
  | i = INT
    { ACint i }
